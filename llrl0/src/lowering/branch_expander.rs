use super::ir::*;
use derive_new::new;
use std::borrow::Cow;
use std::collections::{HashMap, VecDeque};
use std::rc::Rc;

pub trait Env {
    fn alloc_rt(&mut self) -> RtId;
}

pub fn expand(src: &mut impl rewriter::Rewrite, env: &mut impl Env) {
    let _ = rewriter::rewrite(src, &mut MatchExpander::new(env));
}

#[derive(Debug, new)]
struct MatchExpander<'e, E> {
    env: &'e mut E,
}

impl<'e, E: Env> rewriter::Rewriter for MatchExpander<'e, E> {
    type Error = ();

    fn after_rt(&mut self, rt: &mut Rt) -> Result<(), Self::Error> {
        match rt {
            Rt::And(and) => {
                let a = std::mem::take(&mut and.0);
                let b = std::mem::take(&mut and.1);
                *rt = Rt::if_(a, b, Rt::Const(Const::bool(false)))
            }
            Rt::Or(or) => {
                let a = std::mem::take(&mut or.0);
                let b = std::mem::take(&mut or.1);
                *rt = Rt::if_(a, Rt::Const(Const::bool(true)), b)
            }
            Rt::Match(match_) => {
                let target = std::mem::take(&mut match_.0);
                let clauses = std::mem::take(&mut match_.1);
                *rt = self.expand(target, clauses);
            }
            _ => {}
        }
        Ok(())
    }
}

impl<'e, E: Env> MatchExpander<'e, E> {
    fn expand(&mut self, target: Rt, clauses: Vec<RtClause>) -> Rt {
        if let Some(ty) = target.ty() {
            let ty = ty.into_owned();
            let mut conts = Vec::with_capacity(clauses.len());
            let mut matches = VecDeque::with_capacity(clauses.len());

            for clause in clauses {
                let pat = FlattenPat::from(clause.pat);
                let id = self.env.alloc_rt();
                let ret = clause.body.ty().map(|ty| ty.into_owned());
                conts.push(RtCont::new(id, pat.binds, clause.body));
                matches.push_back((pat.parts, id, ret));
            }

            Rt::let_cont(conts, {
                let root = self.env.alloc_rt();
                Rt::let_var(vec![RtVar::new(root, ty.clone(), target)], {
                    let progress = Box::new(MatchProgress::new(root, ty));
                    self.expand_matches(progress, matches)
                })
            })
        } else {
            target
        }
    }

    fn expand_matches(
        &mut self,
        progress: Box<MatchProgress>,
        mut matches: VecDeque<(VecDeque<PatPart>, RtId, Option<Ct>)>,
    ) -> Rt {
        if let Some((parts, id, ret)) = matches.pop_front() {
            let cont = MatchCont::<Self>::new(
                move |_, progress| Rt::cont_call(id, progress.bind_args, ret),
                move |self_, mut progress| {
                    progress.bind_args.clear();
                    self_.expand_matches(progress, matches.clone())
                },
            );
            self.expand_pat_parts(progress, parts, cont)
        } else {
            Rt::Never
        }
    }

    fn expand_pat_parts(
        &mut self,
        progress: Box<MatchProgress>,
        mut parts: VecDeque<PatPart>,
        cont: MatchCont<Self>,
    ) -> Rt {
        if let Some(part) = parts.pop_front() {
            let cont = cont.extend_success(move |self_, progress, cont| {
                self_.expand_pat_parts(progress, parts, cont)
            });
            self.expand_pat_part(progress, part, cont)
        } else {
            (cont.success)(self, progress)
        }
    }

    fn expand_pat_part(
        &mut self,
        mut progress: Box<MatchProgress>,
        part: PatPart,
        cont: MatchCont<Self>,
    ) -> Rt {
        match progress.part(&part) {
            MatchPartProgress::Satisfied(_) => (cont.success)(self, progress),
            MatchPartProgress::Unsatisfied => (cont.failure)(self, progress),
            MatchPartProgress::NotKnown => match progress.part(part.parent()) {
                MatchPartProgress::Satisfied(Some(parent)) => match part.to_test_expr(parent) {
                    Some(test_expr) => {
                        let success = {
                            let mut progress = progress.clone();
                            if let Some(bind_expr) = part.to_bind_expr(parent) {
                                progress.bind_args.push(bind_expr);
                            }
                            progress.mark_as_passed(part.clone());
                            (cont.success)(self, progress)
                        };
                        let failure = {
                            progress.mark_as_failed(part);
                            (cont.failure)(self, progress)
                        };
                        Rt::if_(test_expr, success, failure)
                    }
                    None => {
                        if let Some(expr) = part.to_bind_expr(parent) {
                            progress.bind_args.push(expr);
                        }
                        progress.mark_as_passed(part);
                        (cont.success)(self, progress)
                    }
                },
                MatchPartProgress::Satisfied(None) => {
                    // part.parent() is satisfied but is not obtained, put it to a variable
                    let parent = part.parent().clone();
                    let grandparent = progress.part(parent.parent()).var().unwrap();
                    let parent_var = self.env.alloc_rt();
                    let parent_ty = parent.ty().into_owned();
                    let get_parent_expr = parent.to_get_expr(grandparent).unwrap();
                    Rt::let_var(vec![RtVar::new(parent_var, parent_ty, get_parent_expr)], {
                        progress.set_passed_var(parent, parent_var);
                        self.expand_pat_part(progress, part, cont)
                    })
                }
                MatchPartProgress::Unsatisfied => (cont.failure)(self, progress),
                MatchPartProgress::NotKnown => {
                    // We first need to test that part.parent() is satisfied
                    let parent = part.parent().clone();
                    let cont = cont.extend_success(move |self_, progress, cont| {
                        self_.expand_pat_part(progress, part, cont)
                    });
                    self.expand_pat_part(progress, parent, cont)
                }
            },
        }
    }
}

struct MatchCont<'e, E> {
    success: Box<dyn FnOnce(&mut E, Box<MatchProgress>) -> Rt + 'e>,
    failure: Rc<dyn Fn(&mut E, Box<MatchProgress>) -> Rt + 'e>,
}

impl<'e, E: 'e> MatchCont<'e, E> {
    fn new(
        success: impl FnOnce(&mut E, Box<MatchProgress>) -> Rt + 'e,
        fail: impl Fn(&mut E, Box<MatchProgress>) -> Rt + 'e,
    ) -> Self {
        Self {
            success: Box::new(success),
            failure: Rc::new(fail),
        }
    }

    fn extend_success(
        self,
        success: impl FnOnce(&mut E, Box<MatchProgress>, Self) -> Rt + 'e,
    ) -> Self {
        let failure = Rc::clone(&self.failure);
        Self {
            success: Box::new(move |env, progress| success(env, progress, self)),
            failure,
        }
    }
}

// TODO: Use exhaustiveness information to reduce test
#[derive(Debug, Clone)]
struct MatchProgress {
    determined: HashMap<PatPart, Result<Option<RtId>, ()>>,
    determined_range: HashMap<PatPart, Vec<PatPart>>,
    bind_args: Vec<Rt>,
}

impl MatchProgress {
    fn new(root: RtId, ty: Ct) -> Self {
        Self {
            determined: vec![(PatPart::Root(ty), Ok(Some(root)))]
                .into_iter()
                .collect(),
            determined_range: HashMap::new(),
            bind_args: Vec::new(),
        }
    }

    fn part(&self, part: &PatPart) -> MatchPartProgress {
        match self.determined.get(part) {
            Some(Ok(var)) => MatchPartProgress::Satisfied(*var),
            Some(Err(_)) => MatchPartProgress::Unsatisfied,
            None => match self.determined_range.get(part.parent()) {
                Some(ps) if ps.iter().any(|p| p.excludes(part)) => MatchPartProgress::Unsatisfied,
                _ => MatchPartProgress::NotKnown,
            },
        }
    }

    fn mark_as_passed(&mut self, part: PatPart) {
        self.determined_range
            .entry(part.parent().clone())
            .or_default()
            .push(part.clone());
        assert_eq!(self.determined.insert(part, Ok(None)), None);
    }

    fn mark_as_failed(&mut self, part: PatPart) {
        assert_eq!(self.determined.insert(part, Err(())), None);
    }

    fn set_passed_var(&mut self, part: PatPart, var: RtId) {
        assert_eq!(self.determined.insert(part, Ok(Some(var))), Some(Ok(None)));
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
enum MatchPartProgress {
    NotKnown,
    Unsatisfied,
    Satisfied(Option<RtId>),
}

impl MatchPartProgress {
    fn var(self) -> Option<RtId> {
        match self {
            Self::NotKnown | Self::Unsatisfied => panic!("MatchPartProgress::var on {:?}", self),
            Self::Satisfied(var) => var,
        }
    }
}

/// A representation of a pattern that is expanded as a set of PatParts.
#[derive(PartialEq, Eq, Debug, Clone)]
struct FlattenPat {
    parts: VecDeque<PatPart>,
    binds: Vec<RtParam>,
}

impl FlattenPat {
    #[allow(clippy::new_without_default)]
    fn new() -> Self {
        Self {
            parts: VecDeque::new(),
            binds: Vec::new(),
        }
    }

    /// Collect the necessary `PatPart`s from `RtPat`.
    /// Returns true if any pattern is added as a PatPart.
    fn collect(&mut self, pat: RtPat, parent: &PatPart) -> bool {
        match pat {
            RtPat::Var(id, ty, as_pat) => {
                self.parts.push_back(parent.bind(id));
                self.binds.push(RtParam::new(id, ty));
                if let Some(pat) = as_pat {
                    self.collect(*pat, parent);
                }
                true
            }
            RtPat::Wildcard(_) => false,
            RtPat::Deref(elem_pat) => {
                // Deref is unchecked
                self.collect(*elem_pat, &parent.deref_elem())
            }
            RtPat::NonNull(elem_pat) => {
                let elem = parent.non_null_elem();
                if !self.collect(*elem_pat, &elem) {
                    // Non-null check is required even if the element is unused
                    self.parts.push_back(elem);
                }
                true
            }
            RtPat::Null(_) => {
                self.parts.push_back(parent.null());
                true
            }
            RtPat::Data(_, _, _) => panic!("Found RtPat::Data at branch_expander"),
            RtPat::Struct(_, args) => {
                #[allow(clippy::unnecessary_fold)] // to force side-effects
                args.into_iter()
                    .enumerate()
                    .map(|(i, arg)| {
                        let elem = parent.struct_elem(arg.ty().into_owned(), i);
                        self.collect(arg, &elem)
                    })
                    .fold(false, |a, b| a || b)
            }
            RtPat::Reinterpret(_from_ty, pat) => {
                let to_ty = pat.ty().into_owned();
                self.collect(*pat, &parent.reinterpret(to_ty))
            }
            RtPat::Syntax(pat) => self.collect(*pat, &parent.syntax_body()),
            RtPat::Const(c) => {
                self.parts.push_back(parent.const_(c));
                true
            }
        }
    }
}

impl From<RtPat> for FlattenPat {
    fn from(pat: RtPat) -> Self {
        let mut p = Self::new();
        let ty = pat.ty().into_owned();
        p.collect(pat, &PatPart::Root(ty));
        p
    }
}

/// A representation of a certain part of a pattern.
/// PatPart is represented by a recursive structure from a leaf of the pattern tree to the root.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash)]
enum PatPart {
    Root(Ct),
    Bind(RtId, Box<PatPart>), // a marker PatPart indicating variable binding
    DerefElem(Box<PatPart>),
    NonNullElem(Box<PatPart>),
    Null(Box<PatPart>),
    StructElem(Ct, usize, Box<PatPart>),
    Reinterpret(Ct, Box<PatPart>),
    SyntaxBody(Box<PatPart>),
    Const(Const, Box<PatPart>),
}

impl PatPart {
    fn bind(&self, var: RtId) -> Self {
        Self::Bind(var, Box::new(self.clone()))
    }

    fn deref_elem(&self) -> Self {
        Self::DerefElem(Box::new(self.clone()))
    }

    fn non_null_elem(&self) -> Self {
        Self::NonNullElem(Box::new(self.clone()))
    }

    fn null(&self) -> Self {
        Self::Null(Box::new(self.clone()))
    }

    fn struct_elem(&self, elem_ty: Ct, index: usize) -> Self {
        Self::StructElem(elem_ty, index, Box::new(self.clone()))
    }

    fn reinterpret(&self, ty: Ct) -> Self {
        Self::Reinterpret(ty, Box::new(self.clone()))
    }

    fn syntax_body(&self) -> Self {
        Self::SyntaxBody(Box::new(self.clone()))
    }

    fn const_(&self, c: Const) -> Self {
        Self::Const(c, Box::new(self.clone()))
    }

    fn parent(&self) -> &Self {
        match self {
            PatPart::Root(_) => self,
            PatPart::Bind(_, p)
            | PatPart::DerefElem(p)
            | PatPart::NonNullElem(p)
            | PatPart::Null(p)
            | PatPart::StructElem(_, _, p)
            | PatPart::Reinterpret(_, p)
            | PatPart::SyntaxBody(p)
            | PatPart::Const(_, p) => p,
        }
    }

    fn ty(&self) -> Cow<Ct> {
        match self {
            PatPart::Root(ty) | PatPart::StructElem(ty, _, _) | PatPart::Reinterpret(ty, _) => {
                Cow::Borrowed(ty)
            }
            PatPart::DerefElem(p) | PatPart::NonNullElem(p) => Ct::ptr_elem(p.ty()),
            PatPart::Bind(_, p) | PatPart::Null(p) | PatPart::Const(_, p) => p.ty(),
            PatPart::SyntaxBody(p) => Ct::syntax_body(p.ty()),
        }
    }

    fn excludes(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::NonNullElem(a), Self::Null(b)) | (Self::Null(a), Self::NonNullElem(b)) => a == b,
            (Self::Const(ca, a), Self::Const(cb, b)) => ca != cb && a == b,
            _ => false,
        }
    }

    fn to_bind_expr(&self, parent: RtId) -> Option<Rt> {
        match self {
            Self::Bind(_, p) => Some(Rt::Var(parent, p.ty().into_owned())),
            _ => None,
        }
    }

    fn to_test_expr(&self, parent: RtId) -> Option<Rt> {
        match self {
            Self::NonNullElem(p) => {
                let elem_ty = Ct::ptr_elem(p.ty()).into_owned();
                Some(Rt::unary(
                    Unary::Not,
                    Rt::binary(
                        Binary::PtrEq,
                        Rt::Var(parent, Ct::ptr(elem_ty.clone())),
                        Rt::nullary(Nullary::Null(elem_ty)),
                    ),
                ))
            }
            Self::Null(p) => {
                let elem_ty = Ct::ptr_elem(p.ty()).into_owned();
                Some(Rt::binary(
                    Binary::PtrEq,
                    Rt::Var(parent, Ct::ptr(elem_ty.clone())),
                    Rt::nullary(Nullary::Null(elem_ty)),
                ))
            }
            Self::Const(c, _) => Some(Rt::binary(
                match c {
                    Const::Integer(_, _, _) => Binary::IEq,
                    Const::FPNumber(_, _) => Binary::FEq,
                    Const::String(_) => Binary::StringEq,
                    Const::Char(_) => Binary::IEq,
                    Const::SyntaxSexp(_, _) => panic!("Found Const::SyntaxSexp in Pattern"),
                    Const::Unit => None?,
                },
                Rt::Var(parent, c.ty().into_owned()),
                Rt::Const(c.clone()),
            )),

            _ => None,
        }
    }

    fn to_get_expr(&self, parent: RtId) -> Option<Rt> {
        match self {
            Self::DerefElem(p) | Self::NonNullElem(p) => {
                let ty = p.ty().into_owned();
                Some(Rt::unary(Unary::Load, Rt::Var(parent, ty)))
            }
            Self::StructElem(elem_ty, index, p) => {
                let struct_ty = p.ty().into_owned();
                Some(Rt::unary(
                    Unary::StructElem(elem_ty.clone(), *index),
                    Rt::Var(parent, struct_ty),
                ))
            }
            Self::Reinterpret(to, p) => {
                let from = p.ty().into_owned();
                Some(Rt::unary(
                    Unary::Reinterpret(to.clone()),
                    Rt::Var(parent, from),
                ))
            }
            Self::SyntaxBody(p) => {
                let ty = p.ty().into_owned();
                Some(Rt::unary(Unary::SyntaxBody, Rt::Var(parent, ty)))
            }
            _ => None,
        }
    }
}
