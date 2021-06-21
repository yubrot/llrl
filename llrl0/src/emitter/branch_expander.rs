use super::{ir::*, rewriter};
use derive_new::new;
use smallvec::SmallVec;
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
        let mut conts = Vec::with_capacity(clauses.len());
        let mut matrix = VecDeque::with_capacity(clauses.len());

        for clause in clauses {
            let (row, params) = FlattenPat::collect(clause.pat);
            let id = self.env.alloc_rt();
            conts.push(RtCont::new(id, params, clause.body));
            matrix.push_back((row, id));
        }

        Rt::let_cont(conts, {
            let root = self.env.alloc_rt();
            Rt::let_var(
                vec![RtVar::new(root, Ct::Hole, target)],
                self.expand_matrix(Box::new(MatchProgress::new(root)), matrix),
            )
        })
    }

    fn expand_matrix(
        &mut self,
        progress: Box<MatchProgress>,
        mut matrix: VecDeque<(VecDeque<FlattenPat>, RtId)>,
    ) -> Rt {
        if let Some((row, id)) = matrix.pop_front() {
            let cont = MatchCont::<Self>::new(
                move |_, progress| Rt::Cont(id, progress.args),
                move |self_, mut progress| {
                    progress.reset_args();
                    self_.expand_matrix(progress, matrix.clone())
                },
            );
            self.expand_row(progress, row, cont)
        } else {
            Rt::Never
        }
    }

    fn expand_row(
        &mut self,
        progress: Box<MatchProgress>,
        mut row: VecDeque<FlattenPat>,
        cont: MatchCont<Self>,
    ) -> Rt {
        if let Some(pat) = row.pop_front() {
            let cont = cont
                .extend_success(move |self_, progress, cont| self_.expand_row(progress, row, cont));
            self.expand_pat(progress, pat, cont)
        } else {
            (cont.success)(self, progress)
        }
    }

    fn expand_pat(
        &mut self,
        mut progress: Box<MatchProgress>,
        pat: FlattenPat,
        cont: MatchCont<Self>,
    ) -> Rt {
        if let Some(result) = progress.path_progressess.get_mut(&pat.path) {
            match result.is_satisfied(&pat.cond) {
                Some(true) => (cont.success)(self, progress),
                Some(false) => (cont.failure)(self, progress),
                None => match pat.cond.get_test(result.tmp_var) {
                    None => {
                        match pat.cond {
                            Condition::Bind(_) => progress.args.push(result.to_cont_arg()),
                            _ => result.mark_as_satisfied(pat.cond),
                        }
                        (cont.success)(self, progress)
                    }
                    Some(e) => {
                        let success = {
                            let mut progress = progress.clone();
                            progress
                                .path_progressess
                                .get_mut(&pat.path)
                                .unwrap()
                                .mark_as_satisfied(pat.cond.clone());
                            (cont.success)(self, progress)
                        };
                        let failure = {
                            progress
                                .path_progressess
                                .get_mut(&pat.path)
                                .unwrap()
                                .mark_as_unsatisfied(pat.cond);
                            (cont.failure)(self, progress)
                        };
                        Rt::if_(e, success, failure)
                    }
                },
            }
        } else {
            let id = self.env.alloc_rt();
            let parent = pat.path.clone().parent();
            let index = pat.path.last_index();
            let tmp = progress.path_progressess[&parent].get_child(index);
            Rt::let_var(vec![RtVar::new(id, Ct::Hole, tmp)], {
                progress
                    .path_progressess
                    .insert(pat.path.clone(), PathProgress::new(id));
                self.expand_pat(progress, pat, cont)
            })
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

#[derive(Debug, Clone)]
struct MatchProgress {
    path_progressess: HashMap<Path, PathProgress>,
    args: Vec<Rt>,
}

impl MatchProgress {
    fn new(root: RtId) -> Self {
        Self {
            path_progressess: vec![(Path::root(), PathProgress::new(root))]
                .into_iter()
                .collect(),
            args: Vec::new(),
        }
    }

    fn reset_args(&mut self) {
        self.args.clear();
    }
}

// TODO: Use exhaustiveness information to reduce Condition test
#[derive(Debug, Clone, new)]
struct PathProgress {
    tmp_var: RtId,
    #[new(default)]
    satisfied: SmallVec<[Condition; 1]>,
    #[new(default)]
    unsatisfied: SmallVec<[Condition; 1]>,
}

impl PathProgress {
    fn to_cont_arg(&self) -> Rt {
        Rt::Local(self.tmp_var)
    }

    fn mark_as_satisfied(&mut self, cond: Condition) {
        self.satisfied.push(cond);
    }

    fn mark_as_unsatisfied(&mut self, cond: Condition) {
        self.unsatisfied.push(cond);
    }

    fn is_satisfied(&self, cond: &Condition) -> Option<bool> {
        for satisfied_cond in self.satisfied.iter() {
            if satisfied_cond == cond {
                return Some(true);
            }
            if satisfied_cond.excludes(cond) {
                return Some(false);
            }
        }
        for unsatisfied_cond in self.unsatisfied.iter() {
            if unsatisfied_cond == cond {
                return Some(false);
            }
        }
        None
    }

    fn get_child(&self, index: usize) -> Rt {
        self.satisfied
            .iter()
            .find_map(|c| c.get_child(self.tmp_var, index))
            .expect("Cannot get child from satisfied conditions")
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash, new)]
struct FlattenPat {
    path: Path,
    cond: Condition,
}

impl FlattenPat {
    fn collect(pat: RtPat) -> (VecDeque<FlattenPat>, Vec<FunctionParam>) {
        fn visit(
            pat: RtPat,
            path: Path,
            pats: &mut VecDeque<FlattenPat>,
            params: &mut Vec<FunctionParam>,
        ) {
            match pat {
                RtPat::Var(id, ty, as_pat) => {
                    pats.push_back(FlattenPat::new(path.clone(), Condition::Bind(id)));
                    params.push(FunctionParam::new(id, ty));
                    if let Some(pat) = as_pat {
                        visit(*pat, path, pats, params);
                    }
                }
                RtPat::Wildcard => {}
                RtPat::Deref(pat) => {
                    pats.push_back(FlattenPat::new(path.clone(), Condition::Deref));
                    visit(*pat, path.child(0), pats, params);
                }
                RtPat::NonNull(ty, pat) => {
                    pats.push_back(FlattenPat::new(path.clone(), Condition::NonNull(ty)));
                    visit(*pat, path.child(0), pats, params);
                }
                RtPat::Null(ty) => {
                    pats.push_back(FlattenPat::new(path.clone(), Condition::Null(ty)));
                }
                RtPat::Data(_, _, _) => panic!("Found RtPat::Data at branch_expander"),
                RtPat::Struct(ty, args) => {
                    pats.push_back(FlattenPat::new(path.clone(), Condition::Struct(ty)));
                    for (i, arg) in args.into_iter().enumerate() {
                        visit(arg, path.clone().child(i), pats, params);
                    }
                }
                RtPat::Reinterpret(from, to, pat) => {
                    pats.push_back(FlattenPat::new(
                        path.clone(),
                        Condition::Reinterpret(from, to),
                    ));
                    visit(*pat, path.child(0), pats, params);
                }
                RtPat::Syntax(ty, pat) => {
                    pats.push_back(FlattenPat::new(path.clone(), Condition::Syntax(ty)));
                    visit(*pat, path.child(0), pats, params);
                }
                RtPat::Const(c) => {
                    pats.push_back(FlattenPat::new(path, Condition::Const(c)));
                }
            }
        }

        let mut pats = VecDeque::new();
        let mut params = Vec::new();
        visit(pat, Path::root(), &mut pats, &mut params);
        (pats, params)
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash)]
enum Condition {
    Bind(RtId),
    Deref,
    NonNull(Ct),
    Null(Ct),
    Struct(Ct),
    Reinterpret(Ct, Ct),
    Syntax(Ct),
    Const(Const),
}

impl Condition {
    fn excludes(&self, other: &Condition) -> bool {
        !matches!(self, Self::Bind(_)) && !matches!(other, Self::Bind(_)) && self != other
    }

    fn get_test(&self, var: RtId) -> Option<Rt> {
        match self {
            Condition::Bind(_) => None,
            Condition::Deref => None,
            Condition::NonNull(ty) => Some(Rt::unary(
                Unary::Not,
                Rt::binary(
                    Binary::PtrEq,
                    Rt::Local(var),
                    Rt::nullary(Nullary::Null(ty.clone())),
                ),
            )),
            Condition::Null(ty) => Some(Rt::binary(
                Binary::PtrEq,
                Rt::Local(var),
                Rt::nullary(Nullary::Null(ty.clone())),
            )),
            Condition::Struct(_) => None,
            Condition::Reinterpret(_, _) => None,
            Condition::Syntax(_) => None,
            Condition::Const(const_) => Some(Rt::binary(
                match const_ {
                    Const::Integer(_, _, _) => Binary::IEq,
                    Const::FPNumber(_, _) => Binary::FEq,
                    Const::String(_) => Binary::StringEq,
                    Const::Char(_) => Binary::CharEq,
                    Const::SyntaxSexp(_, _) => panic!("Found Const::SyntaxSexp in Pattern"),
                    Const::Unit => None?,
                },
                Rt::Local(var),
                Rt::Const(const_.clone()),
            )),
        }
    }

    fn get_child(&self, condition_satisfied_var: RtId, index: usize) -> Option<Rt> {
        let v = Rt::Local(condition_satisfied_var);
        match self {
            Self::Deref | Self::NonNull(_) => {
                assert_eq!(index, 0);
                Some(Rt::unary(Unary::Load, v))
            }
            Self::Struct(ty) => Some(Rt::unary(Unary::StructElem(ty.clone(), index), v)),
            Self::Reinterpret(from, to) => {
                assert_eq!(index, 0);
                Some(Rt::unary(Unary::Reinterpret(from.clone(), to.clone()), v))
            }
            Self::Syntax(ty) => {
                assert_eq!(index, 0);
                Some(Rt::unary(Unary::SyntaxBody(ty.clone()), v))
            }
            _ => None,
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash)]
struct Path(Vec<usize>);

impl Path {
    fn root() -> Self {
        Self(Vec::new())
    }

    fn last_index(&self) -> usize {
        self.0.last().copied().unwrap_or_default()
    }

    fn parent(mut self) -> Self {
        self.0.pop();
        self
    }

    fn child(mut self, index: usize) -> Self {
        self.0.push(index);
        self
    }
}
