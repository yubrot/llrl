use crate::ast;
use crate::formatting::ContextualDisplay;
use derive_new::new;
use if_chain::if_chain;
use std::collections::BTreeSet;
use std::fmt;
use std::rc::Rc;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash)]
pub enum Pattern {
    Constructor(Tag, Vec<Pattern>),
    Wildcard,
}

impl Pattern {
    pub fn is_wildcard(&self) -> bool {
        match self {
            Self::Constructor(_, _) => false,
            Self::Wildcard => true,
        }
    }

    pub fn is_incompatible_with(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Constructor(a_tag, a_ps), Self::Constructor(b_tag, b_ps)) => {
                a_tag != b_tag
                    || a_ps
                        .iter()
                        .zip(b_ps)
                        .any(|(a, b)| a.is_incompatible_with(b))
            }
            _ => false,
        }
    }

    pub fn collect_head_tags<'a>(&'a self, set: &mut BTreeSet<&'a Tag>) {
        match self {
            Self::Constructor(tag, _) => {
                set.insert(tag);
            }
            Self::Wildcard => {}
        }
    }

    pub fn matches_tuple(&self) -> Option<&[Pattern]> {
        if_chain! {
            if let Self::Constructor(tag, ps) = self;
            if let Some(n) = tag.matches_tuple();
            if ps.len() == n;
            then { Some(ps.as_slice()) }
            else { None }
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash)]
pub enum Tag {
    Const(ast::Const),
    Finite(FiniteCon, Rc<[FiniteCon]>),
}

impl Tag {
    pub fn arity(&self) -> usize {
        match self {
            Self::Const(_) => 0,
            Self::Finite(con, _) => con.arity,
        }
    }

    pub fn sibling_count(&self) -> Option<usize> {
        match self {
            Self::Const(_) => None,
            Self::Finite(_, cons) => Some(cons.len()),
        }
    }

    pub fn siblings(&self) -> Option<Vec<Tag>> {
        match self {
            Self::Const(_) => None,
            Self::Finite(_, cons) => Some(FiniteCon::tags(cons)),
        }
    }

    pub fn is_complete(tags: &BTreeSet<&Tag>) -> bool {
        match tags.iter().next() {
            Some(tag) => tag.sibling_count() == Some(tags.len()),
            None => false,
        }
    }

    pub fn matches_tuple(&self) -> Option<usize> {
        match self {
            Self::Finite(con, _) => con.use_.matches_tuple(),
            _ => None,
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash, new)]
pub struct FiniteCon {
    pub use_: ast::ValueCon,
    pub arity: usize,
}

impl FiniteCon {
    pub fn tags(cons: &Rc<[FiniteCon]>) -> Vec<Tag> {
        cons.iter()
            .map(move |con| Tag::Finite(*con, Rc::clone(cons)))
            .collect()
    }
}

impl From<&'_ ast::DataValueCon> for FiniteCon {
    fn from(con: &'_ ast::DataValueCon) -> Self {
        let arity = con.fields.as_ref().map_or(0, |fields| fields.len());
        Self::new(con.id.into(), arity)
    }
}

impl From<&'_ ast::BuiltinValueCon> for FiniteCon {
    fn from(con: &'_ ast::BuiltinValueCon) -> Self {
        let arity = con.fields.as_ref().map_or(0, |fields| fields.len());
        Self::new(con.id.into(), arity)
    }
}

impl<F: ast::PatternFormatter> ContextualDisplay<F> for Pattern {
    fn fmt_with(&self, ctx: &F, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(ps) = self.matches_tuple() {
            write!(f, "(:")?;
            for p in ps {
                write!(f, " {}", p.fmt_on(ctx))?;
            }
            return write!(f, ")");
        }

        match self {
            Self::Constructor(tag, _) if tag.arity() == 0 => {
                write!(f, "{}", tag.fmt_on(ctx))
            }
            Self::Constructor(tag, ps) => {
                write!(f, "({}", tag.fmt_on(ctx))?;
                for p in ps {
                    write!(f, " {}", p.fmt_on(ctx))?;
                }
                write!(f, ")")
            }
            Self::Wildcard => write!(f, "_"),
        }
    }
}

impl<F: ast::PatternFormatter> ContextualDisplay<F> for Tag {
    fn fmt_with(&self, ctx: &F, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Const(l) => write!(f, "{}", l),
            Self::Finite(con, _) => write!(f, "{}", con.fmt_on(ctx)),
        }
    }
}

impl<F: ast::PatternFormatter> ContextualDisplay<F> for FiniteCon {
    fn fmt_with(&self, ctx: &F, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", ast::Use::Resolved(self.use_, None).fmt_on(ctx))
    }
}
