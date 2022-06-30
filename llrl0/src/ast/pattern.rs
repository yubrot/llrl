use super::*;
use crate::formatting::ContextualDisplay;
use std::fmt;

#[derive(Debug, Clone, new)]
pub struct Pattern {
    pub id: NodeId<Pattern>,
    pub rep: PatternRep,
}

impl Dfs for Pattern {
    fn dfs_<E>(&self, f: &mut impl FnMut(&Self) -> Result<(), E>) -> Result<(), E> {
        match self.rep {
            PatternRep::Var(ref var) => {
                if let Some(ref as_pat) = var.as_pat {
                    as_pat.dfs_(f)?;
                }
            }
            PatternRep::Wildcard => {}
            PatternRep::Decon(ref decon) => {
                for field in decon.fields.iter().flat_map(|fields| fields.iter()) {
                    field.dfs_(f)?;
                }
            }
            PatternRep::Const(_) => {}
        }
        f(self)
    }
}

#[derive(Debug, Clone)]
pub enum PatternRep {
    Var(Box<PatternVar>),
    Wildcard,
    Decon(Box<PatternDecon>),
    Const(Const),
}

impl PatternRep {
    pub fn unit() -> Self {
        Self::Decon(Box::new(PatternDecon::unit()))
    }

    pub fn var(id: NodeId<PatternVar>, as_pat: Option<Pattern>) -> Self {
        Self::Var(Box::new(PatternVar { id, as_pat }))
    }

    pub fn decon(use_: impl Into<Use<ValueCon>>, fields: Option<Vec<Pattern>>) -> Self {
        Self::Decon(Box::new(PatternDecon {
            use_: use_.into(),
            fields,
        }))
    }

    pub fn tuple(ps: Vec<Pattern>) -> Self {
        Self::Decon(Box::new(PatternDecon::tuple(ps)))
    }
}

#[derive(Debug, Clone)]
pub struct PatternVar {
    pub id: NodeId<PatternVar>,
    pub as_pat: Option<Pattern>,
}

#[derive(Debug, Clone)]
pub struct PatternDecon {
    pub use_: Use<ValueCon>,
    pub fields: Option<Vec<Pattern>>,
}

impl PatternDecon {
    pub fn unit() -> Self {
        Self {
            use_: ValueCon::tuple(0).into(),
            fields: None,
        }
    }

    pub fn arity(&self) -> Option<u32> {
        self.fields.as_ref().map(|fields| fields.len() as u32)
    }

    pub fn tuple(ps: Vec<Pattern>) -> Self {
        Self {
            use_: ValueCon::tuple(ps.len()).into(),
            fields: if ps.is_empty() { None } else { Some(ps) },
        }
    }

    pub fn matches_tuple(&self) -> Option<&[Pattern]> {
        if let Use::Resolved(ValueCon::Data(id), _) = self.use_ {
            match (builtin::matches_tuple(id), self.fields.as_ref()) {
                (Some(0), None) => Some(&[]),
                (Some(n), Some(ps)) if ps.len() == n => Some(ps.as_slice()),
                _ => None,
            }
        } else {
            None
        }
    }
}

pub trait PatternFormatter {
    fn fmt_pattern_var(&self, f: &mut fmt::Formatter<'_>, id: NodeId<PatternVar>) -> fmt::Result;

    fn fmt_value_con_use(&self, f: &mut fmt::Formatter<'_>, use_: Use<ValueCon>) -> fmt::Result;
}

impl<F: PatternFormatter> ContextualDisplay<F> for Pattern {
    fn fmt_with(&self, ctx: &F, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.rep.fmt_on(ctx))
    }
}

impl<F: PatternFormatter> ContextualDisplay<F> for PatternRep {
    fn fmt_with(&self, ctx: &F, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Var(var) => write!(f, "{}", var.as_ref().fmt_on(ctx)),
            Self::Wildcard => write!(f, "_"),
            Self::Decon(decon) => write!(f, "{}", decon.as_ref().fmt_on(ctx)),
            Self::Const(lit) => write!(f, "{}", lit),
        }
    }
}

impl<F: PatternFormatter> ContextualDisplay<F> for PatternVar {
    fn fmt_with(&self, ctx: &F, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.as_pat {
            Some(ref pat) => write!(f, "(let {} {})", self.id.fmt_on(ctx), pat.fmt_on(ctx)),
            None => write!(f, "(let {})", self.id.fmt_on(ctx)),
        }
    }
}

impl<F: PatternFormatter> ContextualDisplay<F> for NodeId<PatternVar> {
    fn fmt_with(&self, ctx: &F, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        ctx.fmt_pattern_var(f, *self)
    }
}

impl<F: PatternFormatter> ContextualDisplay<F> for PatternDecon {
    fn fmt_with(&self, ctx: &F, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(pats) = self.matches_tuple() {
            write!(f, "(:")?;
            for p in pats {
                write!(f, " {}", p.fmt_on(ctx))?;
            }
            return write!(f, ")");
        }

        match self.fields {
            Some(ref fields) => {
                write!(f, "({}", self.use_.fmt_on(ctx))?;
                for field in fields {
                    write!(f, " {}", field.fmt_on(ctx))?;
                }
                write!(f, ")")
            }
            None => write!(f, "{}", self.use_.fmt_on(ctx)),
        }
    }
}

impl<F: PatternFormatter> ContextualDisplay<F> for Use<ValueCon> {
    fn fmt_with(&self, ctx: &F, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        ctx.fmt_value_con_use(f, *self)
    }
}
