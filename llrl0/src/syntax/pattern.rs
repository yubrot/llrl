use super::{Name, Parameterizable, Use};
use crate::sexp::{matcher as m, Sexp, SexpRep};
use crate::source_loc::SourceLocation;
use derive_new::new;
use either::*;
use std::borrow::Cow;

#[derive(Debug, Clone)]
pub enum Pattern<'a> {
    Const(&'a Sexp),
    Var(PatternVar<'a>),
    Tuple(PatternTuple<'a>),
    Unit,
    Decon(PatternDecon<'a>),
    Wildcard,
}

impl<'a> m::Match<'a> for Pattern<'a> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<(m::Symbol, m::Rest<m::Any>)>() {
            Ok(("let", _)) => return Ok(Self::Var(s.matches::<PatternVar>()?)),
            Ok((":", _)) => return Ok(Self::Tuple(s.matches::<PatternTuple>()?)),
            _ => {}
        }

        match s.rep {
            SexpRep::Integer(_, _)
            | SexpRep::FPNumber(_)
            | SexpRep::String(_)
            | SexpRep::Char(_)
            | SexpRep::Bool(_) => Ok(Self::Const(s)),
            SexpRep::Symbol(ref sym) if sym == "unit" => Ok(Self::Unit),
            SexpRep::Symbol(ref sym) if sym == "_" => Ok(Self::Wildcard),
            SexpRep::Symbol(_) | SexpRep::List(_) | SexpRep::Native(_) => {
                Ok(Self::Decon(s.matches::<PatternDecon>()?))
            }
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        "<pattern>".into()
    }
}

impl_default_match_slice!(['a] MatchSlice<'a> for Pattern<'a>);

#[derive(Debug, Clone, new)]
pub struct PatternVar<'a> {
    pub loc: SourceLocation,
    pub name: Name<'a>,
    pub as_pat: Option<&'a Sexp>,
}

impl<'a> m::Match<'a> for PatternVar<'a> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<Either<(m::Symbol, Name), (m::Symbol, Name, m::Any)>>() {
            Ok(Left(("let", name))) => Ok(Self::new(s.loc, name, None)),
            Ok(Right(("let", name, pat))) => Ok(Self::new(s.loc, name, Some(pat))),
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        "<(let <name>) or (let <name> <as-pattern>)>".into()
    }
}

#[derive(Debug, Clone, new)]
pub struct PatternTuple<'a> {
    pub loc: SourceLocation,
    pub patterns: &'a [Sexp],
}

impl<'a> m::Match<'a> for PatternTuple<'a> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<(m::Symbol, m::Rest<m::Any>)>() {
            Ok((":", patterns)) => Ok(Self::new(s.loc, patterns)),
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        "(: <pattern> ...)".into()
    }
}

#[derive(Debug, Clone, new)]
pub struct PatternDecon<'a> {
    pub loc: SourceLocation,
    pub con: Use<'a>,
    pub fields: Option<&'a [Sexp]>,
}

impl<'a> m::Match<'a> for PatternDecon<'a> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<Parameterizable<Use, m::Any>>() {
            Ok(p) => Ok(Self::new(s.loc, p.callee, p.params)),
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        "<<con> or (<con> <field> ...)>".into()
    }
}
