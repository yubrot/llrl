use super::{Name, Parameterizable, Use};
use crate::sexp::{matcher as m, Match as _, MatchError, Sexp};
use crate::source_loc::SourceLocation;
use derive_new::new;
use either::*;
use std::borrow::Cow;

#[derive(Debug, Clone)]
pub enum Type<'a> {
    Fun(TypeFun<'a>),
    Tuple(TypeTuple<'a>),
    Unit,
    Use(Use<'a>),
    App(TypeApply<'a>),
}

impl<'a> m::Match<'a> for Type<'a> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<(m::Symbol, m::Rest<m::Any>)>() {
            Ok(("->", _)) => return Ok(Self::Fun(s.matches::<TypeFun>()?)),
            Ok((":", _)) => return Ok(Self::Tuple(s.matches::<TypeTuple>()?)),
            _ => {}
        }

        match s.matches::<Either<Use, TypeApply>>() {
            Ok(Left(Use::Name(name))) if name.sym == "unit" => Ok(Self::Unit),
            Ok(Left(use_)) => Ok(Self::Use(use_)),
            Ok(Right(apply)) => Ok(Self::App(apply)),
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        "<type>".into()
    }
}

impl_default_match_slice!(['a] MatchSlice<'a> for Type<'a>);

#[derive(Debug, Clone, new)]
pub struct TypeFun<'a> {
    pub loc: SourceLocation,
    pub args: &'a [Sexp],
    pub ret: &'a Sexp,
}

impl<'a> m::Match<'a> for TypeFun<'a> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<(m::Symbol, m::Rest<m::Any>)>() {
            Ok(("->", args_ret)) if !args_ret.is_empty() => {
                let (ret, args) = args_ret.split_last().unwrap();
                Ok(Self::new(s.loc, args, ret))
            }
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        "(-> <arg> ... <ret>)".into()
    }
}

#[derive(Debug, Clone, new)]
pub struct TypeTuple<'a> {
    pub loc: SourceLocation,
    pub tys: &'a [Sexp],
}

impl<'a> m::Match<'a> for TypeTuple<'a> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<(m::Symbol, m::Rest<m::Any>)>() {
            Ok((":", tys)) => Ok(Self::new(s.loc, tys)),
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        "(: <ty> ...)".into()
    }
}

#[derive(Debug, Clone, new)]
pub struct TypeApply<'a> {
    pub loc: SourceLocation,
    pub callee: &'a Sexp,
    pub args: &'a [Sexp],
}

impl<'a> m::Match<'a> for TypeApply<'a> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<(m::Any, m::Rest<m::Any>)>() {
            Ok((callee, args)) => Ok(Self::new(s.loc, callee, args)),
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        "(<callee> <arg> ...)".into()
    }
}

#[derive(Debug, Clone, new)]
pub struct Constraint<'a> {
    pub loc: SourceLocation,
    pub target: Use<'a>,
    pub args: Option<&'a [Sexp]>,
}

impl<'a> m::Match<'a> for Constraint<'a> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<Parameterizable<Use, m::Any>>() {
            Ok(p) => Ok(Self::new(s.loc, p.callee, p.params)),
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        "<<class-name> or (<class-name> <arg> ...)>".into()
    }
}

impl_default_match_slice!(['a] MatchSlice<'a> for Constraint<'a>);

#[derive(Debug, Clone, new)]
pub struct TypeParameter<'a> {
    pub loc: SourceLocation,
    pub name: Name<'a>,
    pub kind_ann: Option<&'a Sexp>,
}

impl<'a> m::Match<'a> for TypeParameter<'a> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<Either<Name, (Name, m::Any)>>() {
            Ok(Left(name)) => Ok(Self::new(s.loc, name, None)),
            Ok(Right((name, kind_ann))) => Ok(Self::new(s.loc, name, Some(kind_ann))),
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        "<<ty-param-name> or (<ty-param-name> <kind>)>".into()
    }
}

impl_default_match_slice!(['a] MatchSlice<'a> for TypeParameter<'a>);

#[derive(Debug, Clone, new)]
pub struct Scheme<'a> {
    pub loc: SourceLocation,
    pub ty_params: Vec<TypeParameter<'a>>,
    pub s_params: Vec<Constraint<'a>>,
    pub body: &'a Sexp,
}

impl<'a> m::Match<'a> for Scheme<'a> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        Ok(Self::new(s.loc, Vec::new(), Vec::new(), s))
    }

    fn expect() -> Cow<'static, str> {
        "<type>".into()
    }
}

impl<'a> m::MatchSlice<'a> for Scheme<'a> {
    type SliceResult = Self;

    fn match_sexp_slice(ls: &'a [Sexp], loc: SourceLocation) -> m::SliceResult<Self> {
        let (forall, ls) = Forall::match_first(ls)?;
        let (ls, where_) = Where::match_last(ls)?;

        match ls {
            [body] => Ok(Self::new(
                body.loc,
                forall.map_or(Vec::new(), |f| f.params),
                where_.map_or(Vec::new(), |w| w.constraints),
                body,
            )),
            _ => Err(MatchError::new(loc, Self::expect_slice)),
        }
    }

    fn expect_slice() -> Cow<'static, str> {
        format!("{}? <type> {}?", Forall::expect(), Where::expect()).into()
    }
}

#[derive(Debug, new)]
pub struct Forall<'a> {
    pub loc: SourceLocation,
    pub params: Vec<TypeParameter<'a>>,
}

impl<'a> m::Match<'a> for Forall<'a> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<(m::Symbol, m::Rest<TypeParameter>)>() {
            Ok(("forall", params)) => Ok(Self::new(s.loc, params)),
            _ => Err(Self::error(s)),
        }
    }

    fn try_match_sexp(s: &'a Sexp) -> Option<m::Result<Self>> {
        match s.matches::<(m::Symbol, m::Rest<m::Any>)>() {
            Ok(("forall", _)) => Some(Self::match_sexp(s)),
            _ => None,
        }
    }

    fn expect() -> Cow<'static, str> {
        "(forall <ty-param> ...)".into()
    }
}

#[derive(Debug, new)]
pub struct Where<'a> {
    pub loc: SourceLocation,
    pub constraints: Vec<Constraint<'a>>,
}

impl<'a> m::Match<'a> for Where<'a> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<(m::Symbol, m::Rest<Constraint>)>() {
            Ok(("where", constraints)) => Ok(Self::new(s.loc, constraints)),
            _ => Err(Self::error(s)),
        }
    }

    fn try_match_sexp(s: &'a Sexp) -> Option<m::Result<Self>> {
        match s.matches::<(m::Symbol, m::Rest<m::Any>)>() {
            Ok(("where", _)) => Some(Self::match_sexp(s)),
            _ => None,
        }
    }

    fn expect() -> Cow<'static, str> {
        "(where <constraint> ...)".into()
    }
}
