use crate::source_loc::SourceLocation;
use crate::sexp::{matcher as m, Sexp, SexpRep};
use derive_new::new;
use std::borrow::Cow;

#[derive(Debug, Clone)]
pub enum Kind<'a> {
    Fun(KindFun<'a>),
    Type,
    Constraint,
    Satisfaction,
    Value,
    Macro,
}

impl<'a> m::Match<'a> for Kind<'a> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<(m::Symbol, m::Rest<m::Any>)>() {
            Ok(("->", _)) => return Ok(Kind::Fun(s.matches::<KindFun>()?)),
            _ => {}
        }

        match s.rep {
            SexpRep::Symbol(ref sym) if sym == "*" => Ok(Kind::Type),
            SexpRep::Symbol(ref sym) if sym == "Constraint" => Ok(Kind::Constraint),
            SexpRep::Symbol(ref sym) if sym == "Satisfaction" => Ok(Kind::Satisfaction),
            SexpRep::Symbol(ref sym) if sym == "Value" => Ok(Kind::Value),
            SexpRep::Symbol(ref sym) if sym == "Macro" => Ok(Kind::Macro),
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        "<kind>".into()
    }
}

impl_default_match_slice!(['a] MatchSlice<'a> for Kind<'a>);

#[derive(Debug, Clone, new)]
pub struct KindFun<'a> {
    pub loc: SourceLocation,
    pub args: &'a [Sexp],
    pub ret: &'a Sexp,
}

impl<'a> m::Match<'a> for KindFun<'a> {
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
