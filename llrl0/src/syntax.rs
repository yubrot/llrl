//! Provides a llrl syntax matcher to S-expressions.

use crate::sexp::{self, matcher as m, Sexp};
use crate::source_loc::SourceLocation;
use derive_new::new;
use either::*;
use std::borrow::Cow;

mod decl;
mod expr;
mod kind;
mod pattern;
mod types;

pub use decl::*;
pub use expr::*;
pub use kind::*;
pub use pattern::*;
pub use types::*;

#[cfg(test)]
mod tests;

#[derive(Debug, Clone, Copy, new)]
pub struct Name<'a> {
    pub loc: SourceLocation,
    pub sym: &'a str,
}

impl<'a> m::Match<'a> for Name<'a> {
    type Result = Name<'a>;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<m::Symbol>() {
            Ok(sym) => Ok(Self::new(s.loc, sym)),
            Err(_) => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        "<name>".into()
    }
}

#[derive(Debug, Clone, new)]
pub struct Native<'a> {
    pub loc: SourceLocation,
    pub rep: &'a sexp::Native,
}

impl<'a> m::Match<'a> for Native<'a> {
    type Result = Native<'a>;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<m::Native>() {
            Ok(rep) => Ok(Self::new(s.loc, rep)),
            Err(_) => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        "<native>".into()
    }
}

#[derive(Debug, Clone)]
pub enum Use<'a> {
    Name(Name<'a>),
    Native(Native<'a>),
}

impl Use<'_> {
    pub fn loc(&self) -> SourceLocation {
        match self {
            Self::Name(name) => name.loc,
            Self::Native(native) => native.loc,
        }
    }
}

impl<'a> m::Match<'a> for Use<'a> {
    type Result = Use<'a>;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<Either<Name, Native>>() {
            Ok(Left(name)) => Ok(Self::Name(name)),
            Ok(Right(native)) => Ok(Self::Native(native)),
            Err(_) => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        "<name>".into()
    }
}

#[derive(Debug, Clone, new)]
pub struct Annotatable<'a, T: m::Match<'a>, S: m::MatchSlice<'a>> {
    body: T::Result,
    annotation: Option<S::SliceResult>,
}

impl<'a, T: m::Match<'a>, S: m::MatchSlice<'a>> m::Match<'a> for Annotatable<'a, T, S> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        if let Ok((sexp::ANNOTATE, body, ann)) = s.matches::<(m::Symbol, m::Any, m::Rest<m::Any>)>()
        {
            let body = T::match_sexp(body)?;
            let ann = S::match_sexp_slice(ann, s.loc)?;
            Ok(Self::new(body, Some(ann)))
        } else if let Ok(body) = s.matches::<T>() {
            Ok(Self::new(body, None))
        } else {
            Err(Self::error(s))
        }
    }

    fn expect() -> Cow<'static, str> {
        let t = T::expect();
        let s = S::expect_slice();
        format!("<{} or {} {{{}}}>", t, t, s).into()
    }
}

#[derive(Debug, Clone, new)]
pub struct Parameterizable<'a, T: m::Match<'a>, S: m::MatchSlice<'a>> {
    callee: T::Result,
    params: Option<S::SliceResult>,
}

impl<'a, T: m::Match<'a>, S: m::MatchSlice<'a>> m::Match<'a> for Parameterizable<'a, T, S> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<Either<T, (T, m::Rest<S>)>>() {
            Ok(Left(callee)) => Ok(Self::new(callee, None)),
            Ok(Right((callee, params))) => Ok(Self::new(callee, Some(params))),
            Err(_) => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        format!(
            "<{} or ({} {})>",
            T::expect(),
            T::expect(),
            S::expect_slice()
        )
        .into()
    }
}

#[derive(Debug, Clone, new)]
pub struct MacroApply<'a> {
    pub loc: SourceLocation,
    pub callee: Use<'a>,
    pub args: &'a [Sexp],
}

impl<'a> m::Match<'a> for MacroApply<'a> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<(Use, m::Rest<m::Any>)>() {
            Ok((callee, args)) => Ok(Self::new(s.loc, callee, args)),
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        "(<macro> _ ...)".into()
    }
}
