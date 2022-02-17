use super::{ir::*, BackendValue};
use crate::string;
use std::fmt;

/// Minimal BackendValue implementation
#[derive(Debug)]
pub enum Value {
    I(i64),
    SyntaxSexp(Syntax<Sexp>),
    String(String),
    Result(Result<Box<Value>, Box<Value>>),
    Null,
    EmptyArgv,
    Unknown,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::I(value) => write!(f, "{}", value),
            Self::SyntaxSexp(s) => write!(f, "'{}", s),
            Self::String(s) => write!(f, "\"{}\"", string::escape(s)),
            Self::Result(Ok(value)) => write!(f, "(ok {})", value),
            Self::Result(Err(value)) => write!(f, "(err {})", value),
            Self::Null => write!(f, "<null>"),
            Self::EmptyArgv => write!(f, "<empty-argv>"),
            Self::Unknown => write!(f, "<unknown>"),
        }
    }
}

impl BackendValue for Value {
    fn as_bool(&self) -> Option<bool> {
        match self {
            Self::I(v) => Some(*v != 0),
            _ => None,
        }
    }

    fn from_macro_src(sexp: &Syntax<Sexp>) -> Self {
        Self::SyntaxSexp(sexp.clone())
    }

    fn into_macro_dest(mut self) -> Result<Syntax<Sexp>, String> {
        match self {
            Self::Result(Ok(ref mut value)) => match **value {
                Self::SyntaxSexp(ref mut s) => {
                    return Ok(std::mem::replace(s, Syntax::<Sexp>::dummy()))
                }
                _ => {}
            },
            Self::Result(Err(ref mut value)) => match **value {
                Self::String(ref mut s) => return Err(std::mem::take(s)),
                _ => {}
            },
            _ => {}
        }
        Err(format!(
            "Cannot treat {} as Result<Syntax<Sexp>, String>",
            self
        ))
    }
}
