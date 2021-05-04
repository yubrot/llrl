use super::builtin::{Sexp, Syntax};
use crate::string;
use ordered_float::OrderedFloat;
use std::fmt;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash)]
pub enum Const {
    Integer(bool, u64),
    FPNumber(OrderedFloat<f64>),
    String(String),
    Char(char),
    SyntaxSexp(Box<Syntax<Sexp>>),
}

impl fmt::Display for Const {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Integer(true, v) => write!(f, "{}", *v as i64),
            Self::Integer(false, v) => write!(f, "{}", v),
            Self::FPNumber(v) => write!(f, "{}", v),
            Self::String(s) => write!(f, "\"{}\"", string::escape(s)),
            Self::Char(c) => write!(f, "#\\{}", string::escape(&c.to_string())),
            Self::SyntaxSexp(s) => write!(f, "'{}", s),
        }
    }
}
