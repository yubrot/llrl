use crate::source_loc::{SourceLocation, SourceLocator};
use crate::string;
use derive_new::new;
use if_chain::if_chain;
use ordered_float::OrderedFloat;
use std::borrow::Cow;
use std::fmt;

#[macro_use]
pub mod matcher;
pub mod native;

#[cfg(test)]
mod tests;

pub use matcher::{Error as MatchError, Match, MatchSlice};
pub use native::{Native, NativeValue};

/// A sequence of S-expressions.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, new)]
pub struct Ss {
    pub loc: SourceLocation,
    pub ss: Vec<Sexp>,
}

impl fmt::Display for Ss {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, s) in self.ss.iter().enumerate() {
            if i != 0 {
                writeln!(f)?;
            }
            write!(f, "{}", s)?;
        }
        Ok(())
    }
}

/// S-expression.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash, new)]
pub struct Sexp {
    pub loc: SourceLocation,
    pub rep: SexpRep,
}

impl Sexp {
    pub fn dummy() -> Self {
        Self::new(
            SourceLocator::temporary().issue(Default::default()),
            SexpRep::default(),
        )
    }

    pub fn integer(loc: SourceLocation, signed: bool, value: u64) -> Self {
        Self::new(loc, SexpRep::Integer(signed, value))
    }

    pub fn signed(loc: SourceLocation, value: i64) -> Self {
        Self::new(loc, SexpRep::signed(value))
    }

    pub fn unsigned(loc: SourceLocation, value: u64) -> Self {
        Self::new(loc, SexpRep::unsigned(value))
    }

    pub fn fpnumber(loc: SourceLocation, value: OrderedFloat<f64>) -> Self {
        Self::new(loc, SexpRep::FPNumber(value))
    }

    pub fn symbol(loc: SourceLocation, value: impl Into<Cow<'static, str>>) -> Self {
        Self::new(loc, SexpRep::symbol(value))
    }

    pub fn string(loc: SourceLocation, value: impl Into<Cow<'static, str>>) -> Self {
        Self::new(loc, SexpRep::string(value))
    }

    pub fn char(loc: SourceLocation, value: char) -> Self {
        Self::new(loc, SexpRep::Char(value))
    }

    pub fn native(loc: SourceLocation, value: impl NativeValue) -> Self {
        Self::new(loc, SexpRep::native(value))
    }

    pub fn list(loc: SourceLocation, list: Vec<Self>) -> Self {
        Self::new(loc, SexpRep::List(list))
    }

    pub fn list_like(loc: SourceLocation, head: Self, init: Vec<Self>, last: Self) -> Self {
        Self::new(loc, SexpRep::list_like(head, init, last))
    }

    pub fn bool(loc: SourceLocation, value: bool) -> Self {
        Self::new(loc, SexpRep::Bool(value))
    }

    pub fn cons(loc: SourceLocation, car: Self, cdr: Self) -> Self {
        Self::new(loc, SexpRep::cons(car, cdr))
    }

    pub fn nil(loc: SourceLocation) -> Self {
        Self::new(loc, SexpRep::nil())
    }

    pub fn quote(loc: SourceLocation, s: Self) -> Self {
        Self::new(loc, SexpRep::quote(loc, s))
    }

    pub fn quasiquote(loc: SourceLocation, s: Self) -> Self {
        Self::new(loc, SexpRep::quasiquote(loc, s))
    }

    pub fn unquote(loc: SourceLocation, s: Self) -> Self {
        Self::new(loc, SexpRep::unquote(loc, s))
    }

    pub fn unquote_splicing(loc: SourceLocation, s: Self) -> Self {
        Self::new(loc, SexpRep::unquote_splicing(loc, s))
    }

    pub fn load(loc: SourceLocation, s: Self) -> Self {
        Self::new(loc, SexpRep::load(loc, s))
    }

    pub fn capture(loc: SourceLocation, s: Self) -> Self {
        Self::new(loc, SexpRep::capture(loc, s))
    }

    pub fn try_question(loc: SourceLocation, s: Self) -> Self {
        Self::new(loc, SexpRep::try_question(loc, s))
    }

    pub fn try_exclamation(loc: SourceLocation, s: Self) -> Self {
        Self::new(loc, SexpRep::try_exclamation(loc, s))
    }

    pub fn annotate(loc: SourceLocation, s: Self, ss: Vec<Self>) -> Self {
        Self::new(loc, SexpRep::annotate(loc, s, ss))
    }

    pub fn matches<'a, M: Match<'a>>(&'a self) -> matcher::Result<M> {
        M::match_sexp(self)
    }
}

impl fmt::Display for Sexp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.rep.fmt(f)
    }
}

impl<'a> From<Sexp> for Cow<'a, Sexp> {
    fn from(s: Sexp) -> Self {
        Cow::Owned(s)
    }
}

impl<'a> From<&'a Sexp> for Cow<'a, Sexp> {
    fn from(s: &'a Sexp) -> Self {
        Cow::Borrowed(s)
    }
}

pub const QUOTE: &str = "quote";
pub const QUASIQUOTE: &str = "quasiquote";
pub const UNQUOTE: &str = "unquote";
pub const UNQUOTE_SPLICING: &str = "unquote-splicing";
pub const LOAD: &str = "load";
pub const CAPTURE: &str = "capture";
pub const TRY_QUESTION: &str = "try?";
pub const TRY_EXCLAMATION: &str = "try!";
pub const ANNOTATE: &str = "annotate";

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash)]
pub enum SexpRep {
    Integer(bool, u64),
    FPNumber(OrderedFloat<f64>),
    Symbol(Cow<'static, str>),
    String(Cow<'static, str>),
    Char(char),
    List(Vec<Sexp>),
    ListLike(Box<(Sexp, Vec<Sexp>, Sexp)>),
    Bool(bool),
    Native(Native),
}

impl SexpRep {
    pub fn signed(value: i64) -> Self {
        Self::Integer(true, value as u64)
    }

    pub fn unsigned(value: u64) -> Self {
        Self::Integer(false, value)
    }

    pub fn symbol(value: impl Into<Cow<'static, str>>) -> Self {
        Self::Symbol(value.into())
    }

    pub fn string(value: impl Into<Cow<'static, str>>) -> Self {
        Self::String(value.into())
    }

    pub fn native(value: impl NativeValue) -> Self {
        Self::Native(Native::new(value))
    }

    pub fn list_like(head: Sexp, init: Vec<Sexp>, last: Sexp) -> Self {
        SexpRep::ListLike(Box::new((head, init, last)))
    }

    pub fn cons(car: Sexp, cdr: Sexp) -> Self {
        match cdr.rep {
            Self::List(mut list) => {
                list.insert(0, car);
                Self::List(list)
            }
            Self::ListLike(mut list_like) => {
                let prev_head = std::mem::replace(&mut list_like.0, car);
                list_like.1.insert(0, prev_head);
                Self::ListLike(list_like)
            }
            _ => Self::list_like(car, Vec::new(), cdr),
        }
    }

    pub fn nil() -> Self {
        Self::List(Vec::new())
    }

    pub fn quote(loc: SourceLocation, s: Sexp) -> Self {
        Self::List(vec![Sexp::symbol(loc, QUOTE), s])
    }

    pub fn quasiquote(loc: SourceLocation, s: Sexp) -> Self {
        Self::List(vec![Sexp::symbol(loc, QUASIQUOTE), s])
    }

    pub fn unquote(loc: SourceLocation, s: Sexp) -> Self {
        Self::List(vec![Sexp::symbol(loc, UNQUOTE), s])
    }

    pub fn unquote_splicing(loc: SourceLocation, s: Sexp) -> Self {
        Self::List(vec![Sexp::symbol(loc, UNQUOTE_SPLICING), s])
    }

    pub fn load(loc: SourceLocation, s: Sexp) -> Self {
        Self::List(vec![Sexp::symbol(loc, LOAD), s])
    }

    pub fn capture(loc: SourceLocation, s: Sexp) -> Self {
        Self::List(vec![Sexp::symbol(loc, CAPTURE), s])
    }

    pub fn try_question(loc: SourceLocation, s: Sexp) -> Self {
        Self::List(vec![Sexp::symbol(loc, TRY_QUESTION), s])
    }

    pub fn try_exclamation(loc: SourceLocation, s: Sexp) -> Self {
        Self::List(vec![Sexp::symbol(loc, TRY_EXCLAMATION), s])
    }

    pub fn annotate(loc: SourceLocation, s: Sexp, mut ss: Vec<Sexp>) -> Self {
        let mut ls = vec![Sexp::symbol(loc, ANNOTATE), s];
        ls.append(&mut ss);
        Self::List(ls)
    }
}

impl Default for SexpRep {
    fn default() -> Self {
        Self::List(Vec::new())
    }
}

impl fmt::Display for SexpRep {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if_chain! {
            if let Self::List(ls) = self;
            if let [callee, args @ ..] = ls.as_slice();
            if let Self::Symbol(ref sym) = callee.rep;
            then {
                match (sym.as_ref(), args) {
                    (QUOTE, [s]) => return write!(f, "'{}", s),
                    (QUASIQUOTE, [s]) => return write!(f, "`{}", s),
                    (UNQUOTE, [s]) => return write!(f, ",{}", s),
                    (UNQUOTE_SPLICING, [s]) => return write!(f, ",@{}", s),
                    (LOAD, [s]) => return write!(f, "~{}", s),
                    (CAPTURE, [s]) => return write!(f, "\\{}", s),
                    (TRY_QUESTION, [s @ Sexp { rep: Self::List(_), .. }]) => return write!(f, "{}?", s),
                    (TRY_EXCLAMATION, [s @ Sexp { rep: Self::List(_), .. }]) => return write!(f, "{}!", s),
                    (ANNOTATE, [s, ss @ ..]) => {
                        write!(f, "{} {{", s)?;
                        for (i, s) in ss.iter().enumerate() {
                            if i == 0 {
                                write!(f, "{}", s)?;
                            } else {
                                write!(f, " {}", s)?;
                            }
                        }
                        write!(f, "}}")?;
                        return Ok(());
                    }
                    _ => {}
                }
            }
        }

        match self {
            Self::Integer(true, v) => write!(f, "{}", *v as i64),
            Self::Integer(false, v) => write!(f, "{}", v),
            Self::FPNumber(v) => write!(f, "{}", v),
            Self::Symbol(s) => write!(f, "{}", s),
            Self::String(s) => write!(f, "\"{}\"", string::escape(s)),
            Self::Char(c) => write!(f, "#\\{}", string::escape(&c.to_string())),
            Self::List(l) => {
                write!(f, "(")?;
                for (i, s) in l.iter().enumerate() {
                    if i == 0 {
                        write!(f, "{}", s)?;
                    } else {
                        write!(f, " {}", s)?;
                    }
                }
                write!(f, ")")
            }
            Self::ListLike(l) => {
                write!(f, "({}", l.0)?;
                for v in l.1.iter() {
                    write!(f, " {}", v)?;
                }
                write!(f, " . {})", l.2)
            }
            Self::Bool(true) => write!(f, "#t"),
            Self::Bool(false) => write!(f, "#f"),
            Self::Native(native) => write!(f, "{}", native),
        }
    }
}
