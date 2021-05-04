use super::{Sexp, SexpRep};
use crate::formatting::ContextualDisplay;
use crate::source_loc::{SourceLocation, SourceLocationTable};
use derive_new::new;
use either::*;
use std::borrow::Cow;
use std::fmt::{self, Write as _};
use std::marker::PhantomData;

#[derive(PartialEq, PartialOrd, Debug, Clone, new)]
pub struct Error {
    pub loc: SourceLocation,
    expect: fn() -> Cow<'static, str>,
}

impl ContextualDisplay<SourceLocationTable> for Error {
    fn fmt_with(&self, ctx: &SourceLocationTable, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}: Syntax error: expected {}",
            self.loc.fmt_on(ctx),
            (self.expect)()
        )
    }
}

pub type Result<'a, T> = std::result::Result<<T as Match<'a>>::Result, Error>;

pub type MatchFirstResult<'a, T> =
    std::result::Result<(Option<<T as Match<'a>>::Result>, &'a [Sexp]), Error>;

pub type MatchLastResult<'a, T> =
    std::result::Result<(&'a [Sexp], Option<<T as Match<'a>>::Result>), Error>;

/// Pattern matching with the structure of a S-expression.
pub trait Match<'a> {
    type Result;

    fn match_sexp(s: &'a Sexp) -> Result<Self>;

    fn try_match_sexp(s: &'a Sexp) -> Option<Result<Self>> {
        Some(Self::match_sexp(s))
    }

    fn expect() -> Cow<'static, str>;

    fn error(s: &'a Sexp) -> Error {
        Error::new(s.loc, Self::expect)
    }

    fn match_first(slice: &'a [Sexp]) -> MatchFirstResult<Self> {
        Ok(match slice.first().and_then(Self::try_match_sexp) {
            Some(r) => (Some(r?), &slice[1..]),
            _ => (None, slice),
        })
    }

    fn match_last(slice: &'a [Sexp]) -> MatchLastResult<Self> {
        Ok(match slice.last().and_then(Self::try_match_sexp) {
            Some(r) => (&slice[..slice.len() - 1], Some(r?)),
            _ => (slice, None),
        })
    }
}

pub type SliceResult<'a, T> = std::result::Result<<T as MatchSlice<'a>>::SliceResult, Error>;

pub trait MatchSlice<'a>: Match<'a> {
    type SliceResult;

    fn match_sexp_slice(slice: &'a [Sexp], loc: SourceLocation) -> SliceResult<Self>;

    fn expect_slice() -> Cow<'static, str>;
}

// rust-lang/rust #37653
macro_rules! impl_default_match_slice {
    ([$($p:tt)*] MatchSlice<$l:lifetime> for $ty:ty) => {
        impl<$($p)*> $crate::sexp::matcher::MatchSlice<$l> for $ty {
            type SliceResult = Vec<Self::Result>;

            fn match_sexp_slice(
                slice: &$l [$crate::sexp::Sexp],
                _: crate::source_loc::SourceLocation,
            ) -> $crate::sexp::matcher::SliceResult<Self> {
                slice
                    .into_iter()
                    .map(|s| <Self as $crate::sexp::matcher::Match>::match_sexp(s))
                    .collect()
            }

            fn expect_slice() -> std::borrow::Cow<'static, str> {
                format!("{} ...", <Self as $crate::sexp::matcher::Match>::expect()).into()
            }
        }
    };
}

#[derive(Debug, Clone)]
pub struct Any;

impl<'a> Match<'a> for Any {
    type Result = &'a Sexp;

    fn match_sexp(s: &'a Sexp) -> Result<Self> {
        Ok(s)
    }

    fn expect() -> Cow<'static, str> {
        "_".into()
    }
}

impl<'a> MatchSlice<'a> for Any {
    type SliceResult = &'a [Sexp];

    fn match_sexp_slice(slice: &'a [Sexp], _: SourceLocation) -> SliceResult<Self> {
        Ok(slice)
    }

    fn expect_slice() -> Cow<'static, str> {
        "_ ...".into()
    }
}

#[derive(Debug, Clone)]
pub struct Symbol;

impl<'a> Match<'a> for Symbol {
    type Result = &'a str;

    fn match_sexp(s: &'a Sexp) -> Result<Self> {
        match s.rep {
            SexpRep::Symbol(ref s) => Ok(s),
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        "<symbol>".into()
    }
}

impl_default_match_slice!(['a] MatchSlice<'a> for Symbol);

#[derive(Debug, Clone)]
pub struct String;

impl<'a> Match<'a> for String {
    type Result = &'a str;

    fn match_sexp(s: &'a Sexp) -> Result<Self> {
        match s.rep {
            SexpRep::String(ref s) => Ok(s),
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        "<string>".into()
    }
}

impl_default_match_slice!(['a] MatchSlice<'a> for String);

#[derive(Debug, Clone)]
pub struct Native;

impl<'a> Match<'a> for Native {
    type Result = &'a super::Native;

    fn match_sexp(s: &'a Sexp) -> Result<Self> {
        match s.rep {
            SexpRep::Native(ref n) => Ok(n),
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        "<native>".into()
    }
}

impl_default_match_slice!(['a] MatchSlice<'a> for Native);

#[derive(Debug, Clone)]
pub struct List<T> {
    _marker: PhantomData<T>,
}

impl<'a, T: MatchSlice<'a>> Match<'a> for List<T> {
    type Result = T::SliceResult;

    fn match_sexp(s: &'a Sexp) -> Result<Self> {
        match s.rep {
            SexpRep::List(ref ls) => T::match_sexp_slice(ls.as_slice(), s.loc),
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        format!("({})", T::expect_slice()).into()
    }
}

impl_default_match_slice!(['a, T: MatchSlice<'a>] MatchSlice<'a> for List<T>);

impl<'a> Match<'a> for () {
    type Result = ();

    fn match_sexp(s: &'a Sexp) -> Result<Self> {
        match s.rep {
            SexpRep::List(ref ss) if ss.is_empty() => Ok(()),
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        "()".into()
    }
}

impl_default_match_slice!(['a] MatchSlice<'a> for ());

#[derive(Debug)]
pub struct Rest<T> {
    _marker: PhantomData<T>,
}

macro_rules! seq_matcher {
    ($($var:ident: $matcher:ident),*) => {
        impl<'a, $($matcher: Match<'a>),*> Match<'a> for ($($matcher,)*) {
            type Result = ($($matcher::Result,)*);

            fn match_sexp(s: &'a Sexp) -> Result<Self> {
                match s.rep {
                    SexpRep::List(ref ss) => {
                        match ss.as_slice() {
                            [$($var),*] => {
                                $(let $var = $matcher::match_sexp($var)?;)*
                                Ok(($($var,)*))
                            }
                            _ => Err(Self::error(s)),
                        }
                    }
                    _ => Err(Self::error(s)),
                }
            }

            fn expect() -> Cow<'static, str> {
                let mut w = std::string::String::new();
                write!(&mut w, "(").unwrap();
                for (i, s) in [$($matcher::expect()),*].iter().enumerate() {
                    if i != 0 {
                        write!(&mut w, " ").unwrap();
                    }
                    write!(&mut w, "{}", s).unwrap();
                }
                write!(&mut w, ")").unwrap();
                w.into()
            }
        }

        impl_default_match_slice!(['a, $($matcher: Match<'a>),*] MatchSlice<'a> for ($($matcher,)*));

        impl<'a, $($matcher: Match<'a>,)* T: MatchSlice<'a>> Match<'a> for ($($matcher,)* Rest<T>) {
            type Result = ($($matcher::Result,)* T::SliceResult);

            fn match_sexp(s: &'a Sexp) -> Result<Self> {
                match s.rep {
                    SexpRep::List(ref ss) => {
                        match ss.as_slice() {
                            [$($var,)* rest @ ..] => {
                                $(let $var = $matcher::match_sexp($var)?;)*
                                let rest = T::match_sexp_slice(rest, s.loc)?;
                                Ok(($($var,)* rest))
                            }
                            _ => Err(Self::error(s)),
                        }
                    }
                    _ => Err(Self::error(s)),
                }
            }

            fn expect() -> Cow<'static, str> {
                let mut w = std::string::String::new();
                write!(&mut w, "(").unwrap();
                for s in (&[$($matcher::expect()),*]).iter() {
                    write!(&mut w, "{} ", s).unwrap();
                }
                write!(&mut w, "{} ...)", T::expect()).unwrap();
                w.into()
            }
        }

        impl_default_match_slice!(['a, $($matcher: Match<'a>,)* T: MatchSlice<'a>] MatchSlice<'a> for ($($matcher,)* Rest<T>));
    };
}

seq_matcher!(a: A);
seq_matcher!(a: A, b: B);
seq_matcher!(a: A, b: B, c: C);
seq_matcher!(a: A, b: B, c: C, d: D);
seq_matcher!(a: A, b: B, c: C, d: D, e: E);
seq_matcher!(a: A, b: B, c: C, d: D, e: E, f: F);
seq_matcher!(a: A, b: B, c: C, d: D, e: E, f: F, g: G);

impl<'a, L: Match<'a>, R: Match<'a>> Match<'a> for Either<L, R> {
    type Result = Either<L::Result, R::Result>;

    fn match_sexp(s: &'a Sexp) -> Result<Self> {
        match L::match_sexp(s) {
            Ok(r) => Ok(Left(r)),
            Err(_) => match R::match_sexp(s) {
                Ok(r) => Ok(Right(r)),
                Err(_) => Err(Self::error(s)),
            },
        }
    }

    fn expect() -> Cow<'static, str> {
        format!("<{} or {}>", L::expect(), R::expect()).into()
    }
}

impl_default_match_slice!(['a, L: Match<'a>, R: Match<'a>] MatchSlice<'a> for Either<L, R>);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::parse;
    use crate::sexp::{Sexp, SexpRep};
    use crate::source_loc::SourceLocator;

    macro_rules! assert_matches {
        ($input:tt, $ty:ty, $($pat:tt)*) => {
            assert!(matches!(
                parse::<Sexp, _>(&mut SourceLocator::temporary(), Lexer::new($input))
                    .aggregate_errors()
                    .unwrap()
                    .matches::<$ty>(),
                $($pat)*
            ));
        };
    }

    #[test]
    fn test_any() {
        assert_matches!(
            "123",
            Any,
            Ok(Sexp {
                rep: SexpRep::Integer(_, _),
                ..
            })
        );
        assert_matches!(
            "(a b)",
            Any,
            Ok(Sexp {
                rep: SexpRep::List(_),
                ..
            })
        );
        assert_eq!(Any::expect(), "_");
    }

    #[test]
    fn test_symbol() {
        assert_matches!("foo", Symbol, Ok("foo"));
        assert_matches!("(a b)", Symbol, Err(_));
        assert_eq!(Symbol::expect(), "<symbol>");
    }

    #[test]
    fn test_string() {
        assert_matches!("foo", String, Err(_));
        assert_matches!("\"foo\"", String, Ok("foo"));
        assert_eq!(String::expect(), "<string>");
    }

    #[test]
    fn test_list() {
        assert_matches!("foo", List<Any>, Err(_));
        assert_matches!("(x y z)", List<Any>, Ok([_, _, _]));
        assert_matches!("(1 2 3)", List<Symbol>, Err(_));
        assert_matches!("(x y)", List<Symbol>, Ok(vec) if matches!(vec.as_slice(), ["x", "y"]));
        assert_eq!(List::<Any>::expect(), "(_ ...)");
        assert_eq!(List::<Symbol>::expect(), "(<symbol> ...)");
    }

    #[test]
    fn test_tuple() {
        assert_matches!("foo", (), Err(_));
        assert_matches!("()", (), Ok(()));
        assert_matches!("()", (Symbol,), Err(_));
        assert_matches!("(x)", (Symbol,), Ok(("x",)));
        assert_matches!("()", (Symbol, Symbol), Err(_));
        assert_matches!("(x)", (Symbol, Symbol), Err(_));
        assert_matches!("(x y)", (Symbol, Symbol), Ok(("x", "y")));
        assert_matches!("(x y z)", (Symbol, Symbol), Err(_));
        assert_matches!(
            "(x \"a\" ())",
            (Symbol, String, Any),
            Ok((
                "x",
                "a",
                Sexp {
                    rep: SexpRep::List(_),
                    ..
                },
            ))
        );
        assert_eq!(<(Symbol,)>::expect(), "(<symbol>)");
        assert_eq!(<(Symbol, Symbol)>::expect(), "(<symbol> <symbol>)");
        assert_eq!(
            <(Symbol, String, Symbol)>::expect(),
            "(<symbol> <string> <symbol>)"
        );
    }

    #[test]
    fn test_tuple_with_rest() {
        assert_matches!("()", (Symbol, Rest<Symbol>), Err(_));
        assert_matches!(
            "(f)",
            (Symbol, Rest<Symbol>),
            Ok(("f", vec)) if matches!(vec.as_slice(), [])
        );
        assert_matches!(
            "(f a)",
            (Symbol, Rest<Symbol>),
            Ok(("f", vec)) if matches!(vec.as_slice(), ["a"])
        );
        assert_matches!(
            "(f a b)",
            (Symbol, Rest<Symbol>),
            Ok(("f", vec)) if matches!(vec.as_slice(), ["a", "b"])
        );
        assert_matches!("(f a b \"c\")", (Symbol, Rest<Symbol>), Err(_));
        assert_matches!(
            "(f \"a\" b 123 \"c\")",
            (Symbol, String, Rest<Any>),
            Ok(("f", "a", [_, _, _]))
        );
        assert_eq!(
            <(Symbol, Rest<Symbol>)>::expect(),
            "(<symbol> <symbol> ...)"
        );
        assert_eq!(
            <(Symbol, String, Rest<Symbol>)>::expect(),
            "(<symbol> <string> <symbol> ...)"
        );
    }

    #[test]
    fn test_either() {
        assert_matches!("()", Either<Symbol, String>, Err(_));
        assert_matches!("foo", Either<Symbol, String>, Ok(Left("foo")));
        assert_matches!("\"bar\"", Either<Symbol, String>, Ok(Right("bar")));
        assert_matches!("foo", Either<Symbol, Any>, Ok(Left("foo")));
        assert_matches!("123", Either<Symbol, Any>, Ok(Right(_)));
        assert_eq!(
            Either::<String, (Symbol,)>::expect(),
            "<<string> or (<symbol>)>"
        );
    }
}
