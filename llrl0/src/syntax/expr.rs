use super::{Annotatable, Name, Parameter, Parameterizable, Scheme, Use};
use crate::sexp::{self, matcher as m, MatchSlice as _, Sexp, SexpRep};
use crate::source_loc::SourceLocation;
use derive_new::new;
use either::*;
use std::borrow::Cow;
use std::marker::PhantomData;

#[derive(Debug, Clone)]
pub enum Expr<'a> {
    Begin(ExprBegin<'a>),
    Let(ExprLet<'a>),
    If(ExprIf<'a>),
    While(ExprWhile<'a>),
    Match(ExprMatch<'a>),
    Return(Option<&'a Sexp>),
    Tuple(ExprTuple<'a>),
    Unit,
    Use(Use<'a>),
    App(ExprApply<'a>),
    Quote(&'a Sexp),
    Capture(Name<'a>),
    Annotate(ExprAnnotate<'a>),
    Literal(&'a Sexp),
}

impl<'a> m::Match<'a> for Expr<'a> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<(m::Symbol, m::Rest<m::Any>)>() {
            Ok(("begin", _)) => return Ok(Self::Begin(s.matches::<ExprBegin>()?)),
            Ok(("let", _)) => return Ok(Self::Let(s.matches::<ExprLet>()?)),
            Ok(("if", _)) => return Ok(Self::If(s.matches::<ExprIf>()?)),
            Ok(("while", _)) => return Ok(Self::While(s.matches::<ExprWhile>()?)),
            Ok(("match", _)) => return Ok(Self::Match(s.matches::<ExprMatch>()?)),
            Ok(("return", _)) => return Ok(Self::Return(s.matches::<ExprReturn>()?)),
            Ok((":", _)) => return Ok(Self::Tuple(s.matches::<ExprTuple>()?)),
            Ok((sexp::QUOTE, _)) => return Ok(Self::Quote(s.matches::<ExprQuote>()?)),
            Ok((sexp::CAPTURE, _)) => return Ok(Self::Capture(s.matches::<ExprCapture>()?)),
            Ok((sexp::ANNOTATE, _)) => return Ok(Self::Annotate(s.matches::<ExprAnnotate>()?)),
            _ => {}
        }

        match s.matches::<Either<Use, ExprApply>>() {
            Ok(Left(Use::Name(name))) if name.sym == "unit" => Ok(Self::Unit),
            Ok(Left(use_)) => Ok(Self::Use(use_)),
            Ok(Right(apply)) => Ok(Self::App(apply)),
            _ => match s.rep {
                SexpRep::Integer(_, _)
                | SexpRep::FPNumber(_)
                | SexpRep::String(_)
                | SexpRep::Char(_)
                | SexpRep::Bool(_) => Ok(Self::Literal(s)),
                _ => Err(Self::error(s)),
            },
        }
    }

    fn expect() -> Cow<'static, str> {
        "<expr>".into()
    }
}

impl_default_match_slice!(['a] MatchSlice<'a> for Expr<'a>);

#[derive(Debug, Clone, new)]
pub struct ExprBegin<'a> {
    pub loc: SourceLocation,
    pub body: &'a [Sexp],
}

impl<'a> m::Match<'a> for ExprBegin<'a> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<(m::Symbol, m::Rest<m::Any>)>() {
            Ok(("begin", body)) => Ok(Self::new(s.loc, body)),
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        "(begin <body> ...)".into()
    }
}

#[derive(Debug, Clone, new)]
pub struct ExprLet<'a, Def: m::MatchSlice<'a> = m::Any> {
    pub loc: SourceLocation,
    pub defs: Def::SliceResult,
    pub body: &'a [Sexp],
}

impl<'a, Def: m::MatchSlice<'a>> m::Match<'a> for ExprLet<'a, Def> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<(m::Symbol, m::List<m::Any>, m::Rest<m::Any>)>() {
            Ok(("let", vars, body)) => {
                let defs = Def::match_sexp_slice(vars, s.loc)?;
                Ok(Self::new(s.loc, defs, body))
            }
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        "(let (<def> ...) <body> ...)".into()
    }
}

#[derive(Debug, Clone, new)]
pub struct LocalDef<'a> {
    pub loc: SourceLocation,
    pub name: Name<'a>,
    pub params: Option<Vec<Parameter<'a>>>,
    pub body: &'a [Sexp],
    pub scheme: Option<Scheme<'a>>,
}

impl<'a> m::Match<'a> for LocalDef<'a> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<(
            Annotatable<Parameterizable<Name, Parameter>, Scheme>,
            m::Rest<m::Any>,
        )>() {
            Ok((decl, body)) => Ok(Self::new(
                s.loc,
                decl.body.callee,
                decl.body.params,
                body,
                decl.annotation,
            )),
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        format!(
            "(<<name> or (<name> <param> ...)> {{{}}}? <body> ...)",
            Scheme::expect_slice()
        )
        .into()
    }
}

impl_default_match_slice!(['a] MatchSlice<'a> for LocalDef<'a>);

#[derive(Debug, Clone, new)]
pub struct ExprIf<'a> {
    pub loc: SourceLocation,
    pub cond: &'a Sexp,
    pub then: &'a Sexp,
    pub else_: &'a Sexp,
}

impl<'a> m::Match<'a> for ExprIf<'a> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<(m::Symbol, m::Any, m::Any, m::Any)>() {
            Ok(("if", cond, then, else_)) => Ok(Self::new(s.loc, cond, then, else_)),
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        "(if <cond> <then> <else>)".into()
    }
}

#[derive(Debug, Clone, new)]
pub struct ExprWhile<'a> {
    pub loc: SourceLocation,
    pub cond: &'a Sexp,
    pub body: &'a [Sexp],
}

impl<'a> m::Match<'a> for ExprWhile<'a> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<(m::Symbol, m::Any, m::Rest<m::Any>)>() {
            Ok(("while", cond, body)) => Ok(Self::new(s.loc, cond, body)),
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        "(while <cond> <body> ...)".into()
    }
}

#[derive(Debug, Clone, new)]
pub struct ExprMatch<'a, Clause: m::MatchSlice<'a> = m::Any> {
    pub loc: SourceLocation,
    pub target: &'a Sexp,
    pub clauses: Clause::SliceResult,
}

impl<'a, Clause: m::MatchSlice<'a>> m::Match<'a> for ExprMatch<'a, Clause> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<(m::Symbol, m::Any, m::Rest<m::Any>)>() {
            Ok(("match", target, clauses)) => {
                let clauses = Clause::match_sexp_slice(clauses, s.loc)?;
                Ok(Self::new(s.loc, target, clauses))
            }
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        "(match <target> <clause> ...)".into()
    }
}

#[derive(Debug, Clone, new)]
pub struct MatchClause<'a> {
    pub loc: SourceLocation,
    pub pat: &'a Sexp,
    pub body: &'a [Sexp],
}

impl<'a> m::Match<'a> for MatchClause<'a> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<(m::Any, m::Rest<m::Any>)>() {
            Ok((pat, body)) => Ok(Self::new(s.loc, pat, body)),
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        "(<pattern> <body> ...)".into()
    }
}

impl_default_match_slice!(['a] MatchSlice<'a> for MatchClause<'a>);

#[derive(Debug)]
pub struct ExprReturn<'a> {
    _marker: PhantomData<&'a Sexp>,
}

impl<'a> m::Match<'a> for ExprReturn<'a> {
    type Result = Option<&'a Sexp>;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<Either<(m::Symbol,), (m::Symbol, m::Any)>>() {
            Ok(Left(("return",))) => Ok(None),
            Ok(Right(("return", a))) => Ok(Some(a)),
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        "(return <expr>?)".into()
    }
}

#[derive(Debug, Clone, new)]
pub struct ExprTuple<'a> {
    pub loc: SourceLocation,
    pub exprs: &'a [Sexp],
}

impl<'a> m::Match<'a> for ExprTuple<'a> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<(m::Symbol, m::Rest<m::Any>)>() {
            Ok((":", exprs)) => Ok(Self::new(s.loc, exprs)),
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        "(: <expr> ...)".into()
    }
}

#[derive(Debug, Clone, new)]
pub struct ExprApply<'a> {
    pub loc: SourceLocation,
    pub callee: &'a Sexp,
    pub args: &'a [Sexp],
}

impl<'a> m::Match<'a> for ExprApply<'a> {
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

#[derive(Debug)]
pub struct ExprQuote<'a> {
    _marker: PhantomData<&'a Sexp>,
}

impl<'a> m::Match<'a> for ExprQuote<'a> {
    type Result = &'a Sexp;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<(m::Symbol, m::Any)>() {
            Ok((sexp::QUOTE, expr)) => Ok(expr),
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        "'<expr>".into()
    }
}

#[derive(Debug)]
pub struct ExprCapture<'a> {
    _marker: PhantomData<&'a str>,
}

impl<'a> m::Match<'a> for ExprCapture<'a> {
    type Result = Name<'a>;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<(m::Symbol, Name)>() {
            Ok((sexp::CAPTURE, name)) => Ok(name),
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        "\\symbol".into()
    }
}

#[derive(Debug, Clone, new)]
pub struct ExprAnnotate<'a> {
    pub loc: SourceLocation,
    pub expr: &'a Sexp,
    pub scheme: Scheme<'a>,
}

impl<'a> m::Match<'a> for ExprAnnotate<'a> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<(m::Symbol, m::Any, m::Rest<Scheme>)>() {
            Ok((sexp::ANNOTATE, expr, scheme)) => Ok(Self::new(s.loc, expr, scheme)),
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        "<expr> {<type>}".into()
    }
}
