use crate::sexp::{Sexp, SexpRep, Ss};
use crate::source_loc::SourceLocation;
use if_chain::if_chain;
use itertools::Itertools;
use smallvec::{smallvec, SmallVec};
use std::collections::{HashMap, HashSet};

mod error;

#[cfg(test)]
mod tests;

pub use error::Error;

const META_PREFIX: char = '$';

/// A preprocessor that processes directives in S-expressions.
/// The preprocessor is executed before semantic analysis.
#[derive(Debug, Clone)]
pub struct Preprocessor {
    features: HashSet<String>,
}

impl Preprocessor {
    pub fn new() -> Self {
        Self {
            features: HashSet::new(),
        }
    }

    pub fn enable_feature(&mut self, feature: impl Into<String>) {
        self.features.insert(feature.into());
    }

    pub fn run(&self, ss: &mut Ss) -> Result<(), Error> {
        let mut state = State {
            pp: self,
            vars: HashMap::new(),
        };
        let new_ss = state.expand_seq(std::mem::take(&mut ss.ss))?;
        ss.ss = new_ss;
        Ok(())
    }
}

impl Default for Preprocessor {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
struct State<'a> {
    pp: &'a Preprocessor,
    vars: HashMap<String, Sexp>,
}

impl<'a> State<'a> {
    fn expand_seq<I: IntoIterator<Item = Sexp>, O: FromIterator<Sexp>>(
        &mut self,
        ss: I,
    ) -> Result<O, Error> {
        ss.into_iter()
            .map(|s| self.expand(s))
            .flatten_ok()
            .collect::<Result<O, _>>()
    }

    fn expand_one(&mut self, s: Sexp, place: &'static str) -> Result<Sexp, Error> {
        let loc = s.loc;
        let mut s = self.expand(s)?;
        if s.len() == 1 {
            Ok(s.swap_remove(0))
        } else {
            Err(Error::ExpansionDisallowed(loc, place))
        }
    }

    fn expand(&mut self, s: Sexp) -> Result<SmallVec<[Sexp; 1]>, Error> {
        let loc = s.loc;
        match s.rep {
            SexpRep::Symbol(sym) if sym.starts_with(META_PREFIX) => {
                if let Some(s) = self.vars.get(sym.as_ref()) {
                    Ok(smallvec![s.clone()])
                } else {
                    Err(Error::UndefinedMetaVariable(s.loc, sym.to_string()))
                }
            }
            SexpRep::List(ss) if !ss.is_empty() => if_chain! {
                if let SexpRep::Symbol(ref s) = ss[0].rep;
                if let Ok(directive) = Directive::try_from(s.as_ref());

                then {
                    self.handle_directive(directive, loc, ss)
                } else {
                    Ok(smallvec![Sexp::list(loc, self.expand_seq(ss)?)])
                }
            },
            SexpRep::ListLike(ll) => {
                let ss: Vec<_> = self.expand_seq(std::iter::once(ll.0).chain(ll.1))?;
                let tail = self.expand_one(ll.2, "cdr of cons cell")?;
                Ok(smallvec![ss
                    .into_iter()
                    .rev()
                    .fold(tail, |a, b| Sexp::cons(loc, b, a))])
            }
            _ => Ok(smallvec![s]),
        }
    }

    fn handle_directive(
        &mut self,
        directive: Directive,
        loc: SourceLocation,
        ss: Vec<Sexp>,
    ) -> Result<SmallVec<[Sexp; 1]>, Error> {
        match directive {
            Directive::Symbol => self.handle_symbol_directive(loc, ss),
            Directive::Let => self.handle_let_directive(loc, ss),
            Directive::Let1 => self.handle_let1_directive(loc, ss),
            Directive::For => self.handle_for_directive(loc, ss),
            Directive::For1 => self.handle_for1_directive(loc, ss),
            Directive::When => self.handle_when_directive(loc, ss),
            Directive::Not => self.handle_not_directive(loc, ss),
            Directive::Feature => self.handle_feature_directive(loc, ss),
        }
    }

    fn handle_symbol_directive(
        &mut self,
        loc: SourceLocation,
        ss: Vec<Sexp>,
    ) -> Result<SmallVec<[Sexp; 1]>, Error> {
        let ss: Vec<_> = self.expand_seq(ss.into_iter().skip(1))?;
        let mut name = String::new();
        for s in ss {
            match s.rep {
                SexpRep::Symbol(ref s) => name.push_str(s),
                SexpRep::String(ref s) => name.push_str(s),
                SexpRep::Char(c) => name.push(c),
                _ => Err(Error::DirectiveSyntax(
                    s.loc,
                    "($symbol <symbol or string or char>...)",
                ))?,
            }
        }

        if name.is_empty() {
            Err(Error::EmptySymbol(loc))?;
        }
        Ok(smallvec![Sexp::symbol(loc, name)])
    }

    fn handle_let_directive(
        &mut self,
        loc: SourceLocation,
        mut ss: Vec<Sexp>,
    ) -> Result<SmallVec<[Sexp; 1]>, Error> {
        let binds = if_chain! {
            if 2 <= ss.len();
            if let SexpRep::List(binds) = ss.remove(1).rep;
            if let Some(binds) = binds
                .into_iter()
                .map(|bind| Ok(if_chain! {
                    if let SexpRep::List(mut ss) = bind.rep;
                    if ss.len() == 2;
                    if let SexpRep::Symbol(name) = ss.remove(0).rep;
                    if metavar(&name);
                    let expr = self.expand_one(ss.remove(0), "$let bind expression")?;

                    then { Some((name.to_string(), expr)) }
                    else { None }
                }))
                .collect::<Result<Option<Vec<_>>, _>>()?;

            then { Ok(binds) }
            else { Err(Error::DirectiveSyntax(loc, "($let ([$var <expr>]...) <body>...)")) }
        }?;

        self.with_binds(binds, |self_| self_.expand_seq(ss.iter().skip(1).cloned()))
    }

    fn handle_let1_directive(
        &mut self,
        loc: SourceLocation,
        mut ss: Vec<Sexp>,
    ) -> Result<SmallVec<[Sexp; 1]>, Error> {
        let name = if_chain! {
            if 3 <= ss.len();
            if let SexpRep::Symbol(name) = ss.remove(1).rep;
            if metavar(&name);

            then { Ok(name) }
            else { Err(Error::DirectiveSyntax(loc, "($let1 $var <expr> <body>...)")) }
        }?;
        let expr = self.expand_one(ss.remove(1), "$let1 expression")?;

        self.with_binds(std::iter::once((name.to_string(), expr)), |self_| {
            self_.expand_seq(ss.iter().skip(1).cloned())
        })
    }

    fn handle_for_directive(
        &mut self,
        loc: SourceLocation,
        mut ss: Vec<Sexp>,
    ) -> Result<SmallVec<[Sexp; 1]>, Error> {
        let (names, elemss) = if_chain! {
            if 3 <= ss.len();
            if let SexpRep::List(names) = ss.remove(1).rep;
            if let SexpRep::List(elemss) = self.expand_one(ss.remove(1), "$for elements")?.rep;
            if let Some(names) = names
                .into_iter()
                .map(|name| match name.rep {
                    SexpRep::Symbol(s) if metavar(s.as_ref()) => Some(s.to_string()),
                    _ => None,
                })
                .collect::<Option<Vec<_>>>();
            if let Some(elemss) = elemss
                .into_iter()
                .map(|elems| match elems.rep {
                    SexpRep::List(elems) if elems.len() == names.len() => Some(elems),
                    _ => None,
                })
                .collect::<Option<Vec<_>>>();

            then { Ok((names, elemss)) }
            else { Err(Error::DirectiveSyntax(loc, "($for [$var...] ([<elem>...]...) <body>...)")) }
        }?;

        let mut result = SmallVec::new();
        for elems in elemss {
            let mut ss: SmallVec<[_; 1]> = self
                .with_binds(names.iter().cloned().zip(elems), |self_| {
                    self_.expand_seq(ss.iter().skip(1).cloned())
                })?;
            result.append(&mut ss);
        }
        Ok(result)
    }

    fn handle_for1_directive(
        &mut self,
        loc: SourceLocation,
        mut ss: Vec<Sexp>,
    ) -> Result<SmallVec<[Sexp; 1]>, Error> {
        let (name, elems) = if_chain! {
            if 3 <= ss.len();
            if let SexpRep::Symbol(name) = ss.remove(1).rep;
            if metavar(&name);
            if let SexpRep::List(elems) = self.expand_one(ss.remove(1), "$for1 elements")?.rep;

            then { Ok((name, elems)) }
            else { Err(Error::DirectiveSyntax(loc, "($for1 $var (<elem>...) <body>...)")) }
        }?;

        let mut result = SmallVec::new();
        for elem in elems {
            let mut ss: SmallVec<[_; 1]> = self
                .with_binds(std::iter::once((name.to_string(), elem)), |self_| {
                    self_.expand_seq(ss.iter().skip(1).cloned())
                })?;
            result.append(&mut ss);
        }
        Ok(result)
    }

    fn handle_when_directive(
        &mut self,
        loc: SourceLocation,
        mut ss: Vec<Sexp>,
    ) -> Result<SmallVec<[Sexp; 1]>, Error> {
        let value = if_chain! {
            if 2 <= ss.len();
            if let SexpRep::Bool(value) = self.expand_one(ss.remove(1), "$when condition")?.rep;

            then { Ok(value) }
            else { Err(Error::DirectiveSyntax(loc, "($when <bool> <body>...)")) }
        }?;

        if value {
            Ok(self.expand_seq(ss.into_iter().skip(1))?)
        } else {
            Ok(SmallVec::new())
        }
    }

    fn handle_not_directive(
        &mut self,
        loc: SourceLocation,
        mut ss: Vec<Sexp>,
    ) -> Result<SmallVec<[Sexp; 1]>, Error> {
        let value = if_chain! {
            if ss.len() == 2;
            if let SexpRep::Bool(value) = self.expand_one(ss.remove(1), "$not condition")?.rep;

            then { Ok(value) }
            else { Err(Error::DirectiveSyntax(loc, "($not <bool>)")) }
        }?;

        Ok(smallvec![Sexp::bool(loc, !value)])
    }

    fn handle_feature_directive(
        &mut self,
        loc: SourceLocation,
        mut ss: Vec<Sexp>,
    ) -> Result<SmallVec<[Sexp; 1]>, Error> {
        let feature = if_chain! {
            if ss.len() == 2;
            if let SexpRep::String(s) = self.expand_one(ss.remove(1), "$feature string")?.rep;

            then { Ok(s) }
            else { Err(Error::DirectiveSyntax(loc, "($feature <string>)")) }
        }?;

        let value = self.pp.features.contains(feature.as_ref());
        Ok(smallvec![Sexp::bool(loc, value)])
    }

    fn with_binds<T>(
        &mut self,
        binds: impl IntoIterator<Item = (String, Sexp)>,
        f: impl FnOnce(&mut Self) -> Result<T, Error>,
    ) -> Result<T, Error> {
        let prev_binds = binds
            .into_iter()
            .filter(|(name, _)| name != "_")
            .map(|(name, expr)| (name.clone(), self.vars.insert(name, expr)))
            .collect::<Vec<_>>();

        let result = f(self);

        for (name, prev_expr) in prev_binds {
            if let Some(prev_expr) = prev_expr {
                self.vars.insert(name, prev_expr);
            } else {
                self.vars.remove(name.as_str());
            }
        }

        result
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
enum Directive {
    Symbol,
    Let,
    Let1,
    For,
    For1,
    When,
    Not,
    Feature,
}

impl TryFrom<&'_ str> for Directive {
    type Error = ();

    fn try_from(value: &'_ str) -> Result<Self, Self::Error> {
        match value {
            "$symbol" => Ok(Directive::Symbol),
            "$let" => Ok(Directive::Let),
            "$let1" => Ok(Directive::Let1),
            "$for" => Ok(Directive::For),
            "$for1" => Ok(Directive::For1),
            "$when" => Ok(Directive::When),
            "$not" => Ok(Directive::Not),
            "$feature" => Ok(Directive::Feature),
            _ => Err(()),
        }
    }
}

fn metavar(sym: &str) -> bool {
    sym.starts_with(META_PREFIX) || sym == "_"
}
