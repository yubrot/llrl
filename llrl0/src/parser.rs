use crate::source_loc::{Location, LocationRange, SourceLocation, SourceLocator};
use crate::token::{Token, TokenRep};
use std::iter::Peekable;

mod error;
mod grammar;
mod primitive;
mod result;

pub use error::Error;
pub use result::{EntireResult, Result};

#[cfg(test)]
mod tests;

/// Performs parsing and return the result with the collected errors occurred during the parsing.
pub fn parse<T: Nonterminal, It: IntoIterator<Item = Token>>(
    locator: &mut SourceLocator,
    it: It,
) -> EntireResult<T::Result> {
    let mut parser = Parser::new(locator, it);
    let result = parser.parse::<T>();
    parser.produce_with_error(result)
}

/// Syntax analyzer for llrl.
pub struct Parser<'l, I>
where
    I: Iterator<Item = Token>,
{
    locator: &'l mut SourceLocator,
    lexer: Peekable<I>,
    latest_token_end: Location,
    inner_errors: Vec<Error>,
}

impl<'l, I> Parser<'l, I>
where
    I: Iterator<Item = Token>,
{
    pub fn new(
        locator: &'l mut SourceLocator,
        it: impl IntoIterator<Item = Token, IntoIter = I>,
    ) -> Self {
        Parser {
            locator,
            lexer: it.into_iter().peekable(),
            latest_token_end: Default::default(),
            inner_errors: Vec::new(),
        }
    }

    /// Peeks the next token without consuming it.
    pub fn peek_token(&mut self) -> Option<&Token> {
        while self
            .lexer
            .peek()
            .map_or(false, |token| token.rep.is_ambient())
        {
            self.lexer.next();
        }
        self.lexer.peek()
    }

    /// Obtains the next token and consumes it.
    pub fn next_token(&mut self) -> Option<Token> {
        loop {
            match self.lexer.next() {
                Some(token) if token.rep.is_ambient() => {}
                Some(token) => {
                    self.latest_token_end = token.range.end;
                    break Some(token);
                }
                None => break None,
            }
        }
    }

    /// Obtains the start location of the next token.
    pub fn next_token_start(&mut self) -> Location {
        self.peek_token()
            .map(|token| token.range.start)
            .unwrap_or(self.latest_token_end)
    }

    pub fn latest_token_end(&self) -> Location {
        self.latest_token_end
    }

    pub fn issue_source_location(&mut self, start: Location, end: Location) -> SourceLocation {
        self.locator.issue(LocationRange::new(start, end))
    }

    /// Calls a closure `parse_body` and captures the source location
    /// corresponding to the parsing performed by `parse_body`.
    pub fn capture_source_location<T, S>(
        &mut self,
        parse_body: impl FnOnce(&mut Self) -> Result<T>,
        produce_with_location: impl FnOnce(SourceLocation, T) -> S,
    ) -> Result<S> {
        let start = self.next_token_start();
        let body = parse_body(self)?;
        let end = self.latest_token_end();
        Ok(produce_with_location(
            self.issue_source_location(start, end),
            body,
        ))
    }

    /// An entry point of the parsing.
    pub fn parse<T: Nonterminal>(&mut self) -> Result<T::Result> {
        T::parse_nonterminal(self)
    }

    pub fn should_parse<T: NonterminalTry>(&mut self) -> bool {
        match self.peek_token() {
            Some(token) => T::is_leading_token(&token.rep),
            _ => false,
        }
    }

    pub fn try_parse<T: NonterminalTry>(&mut self) -> Result<Option<T::Result>> {
        Ok(if self.should_parse::<T>() {
            Some(self.parse::<T>()?)
        } else {
            None
        })
    }

    /// Performs the parsing with error recovering.
    ///
    /// If an error occurred, the parser stores the error in `inner_errors` and
    /// tries to recover from the error by consuming tokens that are not FOLLOWed by `T`.
    pub fn parse_with_error_swallowed<T: NonterminalRecover>(&mut self) -> Option<T::Result> {
        match T::parse_nonterminal(self) {
            Ok(r) => Some(r),
            Err(err) => {
                self.inner_errors.push(err);
                while match self.peek_token() {
                    Some(token) => !T::is_following_token(&token.rep),
                    None => false,
                } {
                    self.next_token();
                }
                None
            }
        }
    }

    pub fn try_parse_with_error_swallowed<T: NonterminalTry + NonterminalRecover>(
        &mut self,
    ) -> Option<Option<T::Result>> {
        Some(if self.should_parse::<T>() {
            Some(self.parse_with_error_swallowed::<T>()?)
        } else {
            None
        })
    }

    /// Produces the entire result with the errors stored in `inner_errors`.
    pub fn produce_with_error<T>(self, final_result: Result<T>) -> EntireResult<T> {
        EntireResult::new(final_result, self.inner_errors)
    }

    pub fn error_expected(&mut self, expected: impl Into<String>) -> Error {
        let unexpected = self.peek_token().cloned().unwrap_or(Token::new(
            LocationRange::empty(self.latest_token_end),
            TokenRep::Eof,
        ));

        Error::new(unexpected, expected)
    }
}

/// Types that can be used as a nonterminal symbol.
pub trait Nonterminal {
    type Result;
    fn parse_nonterminal<I: Iterator<Item = Token>>(p: &mut Parser<I>) -> Result<Self::Result>;
}

/// Nonterminal symbols that support conditional branching by one token of lookahead.
pub trait NonterminalTry: Nonterminal {
    /// Checks whether the token is acceptable by this symbol's rule.
    fn is_leading_token(token_rep: &TokenRep) -> bool;
}

/// Nonterminal symbols that support error recovering.
pub trait NonterminalRecover: Nonterminal {
    /// Checks whether the token is in the FOLLOW set of this symbol.
    fn is_following_token(token_rep: &TokenRep) -> bool;
}
