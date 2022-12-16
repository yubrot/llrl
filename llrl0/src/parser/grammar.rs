use super::{primitive as prim, Nonterminal, NonterminalRecover, NonterminalTry, Parser, Result};
use crate::sexp::{self, Sexp, SexpRep, Ss};
use crate::token::{Token, TokenRep};
use std::marker::PhantomData;

impl Nonterminal for Ss {
    type Result = Ss;

    fn parse_nonterminal<I: Iterator<Item = Token>>(p: &mut Parser<I>) -> Result<Self::Result> {
        p.capture_source_location(
            |p| {
                let ss = p.parse::<prim::RecoverableSeq<Sexp>>()?;
                p.parse::<prim::Eof>()?;
                Ok(ss)
            },
            Ss::new,
        )
    }
}

impl Nonterminal for Sexp {
    type Result = Sexp;

    fn parse_nonterminal<I: Iterator<Item = Token>>(p: &mut Parser<I>) -> Result<Self::Result> {
        let start = p.next_token_start();

        let mut s = p.capture_source_location(
            |p| {
                if let Some(s) = p.try_parse::<SexpInteger>()? {
                    Ok(s)
                } else if let Some(s) = p.try_parse::<SexpFPNumber>()? {
                    Ok(s)
                } else if let Some(s) = p.try_parse::<SexpSymbol>()? {
                    Ok(s)
                } else if let Some(s) = p.try_parse::<SexpString>()? {
                    Ok(s)
                } else if let Some(s) = p.try_parse::<SexpChar>()? {
                    Ok(s)
                } else if let Some(s) = p.try_parse::<SexpListLike<prim::LParen, prim::RParen>>()? {
                    Ok(s)
                } else if let Some(s) = p.try_parse::<SexpListLike<prim::LBrack, prim::RBrack>>()? {
                    Ok(s)
                } else if let Some(s) = p.try_parse::<SexpQuoted<SexpQuote>>()? {
                    Ok(s)
                } else if let Some(s) = p.try_parse::<SexpQuoted<SexpQuasiquote>>()? {
                    Ok(s)
                } else if let Some(s) = p.try_parse::<SexpQuoted<SexpUnquote>>()? {
                    Ok(s)
                } else if let Some(s) = p.try_parse::<SexpQuoted<SexpUnquoteSplicing>>()? {
                    Ok(s)
                } else if let Some(s) = p.try_parse::<SexpQuoted<SexpCapture>>()? {
                    Ok(s)
                } else if let Some(s) = p.try_parse::<SexpQuoted<SexpLoad>>()? {
                    Ok(s)
                } else if let Some(s) = p.try_parse::<SexpBool>()? {
                    Ok(s)
                } else {
                    Err(p.error_expected("S-expression"))
                }
            },
            Sexp::new,
        )?;

        while {
            if p.try_parse::<prim::Question>()?.is_some() {
                let loc = p.issue_source_location(start, p.latest_token_end);
                s = Sexp::try_question(loc, s);
                true
            } else if p.try_parse::<prim::Exclamation>()?.is_some() {
                let loc = p.issue_source_location(start, p.latest_token_end);
                s = Sexp::try_exclamation(loc, s);
                true
            } else if p.try_parse::<prim::LBrace>()?.is_some() {
                let ss = p.parse::<prim::RecoverableSeq<Sexp>>()?;
                p.parse::<prim::RBrace>()?;
                let loc = p.issue_source_location(start, p.latest_token_end);
                s = Sexp::annotate(loc, s, ss);
                true
            } else {
                false
            }
        } {}

        Ok(s)
    }
}

impl NonterminalTry for Sexp {
    fn is_leading_token(token_rep: &TokenRep) -> bool {
        !matches!(
            token_rep,
            TokenRep::Dot
                | TokenRep::At
                | TokenRep::Question
                | TokenRep::Exclamation
                | TokenRep::RParen
                | TokenRep::RBrack
                | TokenRep::RBrace
        )
    }
}

impl NonterminalRecover for Sexp {
    fn is_following_token(token_rep: &TokenRep) -> bool {
        !token_rep.is_ambient()
            && !token_rep.is_error()
            && !matches!(
                token_rep,
                TokenRep::LBrace | TokenRep::Question | TokenRep::Exclamation
            )
    }
}

pub struct SexpInteger;

impl Nonterminal for SexpInteger {
    type Result = SexpRep;

    fn parse_nonterminal<I: Iterator<Item = Token>>(p: &mut Parser<I>) -> Result<Self::Result> {
        if let Some(value) = p.try_parse::<prim::Signed>()? {
            Ok(SexpRep::signed(value))
        } else if let Some(value) = p.try_parse::<prim::Unsigned>()? {
            Ok(SexpRep::unsigned(value))
        } else {
            Err(p.error_expected("integer"))
        }
    }
}

impl NonterminalTry for SexpInteger {
    fn is_leading_token(token_rep: &TokenRep) -> bool {
        prim::Signed::is_leading_token(token_rep) || prim::Unsigned::is_leading_token(token_rep)
    }
}

pub struct SexpFPNumber;

impl Nonterminal for SexpFPNumber {
    type Result = SexpRep;

    fn parse_nonterminal<I: Iterator<Item = Token>>(p: &mut Parser<I>) -> Result<Self::Result> {
        let value = p.parse::<prim::FPNumber>()?;
        Ok(SexpRep::FPNumber(value.into()))
    }
}

impl NonterminalTry for SexpFPNumber {
    fn is_leading_token(token_rep: &TokenRep) -> bool {
        prim::FPNumber::is_leading_token(token_rep)
    }
}

pub struct SexpSymbol;

impl Nonterminal for SexpSymbol {
    type Result = SexpRep;

    fn parse_nonterminal<I: Iterator<Item = Token>>(p: &mut Parser<I>) -> Result<Self::Result> {
        let value = p.parse::<prim::Symbol>()?;
        Ok(SexpRep::symbol(value))
    }
}

impl NonterminalTry for SexpSymbol {
    fn is_leading_token(token_rep: &TokenRep) -> bool {
        prim::Symbol::is_leading_token(token_rep)
    }
}

pub struct SexpString;

impl Nonterminal for SexpString {
    type Result = SexpRep;

    fn parse_nonterminal<I: Iterator<Item = Token>>(p: &mut Parser<I>) -> Result<Self::Result> {
        let value = p.parse::<prim::String>()?;
        Ok(SexpRep::string(value))
    }
}

impl NonterminalTry for SexpString {
    fn is_leading_token(token_rep: &TokenRep) -> bool {
        prim::String::is_leading_token(token_rep)
    }
}

pub struct SexpChar;

impl Nonterminal for SexpChar {
    type Result = SexpRep;

    fn parse_nonterminal<I: Iterator<Item = Token>>(p: &mut Parser<I>) -> Result<Self::Result> {
        let value = p.parse::<prim::Char>()?;
        Ok(SexpRep::Char(value))
    }
}

impl NonterminalTry for SexpChar {
    fn is_leading_token(token_rep: &TokenRep) -> bool {
        prim::Char::is_leading_token(token_rep)
    }
}

pub struct SexpListLike<L, R> {
    _l: PhantomData<L>,
    _r: PhantomData<R>,
}

impl<L: Nonterminal, R: Nonterminal> Nonterminal for SexpListLike<L, R> {
    type Result = SexpRep;

    fn parse_nonterminal<I: Iterator<Item = Token>>(p: &mut Parser<I>) -> Result<Self::Result> {
        p.parse::<L>()?;
        let result = p.parse::<SexpListLikeBody>()?;
        p.parse::<R>()?;
        Ok(result)
    }
}

impl<L: NonterminalTry, R: Nonterminal> NonterminalTry for SexpListLike<L, R> {
    fn is_leading_token(token_rep: &TokenRep) -> bool {
        L::is_leading_token(token_rep)
    }
}

pub struct SexpListLikeBody;

impl Nonterminal for SexpListLikeBody {
    type Result = SexpRep;

    fn parse_nonterminal<I: Iterator<Item = Token>>(p: &mut Parser<I>) -> Result<Self::Result> {
        let mut items = p.parse::<prim::RecoverableSeq<Sexp>>()?;
        Ok(if p.try_parse::<prim::At>()?.is_some() {
            items.push(p.capture_source_location(|p| p.parse::<SexpListLikeBody>(), Sexp::new)?);
            SexpRep::List(items)
        } else if p.try_parse::<prim::Dot>()?.is_some() {
            let last = p.parse::<Sexp>()?;
            if items.is_empty() {
                last.rep
            } else {
                let head = items.remove(0);
                SexpRep::list_like(head, items, last)
            }
        } else {
            SexpRep::List(items)
        })
    }
}

pub struct SexpQuoted<Q> {
    _q: PhantomData<Q>,
}

impl<Q: Nonterminal<Result = SexpRep>> Nonterminal for SexpQuoted<Q> {
    type Result = SexpRep;

    fn parse_nonterminal<I: Iterator<Item = Token>>(p: &mut Parser<I>) -> Result<Self::Result> {
        let q = p.capture_source_location(|p| p.parse::<Q>(), Sexp::new)?;
        let s = p.parse::<Sexp>()?;
        Ok(SexpRep::List(vec![q, s]))
    }
}

impl<Q: NonterminalTry<Result = SexpRep>> NonterminalTry for SexpQuoted<Q> {
    fn is_leading_token(token_rep: &TokenRep) -> bool {
        Q::is_leading_token(token_rep)
    }
}

pub struct SexpBool;

impl Nonterminal for SexpBool {
    type Result = SexpRep;

    fn parse_nonterminal<I: Iterator<Item = Token>>(p: &mut Parser<I>) -> Result<Self::Result> {
        if p.try_parse::<prim::True>()?.is_some() {
            Ok(SexpRep::Bool(true))
        } else if p.try_parse::<prim::False>()?.is_some() {
            Ok(SexpRep::Bool(false))
        } else {
            Err(p.error_expected("boolean"))
        }
    }
}

impl NonterminalTry for SexpBool {
    fn is_leading_token(token_rep: &TokenRep) -> bool {
        prim::True::is_leading_token(token_rep) || prim::False::is_leading_token(token_rep)
    }
}

pub struct SexpQuote;

impl Nonterminal for SexpQuote {
    type Result = SexpRep;

    fn parse_nonterminal<I: Iterator<Item = Token>>(p: &mut Parser<I>) -> Result<Self::Result> {
        p.parse::<prim::Quote>()?;
        Ok(SexpRep::symbol(sexp::QUOTE))
    }
}

impl NonterminalTry for SexpQuote {
    fn is_leading_token(token_rep: &TokenRep) -> bool {
        prim::Quote::is_leading_token(token_rep)
    }
}

pub struct SexpQuasiquote;

impl Nonterminal for SexpQuasiquote {
    type Result = SexpRep;

    fn parse_nonterminal<I: Iterator<Item = Token>>(p: &mut Parser<I>) -> Result<Self::Result> {
        p.parse::<prim::Backquote>()?;
        Ok(SexpRep::symbol(sexp::QUASIQUOTE))
    }
}

impl NonterminalTry for SexpQuasiquote {
    fn is_leading_token(token_rep: &TokenRep) -> bool {
        prim::Backquote::is_leading_token(token_rep)
    }
}

pub struct SexpUnquote;

impl Nonterminal for SexpUnquote {
    type Result = SexpRep;

    fn parse_nonterminal<I: Iterator<Item = Token>>(p: &mut Parser<I>) -> Result<Self::Result> {
        p.parse::<prim::Comma>()?;
        Ok(SexpRep::symbol(sexp::UNQUOTE))
    }
}

impl NonterminalTry for SexpUnquote {
    fn is_leading_token(token_rep: &TokenRep) -> bool {
        prim::Comma::is_leading_token(token_rep)
    }
}

pub struct SexpUnquoteSplicing;

impl Nonterminal for SexpUnquoteSplicing {
    type Result = SexpRep;

    fn parse_nonterminal<I: Iterator<Item = Token>>(p: &mut Parser<I>) -> Result<Self::Result> {
        p.parse::<prim::CommaAt>()?;
        Ok(SexpRep::symbol(sexp::UNQUOTE_SPLICING))
    }
}

impl NonterminalTry for SexpUnquoteSplicing {
    fn is_leading_token(token_rep: &TokenRep) -> bool {
        prim::CommaAt::is_leading_token(token_rep)
    }
}

pub struct SexpCapture;

impl Nonterminal for SexpCapture {
    type Result = SexpRep;

    fn parse_nonterminal<I: Iterator<Item = Token>>(p: &mut Parser<I>) -> Result<Self::Result> {
        p.parse::<prim::Backslash>()?;
        Ok(SexpRep::symbol(sexp::CAPTURE))
    }
}

impl NonterminalTry for SexpCapture {
    fn is_leading_token(token_rep: &TokenRep) -> bool {
        prim::Backslash::is_leading_token(token_rep)
    }
}

pub struct SexpLoad;

impl Nonterminal for SexpLoad {
    type Result = SexpRep;

    fn parse_nonterminal<I: Iterator<Item = Token>>(p: &mut Parser<I>) -> Result<Self::Result> {
        p.parse::<prim::Tilde>()?;
        Ok(SexpRep::symbol(sexp::LOAD))
    }
}

impl NonterminalTry for SexpLoad {
    fn is_leading_token(token_rep: &TokenRep) -> bool {
        prim::Tilde::is_leading_token(token_rep)
    }
}
