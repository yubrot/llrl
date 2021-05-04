use super::{Nonterminal, NonterminalRecover, NonterminalTry, Parser, Result};
use crate::sexp::{Token, TokenRep};
use std::marker::PhantomData;

pub struct Eof;

impl Nonterminal for Eof {
    type Result = ();

    fn parse_nonterminal<I: Iterator<Item = Token>>(p: &mut Parser<I>) -> Result<Self::Result> {
        match p.peek_token() {
            None => Ok(()),
            Some(_) => Err(p.error_expected("eof")),
        }
    }
}

impl Nonterminal for () {
    type Result = ();

    fn parse_nonterminal<I: Iterator<Item = Token>>(_p: &mut Parser<I>) -> Result<Self::Result> {
        Ok(())
    }
}

impl NonterminalTry for () {
    fn is_leading_token(_token_rep: &TokenRep) -> bool {
        true
    }
}

macro_rules! impl_tuple_nonterminal {
    ($a:ident: $at:ident, $($b:ident: $bt:ident),*) => {
        impl<$at: Nonterminal, $($bt: Nonterminal),*> Nonterminal for ($at, $($bt),*) {
            type Result = ($at::Result, $($bt::Result),*);

            fn parse_nonterminal<I: Iterator<Item = Token>>(p: &mut Parser<I>) -> Result<Self::Result> {
                let $a = p.parse::<$at>()?;
                $( let $b = p.parse::<$bt>()?; )*
                Ok(($a, $($b),*))
            }
        }

        impl<$at: NonterminalTry, $($bt: Nonterminal),*> NonterminalTry for ($at, $($bt),*) {
            fn is_leading_token(token_rep: &TokenRep) -> bool {
                $at::is_leading_token(token_rep)
            }
        }
    };
}

impl_tuple_nonterminal!(a: A,);
impl_tuple_nonterminal!(a: A, b: B);
impl_tuple_nonterminal!(a: A, b: B, c: C);
impl_tuple_nonterminal!(a: A, b: B, c: C, d: D);
impl_tuple_nonterminal!(a: A, b: B, c: C, d: D, e: E);
impl_tuple_nonterminal!(a: A, b: B, c: C, d: D, e: E, f: F);

impl<T: NonterminalTry> Nonterminal for Option<T> {
    type Result = Option<T::Result>;

    fn parse_nonterminal<I: Iterator<Item = Token>>(p: &mut Parser<I>) -> Result<Self::Result> {
        p.try_parse::<T>()
    }
}

impl<T: NonterminalTry> NonterminalTry for Option<T> {
    fn is_leading_token(_token_rep: &TokenRep) -> bool {
        true
    }
}

impl<T: NonterminalTry + NonterminalRecover> NonterminalRecover for Option<T> {
    fn is_following_token(token_rep: &TokenRep) -> bool {
        T::is_following_token(token_rep)
    }
}

pub struct Seq<Element, Separator = ()> {
    _element: PhantomData<Element>,
    _separator: PhantomData<Separator>,
}

impl<Element: NonterminalTry, Separator: NonterminalTry> Nonterminal for Seq<Element, Separator> {
    type Result = Vec<Element::Result>;

    fn parse_nonterminal<I: Iterator<Item = Token>>(p: &mut Parser<I>) -> Result<Self::Result> {
        let mut r = Vec::new();
        while p.should_parse::<Element>() {
            r.push(p.parse::<Element>()?);

            if p.try_parse::<Separator>()?.is_none() {
                break;
            }
        }
        Ok(r)
    }
}

pub struct RecoverableSeq<Element, Separator = ()> {
    _element: PhantomData<Element>,
    _separator: PhantomData<Separator>,
}

impl<Element: NonterminalTry + NonterminalRecover, Separator: NonterminalTry> Nonterminal
    for RecoverableSeq<Element, Separator>
{
    type Result = Vec<Element::Result>;

    fn parse_nonterminal<I: Iterator<Item = Token>>(p: &mut Parser<I>) -> Result<Self::Result> {
        let mut r = Vec::new();
        while p.should_parse::<Element>() {
            if let Some(a) = p.parse_with_error_swallowed::<Element>() {
                r.push(a);
            }

            if p.try_parse::<Separator>()?.is_none() {
                break;
            }
        }
        Ok(r)
    }
}

macro_rules! impl_token_nonterminal {
    ($name:ident$(($var:ident: $ty:ty))?, $rep:path, $msg:tt) => {
        pub struct $name;

        #[allow(unused_parens)]
        #[allow(unused_variables)]
        impl Nonterminal for $name {
            type Result = ($($ty)?);

            fn parse_nonterminal<I: Iterator<Item = Token>>(p: &mut Parser<I>) -> Result<Self::Result> {
                match p.peek_token().map(|t| &t.rep) {
                    Some($rep$(($var))?) => match p.next_token().map(|t| t.rep) {
                        Some($rep$(($var))?) => Ok(($($var)?)),
                        _ => panic!("peek_token and next_token do not match"),
                    },
                    _ => Err(p.error_expected($msg)),
                }
            }
        }

        #[allow(unused_variables)]
        impl NonterminalTry for $name {
            fn is_leading_token(token_rep: &TokenRep) -> bool {
                matches!(token_rep, $rep$(($var))?)
            }
        }
    };
}

impl_token_nonterminal!(Signed(n: i64), TokenRep::Signed, "signed integer");
impl_token_nonterminal!(Unsigned(n: u64), TokenRep::Unsigned, "unsigned integer");
impl_token_nonterminal!(FPNumber(n: f64), TokenRep::FPNumber, "number");
impl_token_nonterminal!(Symbol(s: std::string::String), TokenRep::Symbol, "symbol");
impl_token_nonterminal!(String(s: std::string::String), TokenRep::String, "string");
impl_token_nonterminal!(Char(c: char), TokenRep::Char, "char");

impl_token_nonterminal!(Dot, TokenRep::Dot, ".");
impl_token_nonterminal!(Question, TokenRep::Question, "?");
impl_token_nonterminal!(Exclamation, TokenRep::Exclamation, "!");

impl_token_nonterminal!(LParen, TokenRep::LParen, "(");
impl_token_nonterminal!(RParen, TokenRep::RParen, ")");
impl_token_nonterminal!(LBrack, TokenRep::LBrack, "[");
impl_token_nonterminal!(RBrack, TokenRep::RBrack, "]");
impl_token_nonterminal!(LBrace, TokenRep::LBrace, "{");
impl_token_nonterminal!(RBrace, TokenRep::RBrace, "}");
impl_token_nonterminal!(Quote, TokenRep::Quote, "'");
impl_token_nonterminal!(Backquote, TokenRep::Backquote, "`");
impl_token_nonterminal!(Backslash, TokenRep::Backslash, "\\");
impl_token_nonterminal!(Tilde, TokenRep::Tilde, "~");
impl_token_nonterminal!(Comma, TokenRep::Comma, ",");
impl_token_nonterminal!(CommaAt, TokenRep::CommaAt, ",@");
impl_token_nonterminal!(True, TokenRep::True, "#t");
impl_token_nonterminal!(False, TokenRep::False, "#f");
