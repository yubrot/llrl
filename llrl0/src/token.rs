use crate::source_loc::LocationRange;
use crate::string;
use derive_new::new;
use std::fmt;

#[derive(PartialEq, PartialOrd, Debug, Clone, new)]
pub struct Token {
    pub range: LocationRange,
    pub rep: TokenRep,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.rep.fmt(f)
    }
}

#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub enum TokenRep {
    Signed(i64),
    Unsigned(u64),
    FPNumber(f64),
    Symbol(String),
    String(String),
    Char(char),

    Dot,
    Question,
    Exclamation,

    LParen,
    RParen,
    LBrack,
    RBrack,
    LBrace,
    RBrace,
    Quote,
    Backquote,
    Backslash,
    Tilde,
    Comma,
    CommaAt,
    At,
    True,
    False,

    Whitespaces,
    LineComment,

    UnterminatedString,
    UnterminatedChar,
    UnknownCharacter,

    Eof,
}

impl TokenRep {
    pub fn is_ambient(&self) -> bool {
        matches!(self, Self::Whitespaces | Self::LineComment)
    }

    pub fn is_error(&self) -> bool {
        matches!(
            self,
            Self::UnterminatedString | Self::UnterminatedChar | Self::UnknownCharacter
        )
    }

    pub fn is_eof(&self) -> bool {
        matches!(self, Self::Eof)
    }
}

impl fmt::Display for TokenRep {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Signed(v) => write!(f, "{}", v),
            Self::Unsigned(v) => write!(f, "{}", v),
            Self::FPNumber(v) => write!(f, "{}", v),
            Self::Symbol(s) => write!(f, "{}", s),
            Self::String(s) => write!(f, "\"{}\"", string::escape(s)),
            Self::Char(c) => write!(f, "#\\{}", string::escape(&c.to_string())),
            Self::Dot => write!(f, "."),
            Self::Question => write!(f, "?"),
            Self::Exclamation => write!(f, "!"),
            Self::LParen => write!(f, "("),
            Self::RParen => write!(f, ")"),
            Self::LBrack => write!(f, "["),
            Self::RBrack => write!(f, "]"),
            Self::LBrace => write!(f, "{{"),
            Self::RBrace => write!(f, "}}"),
            Self::Quote => write!(f, "'"),
            Self::Backquote => write!(f, "`"),
            Self::Backslash => write!(f, "\\"),
            Self::Tilde => write!(f, "~"),
            Self::Comma => write!(f, ","),
            Self::CommaAt => write!(f, ",@"),
            Self::At => write!(f, "@"),
            Self::True => write!(f, "#t"),
            Self::False => write!(f, "#f"),
            Self::Whitespaces => write!(f, "<spaces>"),
            Self::LineComment => write!(f, "<comment>"),
            Self::UnterminatedString => write!(f, "<unterminated string>"),
            Self::UnterminatedChar => write!(f, "<unterminated char>"),
            Self::UnknownCharacter => write!(f, "<unknown character>"),
            Self::Eof => write!(f, "<eof>"),
        }
    }
}
