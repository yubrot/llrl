use crate::sexp::{Token, TokenRep};
use crate::source_loc::{Location, LocationRange};
use crate::string;

/// Produces a sequence of tokens.
pub fn lex(input: &str) -> Vec<Token> {
    Lexer::new(input).collect()
}

/// Lexical analyzer for llrl.
pub struct Lexer<'a> {
    input: &'a str,
    location: Location,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            input,
            location: Location::new(),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        let mut e = Eat::new(self);
        if e.is_eof() {
            None
        } else if e.eat_whitespaces() {
            Some(e.produce(TokenRep::Whitespaces))
        } else if e.eat_line_comment() {
            Some(e.produce(TokenRep::LineComment))
        } else if e.eat_id() {
            Some(e.produce_with(|s| {
                if let Ok(num) = s.parse::<u64>() {
                    TokenRep::Unsigned(num)
                } else if let Ok(num) = s.parse::<i64>() {
                    TokenRep::Signed(num)
                } else if let Ok(num) = s.parse::<f64>() {
                    TokenRep::FPNumber(num)
                } else if s == "." {
                    TokenRep::Dot
                } else if s == "?" {
                    TokenRep::Question
                } else if s == "!" {
                    TokenRep::Exclamation
                } else {
                    TokenRep::Symbol(s.to_string())
                }
            }))
        } else if let Some(terminated) = e.eat_string() {
            Some(e.produce_with(|s| {
                if terminated {
                    let qlen = '"'.len_utf8();
                    let body = string::unescape(&s[qlen..s.len() - qlen]).unwrap();
                    TokenRep::String(body.into_owned())
                } else {
                    TokenRep::UnterminatedString
                }
            }))
        } else if let Some(terminated) = e.eat_char() {
            Some(e.produce_with(|s| {
                if terminated {
                    let body = string::unescape(&s["#\\".len()..]).unwrap();
                    TokenRep::Char(body.chars().next().expect("single character"))
                } else {
                    TokenRep::UnterminatedChar
                }
            }))
        } else if e.eat_lparen() {
            Some(e.produce(TokenRep::LParen))
        } else if e.eat_rparen() {
            Some(e.produce(TokenRep::RParen))
        } else if e.eat_lbrack() {
            Some(e.produce(TokenRep::LBrack))
        } else if e.eat_rbrack() {
            Some(e.produce(TokenRep::RBrack))
        } else if e.eat_lbrace() {
            Some(e.produce(TokenRep::LBrace))
        } else if e.eat_rbrace() {
            Some(e.produce(TokenRep::RBrace))
        } else if e.eat_quote() {
            Some(e.produce(TokenRep::Quote))
        } else if e.eat_backquote() {
            Some(e.produce(TokenRep::Backquote))
        } else if e.eat_backslash() {
            Some(e.produce(TokenRep::Backslash))
        } else if e.eat_tilde() {
            Some(e.produce(TokenRep::Tilde))
        } else if e.eat_comma_at() {
            Some(e.produce(TokenRep::CommaAt))
        } else if e.eat_comma() {
            Some(e.produce(TokenRep::Comma))
        } else if e.eat_true() {
            Some(e.produce(TokenRep::True))
        } else if e.eat_false() {
            Some(e.produce(TokenRep::False))
        } else if e.eat_if(|_| true) {
            Some(e.produce(TokenRep::UnknownCharacter))
        } else {
            panic!("no characters consumed")
        }
    }
}

struct Eat<'l, 'a> {
    lexer: &'l mut Lexer<'a>,
    current: &'a str,
    consumed_len: usize,
}

impl<'l, 'a> Eat<'l, 'a> {
    fn new(lexer: &'l mut Lexer<'a>) -> Self {
        let current = lexer.input;
        Eat {
            lexer,
            current,
            consumed_len: 0,
        }
    }

    fn produce(self, repr: TokenRep) -> Token {
        self.produce_with(|_| repr)
    }

    fn produce_with(self, f: impl FnOnce(&'a str) -> TokenRep) -> Token {
        let start = self.lexer.location;
        let (consumed, rest) = self.lexer.input.split_at(self.consumed_len);
        self.lexer.input = rest;
        self.lexer.location.eat(consumed);
        Token::new(LocationRange::new(start, self.lexer.location), f(consumed))
    }

    fn is_eof(&self) -> bool {
        self.current.is_empty()
    }

    fn eat(&mut self, len: usize) -> bool {
        if len != 0 && len <= self.current.len() {
            self.consumed_len += len;
            self.current = &self.current[len..];
            true
        } else {
            false
        }
    }

    fn eat_str(&mut self, text: &str) -> bool {
        if self.current.starts_with(text) {
            self.eat(text.len())
        } else {
            false
        }
    }

    fn eat_if(&mut self, f: impl FnOnce(char) -> bool) -> bool {
        match self.current.chars().next() {
            Some(ch) if f(ch) => self.eat(ch.len_utf8()),
            _ => false,
        }
    }

    fn eat_while(&mut self, f: impl Fn(char) -> bool) -> bool {
        let mut chars = self.current.chars();
        let mut next_start = 0;
        while match chars.next() {
            Some(ch) if f(ch) => {
                next_start += ch.len_utf8();
                true
            }
            _ => false,
        } {}
        self.eat(next_start)
    }

    fn eat_whitespaces(&mut self) -> bool {
        self.eat_while(|c| matches!(c, ' ' | '\t' | '\r' | '\n'))
    }

    fn eat_line_comment(&mut self) -> bool {
        if self.eat_str(";") {
            self.eat_while(|c| c != '\n');
            self.eat_str("\n");
            true
        } else {
            false
        }
    }

    fn eat_id(&mut self) -> bool {
        self.eat_while(|c| {
            matches!(
                c,
                '!' | '$' | '%' | '&' | '*' | '+' | '-' | '.' | '/' | '0'..='9' |
                ':' | '<' | '=' | '>' | '?' | 'A'..='Z' | '^' | '_' | 'a'..='z' | '|'
            )
        })
    }

    fn eat_string(&mut self) -> Option<bool> {
        if self.eat_str("\"") {
            while {
                self.eat_while(|c| !matches!(c, '"' | '\\'));
                if self.eat_str("\"") {
                    return Some(true);
                }
                self.eat('\\'.len_utf8()) && self.eat_if(|c| string::unescape_char(c).is_some())
            } {}
            Some(false)
        } else {
            None
        }
    }

    fn eat_char(&mut self) -> Option<bool> {
        if self.eat_str("#\\") {
            Some(
                self.eat_if(|c| c != '\\')
                    || (self.eat('\\'.len_utf8())
                        && self.eat_if(|c| string::unescape_char(c).is_some())),
            )
        } else {
            None
        }
    }

    fn eat_lparen(&mut self) -> bool {
        self.eat_str("(")
    }

    fn eat_rparen(&mut self) -> bool {
        self.eat_str(")")
    }

    fn eat_lbrack(&mut self) -> bool {
        self.eat_str("[")
    }

    fn eat_rbrack(&mut self) -> bool {
        self.eat_str("]")
    }

    fn eat_lbrace(&mut self) -> bool {
        self.eat_str("{")
    }

    fn eat_rbrace(&mut self) -> bool {
        self.eat_str("}")
    }

    fn eat_quote(&mut self) -> bool {
        self.eat_str("'")
    }

    fn eat_backquote(&mut self) -> bool {
        self.eat_str("`")
    }

    fn eat_backslash(&mut self) -> bool {
        self.eat_str("\\")
    }

    fn eat_tilde(&mut self) -> bool {
        self.eat_str("~")
    }

    fn eat_comma(&mut self) -> bool {
        self.eat_str(",")
    }

    fn eat_comma_at(&mut self) -> bool {
        self.eat_str(",@")
    }

    fn eat_true(&mut self) -> bool {
        self.eat_str("#t")
    }

    fn eat_false(&mut self) -> bool {
        self.eat_str("#f")
    }
}

#[cfg(test)]
mod tests {
    use self::TokenRep::*;
    use super::*;

    fn lex(input: &str) -> Vec<(u32, u32, TokenRep)> {
        Lexer::new(input)
            .map(|t| (t.range.start.offset, t.range.end.offset, t.rep))
            .collect()
    }

    #[test]
    fn test_lex() {
        assert_eq!(lex(""), vec![]);
        assert_eq!(lex("  \t  \r\n \n"), vec![(0, 9, Whitespaces)]);
        assert_eq!(
            lex("; hello\n;world \n"),
            vec![(0, 8, LineComment), (8, 16, LineComment)]
        );
        assert_eq!(
            lex("12 3a b-4 cd5e -6  7.8 .a ."),
            vec![
                (0, 2, Unsigned(12)),
                (2, 3, Whitespaces),
                (3, 5, Symbol("3a".to_string())),
                (5, 6, Whitespaces),
                (6, 9, Symbol("b-4".to_string())),
                (9, 10, Whitespaces),
                (10, 14, Symbol("cd5e".to_string())),
                (14, 15, Whitespaces),
                (15, 17, Signed(-6)),
                (17, 19, Whitespaces),
                (19, 22, FPNumber(7.8)),
                (22, 23, Whitespaces),
                (23, 25, Symbol(".a".to_string())),
                (25, 26, Whitespaces),
                (26, 27, Dot),
            ]
        );
        assert_eq!(
            lex("[)=,@,`'(]{}\\~"),
            vec![
                (0, 1, LBrack),
                (1, 2, RParen),
                (2, 3, Symbol("=".to_string())),
                (3, 5, CommaAt),
                (5, 6, Comma),
                (6, 7, Backquote),
                (7, 8, Quote),
                (8, 9, LParen),
                (9, 10, RBrack),
                (10, 11, LBrace),
                (11, 12, RBrace),
                (12, 13, Backslash),
                (13, 14, Tilde),
            ]
        );
        assert_eq!(
            lex("#t\"Hello, World!\\n\"#f"),
            vec![
                (0, 2, True),
                (2, 19, String("Hello, World!\n".to_string())),
                (19, 21, False)
            ]
        );
        assert_eq!(
            lex("#\\a#\\b#\\\\n#\\話"),
            vec![
                (0, 3, Char('a')),
                (3, 6, Char('b')),
                (6, 10, Char('\n')),
                (10, 15, Char('話'))
            ]
        );
        assert_eq!(lex("\"hello"), vec![(0, 6, UnterminatedString),]);
        assert_eq!(lex("\"hello\\"), vec![(0, 7, UnterminatedString),]);
        assert_eq!(lex("\0"), vec![(0, 1, UnknownCharacter)]);
        assert_eq!(
            lex("日本語"),
            vec![
                (0, 3, UnknownCharacter),
                (3, 6, UnknownCharacter),
                (6, 9, UnknownCharacter)
            ]
        );
    }
}
