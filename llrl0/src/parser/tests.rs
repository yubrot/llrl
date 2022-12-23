use super::*;
use crate::lexer::Lexer;
use crate::sexp::{Sexp, Ss};
use crate::source_loc::SourceLocator;

fn parse<T: Nonterminal>(input: &str) -> std::result::Result<String, (TokenRep, String)>
where
    T::Result: ToString,
{
    match super::parse::<T, _>(&mut SourceLocator::temporary(), Lexer::new(input))
        .aggregate_errors()
    {
        Ok(r) => Ok(r.to_string()),
        Err(e) => Err((e.unexpected.rep, e.expected)),
    }
}

#[test]
fn test_parse() {
    assert_eq!(parse::<Sexp>("foo"), Ok("foo".to_string()));
    assert_eq!(parse::<Sexp>("abc123"), Ok("abc123".to_string()));
    assert_eq!(parse::<Sexp>("foo-bar"), Ok("foo-bar".to_string()));
    assert_eq!(parse::<Sexp>("*foo*"), Ok("*foo*".to_string()));
    assert_eq!(
        parse::<Sexp>("x1-y2!$%&*+-/:<=>?^_"),
        Ok("x1-y2!$%&*+-/:<=>?^_".to_string())
    );
    assert_eq!(
        parse::<Sexp>("the-Answer-to-the-Ultimate-Question"),
        Ok("the-Answer-to-the-Ultimate-Question".to_string())
    );
    assert_eq!(parse::<Sexp>("0"), Ok("0".to_string()));
    assert_eq!(parse::<Sexp>("1234"), Ok("1234".to_string()));
    assert_eq!(parse::<Sexp>("3.5"), Ok("3.5".to_string()));
    assert_eq!(parse::<Sexp>("+732"), Ok("732".to_string()));
    assert_eq!(parse::<Sexp>("-42"), Ok("-42".to_string()));
    assert_eq!(parse::<Sexp>("13e2"), Ok("1300".to_string()));
    assert_eq!(parse::<Sexp>("9e+1"), Ok("90".to_string()));
    assert_eq!(parse::<Sexp>("125e-1"), Ok("12.5".to_string()));
    assert_eq!(parse::<Sexp>("\"foo\""), Ok("\"foo\"".to_string()));
    assert_eq!(
        parse::<Sexp>("\"Hello, World!\""),
        Ok("\"Hello, World!\"".to_string())
    );
    assert_eq!(
        parse::<Sexp>("\"( ) . 0 a\""),
        Ok("\"( ) . 0 a\"".to_string())
    );
    assert_eq!(
        parse::<Sexp>("\"foo\\nbar\""),
        Ok("\"foo\\nbar\"".to_string())
    );
    assert_eq!(
        parse::<Sexp>("\"foo\nbar\""),
        Ok("\"foo\\nbar\"".to_string())
    );
    assert_eq!(
        parse::<Sexp>("\"A \\\"\\\\ B\""),
        Ok("\"A \\\"\\\\ B\"".to_string())
    );
    assert_eq!(parse::<Sexp>("#\\z"), Ok("#\\z".to_string()));
    assert_eq!(parse::<Sexp>("#\\\\t"), Ok("#\\\\t".to_string()));
    assert_eq!(parse::<Sexp>("()"), Ok("()".to_string()));
    assert_eq!(parse::<Sexp>("[]"), Ok("()".to_string()));
    assert_eq!(parse::<Sexp>("(foo . bar)"), Ok("(foo . bar)".to_string()));
    assert_eq!(parse::<Sexp>("(1 2 . 3)"), Ok("(1 2 . 3)".to_string()));
    assert_eq!(
        parse::<Sexp>("(1 2 . (3 . 4))"),
        Ok("(1 2 3 . 4)".to_string())
    );
    assert_eq!(parse::<Sexp>("(1 2 . (3 4))"), Ok("(1 2 3 4)".to_string()));
    assert_eq!(parse::<Sexp>("(a b c d)"), Ok("(a b c d)".to_string()));
    assert_eq!(parse::<Sexp>("(@)"), Ok("(())".to_string()));
    assert_eq!(parse::<Sexp>("(@ a)"), Ok("((a))".to_string()));
    assert_eq!(parse::<Sexp>("(a @ b)"), Ok("(a (b))".to_string()));
    assert_eq!(parse::<Sexp>("(a @ b @ c)"), Ok("(a (b (c)))".to_string()));
    assert_eq!(
        parse::<Sexp>("(@ a b @ c d e)"),
        Ok("((a b (c d e)))".to_string())
    );
    assert_eq!(
        parse::<Sexp>("(e .)"),
        Err((TokenRep::RParen, "S-expression".to_string()))
    );
    assert_eq!(parse::<Sexp>("(. e)"), Ok("e".to_string()));
    assert_eq!(parse::<Sexp>("["), Err((TokenRep::Eof, "]".to_string())));
    assert_eq!(
        parse::<Sexp>("[)"),
        Err((TokenRep::RParen, "]".to_string()))
    );
    assert_eq!(
        parse::<Sexp>("(foo [bar (baz)])"),
        Ok("(foo (bar (baz)))".to_string())
    );
    assert_eq!(parse::<Sexp>("(#t #f)"), Ok("(#t #f)".to_string()));
    assert_eq!(parse::<Sexp>("'foo"), Ok("'foo".to_string()));
    assert_eq!(parse::<Sexp>("'(foo 123)"), Ok("'(foo 123)".to_string()));
    assert_eq!(parse::<Sexp>("'\\foo"), Ok("'\\foo".to_string()));
    assert_eq!(
        parse::<Sexp>("`(i ,j ,@(k l))"),
        Ok("`(i ,j ,@(k l))".to_string())
    );
    assert_eq!(parse::<Sexp>("       123"), Ok("123".to_string()));
    assert_eq!(
        parse::<Sexp>("; hello\n; world\n   ()   "),
        Ok("()".to_string())
    );
    assert_eq!(
        parse::<Sexp>("(foo ; comment\nbar ; comment\n)"),
        Ok("(foo bar)".to_string())
    );
    assert_eq!(
        parse::<Sexp>("\0"),
        Err((TokenRep::UnknownCharacter, "S-expression".to_string()))
    );
    assert_eq!(
        parse::<Ss>("\0"),
        Err((TokenRep::UnknownCharacter, "S-expression".to_string()))
    );
    assert_eq!(parse::<Ss>(")"), Err((TokenRep::RParen, "eof".to_string())));
    assert_eq!(
        parse::<Ss>("    foo  \n  bar ; comment\n baz   ; eof"),
        Ok("foo\nbar\nbaz".to_string())
    );

    let result = super::parse::<Sexp, _>(
        &mut SourceLocator::temporary(),
        Lexer::new("(1 \0 2 (3 \0 4) 5)"),
    );
    assert_eq!(
        result.final_result.map(|r| r.to_string()),
        Ok("(1 2 (3 4) 5)".to_string())
    );
    assert_eq!(
        result
            .inner_errors
            .iter()
            .map(|e| e.to_string())
            .collect::<Vec<_>>(),
        vec![
            format!(
                "1:4-1:5: Expected S-expression but got {}",
                TokenRep::UnknownCharacter
            ),
            format!(
                "1:11-1:12: Expected S-expression but got {}",
                TokenRep::UnknownCharacter
            ),
        ]
    );
}
