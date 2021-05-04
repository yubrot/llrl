use super::*;
use crate::source_loc::SourceLocator;

#[test]
fn test_display() {
    let l = SourceLocator::temporary().issue(Default::default());

    assert_eq!(Sexp::signed(l, 123).to_string(), "123");
    assert_eq!(Sexp::signed(l, -456789).to_string(), "-456789");
    assert_eq!(
        Sexp::signed(l, std::i64::MIN).to_string(),
        std::i64::MIN.to_string()
    );
    assert_eq!(
        Sexp::signed(l, std::i64::MAX).to_string(),
        std::i64::MAX.to_string()
    );

    assert_eq!(Sexp::unsigned(l, 0).to_string(), "0");
    assert_eq!(
        Sexp::unsigned(l, std::u64::MAX).to_string(),
        std::u64::MAX.to_string()
    );

    assert_eq!(Sexp::fpnumber(l, 3.1415.into()).to_string(), "3.1415");

    assert_eq!(Sexp::char(l, '\n').to_string(), "#\\\\n");

    assert_eq!(
        Sexp::symbol(l, "hello/world".to_string()).to_string(),
        "hello/world"
    );

    assert_eq!(
        Sexp::string(l, "Hello, World!\n".to_string()).to_string(),
        "\"Hello, World!\\n\""
    );

    assert_eq!(Sexp::list(l, vec![]).to_string(), "()");
    assert_eq!(Sexp::list(l, vec![Sexp::signed(l, 1)]).to_string(), "(1)");
    assert_eq!(
        Sexp::list(l, vec![Sexp::signed(l, 1), Sexp::signed(l, 2)]).to_string(),
        "(1 2)"
    );
    assert_eq!(
        Sexp::list(
            l,
            vec![
                Sexp::signed(l, 1),
                Sexp::list(l, vec![Sexp::signed(l, 2), Sexp::signed(l, 3)])
            ]
        )
        .to_string(),
        "(1 (2 3))"
    );

    assert_eq!(
        Sexp::list_like(l, Sexp::signed(l, 1), vec![], Sexp::signed(l, 2),).to_string(),
        "(1 . 2)"
    );
    assert_eq!(
        Sexp::list_like(
            l,
            Sexp::signed(l, 1),
            vec![Sexp::signed(l, 3)],
            Sexp::signed(l, 2),
        )
        .to_string(),
        "(1 3 . 2)"
    );
    assert_eq!(
        Sexp::list_like(
            l,
            Sexp::signed(l, 1),
            vec![Sexp::signed(l, 3), Sexp::signed(l, 4)],
            Sexp::signed(l, 2),
        )
        .to_string(),
        "(1 3 4 . 2)"
    );

    assert_eq!(Sexp::bool(l, true).to_string(), "#t");
    assert_eq!(Sexp::bool(l, false).to_string(), "#f");

    assert_eq!(
        Sexp::cons(l, Sexp::bool(l, true), Sexp::bool(l, false)).to_string(),
        "(#t . #f)"
    );
    assert_eq!(
        Sexp::cons(
            l,
            Sexp::bool(l, true),
            Sexp::list_like(l, Sexp::signed(l, 1), vec![], Sexp::signed(l, 2))
        )
        .to_string(),
        "(#t 1 . 2)"
    );
    assert_eq!(
        Sexp::cons(
            l,
            Sexp::bool(l, true),
            Sexp::list(l, vec![Sexp::signed(l, 1)])
        )
        .to_string(),
        "(#t 1)"
    );

    assert_eq!(Sexp::nil(l).to_string(), "()");

    assert_eq!(Sexp::quote(l, Sexp::bool(l, true)).to_string(), "'#t");

    assert_eq!(
        Sexp::quasiquote(
            l,
            Sexp::list(
                l,
                vec![
                    Sexp::bool(l, true),
                    Sexp::unquote(l, Sexp::signed(l, 1)),
                    Sexp::unquote_splicing(
                        l,
                        Sexp::list(
                            l,
                            vec![Sexp::symbol(l, "foo".to_string()), Sexp::signed(l, 3)]
                        )
                    ),
                ]
            )
        )
        .to_string(),
        "`(#t ,1 ,@(foo 3))"
    );

    assert_eq!(
        Sexp::capture(l, Sexp::symbol(l, "foo".to_string()),).to_string(),
        "\\foo"
    );

    assert_eq!(
        Sexp::try_question(
            l,
            Sexp::list(l, vec![Sexp::bool(l, true), Sexp::bool(l, false),])
        )
        .to_string(),
        "(#t #f)?"
    );

    assert_eq!(
        Sexp::try_exclamation(
            l,
            Sexp::list(l, vec![Sexp::bool(l, true), Sexp::bool(l, false),])
        )
        .to_string(),
        "(#t #f)!"
    );

    assert_eq!(
        Sexp::annotate(
            l,
            Sexp::signed(l, 1),
            vec![Sexp::signed(l, 2), Sexp::signed(l, 3)]
        )
        .to_string(),
        "1 {2 3}"
    );
}
