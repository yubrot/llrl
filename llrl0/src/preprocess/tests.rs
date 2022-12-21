use super::*;
use crate::lexer::Lexer;
use crate::parser::parse;
use crate::source_loc::SourceLocator;

fn run(input: &str) -> Result<String, Error> {
    run_with_features(input, &[])
}

fn run_with_features(input: &str, features: &[&str]) -> Result<String, Error> {
    let mut ss = parse::<Ss, _>(&mut SourceLocator::temporary(), Lexer::new(input))
        .aggregate_errors()
        .unwrap();
    let mut pp = Preprocessor::new();
    for feature in features {
        pp.enable_feature(feature.to_string());
    }
    pp.run(&mut ss)?;
    Ok(ss.to_string())
}

#[test]
fn identity() {
    assert_eq!(run(""), Ok("".to_string()));
    assert_eq!(run("foo"), Ok("foo".to_string()));
    assert_eq!(run("foo\n(bar baz)"), Ok("foo\n(bar baz)".to_string()));
}

#[test]
fn symbol() {
    assert!(matches!(run("($symbol)"), Err(Error::EmptySymbol(_))));
    assert!(matches!(run("($symbol \"\")"), Err(Error::EmptySymbol(_))));
    assert_eq!(run("($symbol foo \"bar\")"), Ok("foobar".to_string()));
    assert_eq!(run("($symbol a \"b\" #\\c)"), Ok("abc".to_string()));
    assert_eq!(
        run("($let1 $foo \"foo\" ($symbol $foo $foo))"),
        Ok("foofoo".to_string())
    );
}

#[test]
fn r#let() {
    assert!(matches!(run("($let)"), Err(Error::DirectiveSyntax(_, _))));
    assert!(matches!(
        run("($let foo)"),
        Err(Error::DirectiveSyntax(_, _))
    ));
    assert!(matches!(
        run("($let ([$foo]))"),
        Err(Error::DirectiveSyntax(_, _))
    ));
    assert!(matches!(
        run("($let ([foo 123]))"),
        Err(Error::DirectiveSyntax(_, _))
    ));
    assert_eq!(run("($let ())"), Ok("".to_string()));
    assert_eq!(run("($let () 1)"), Ok("1".to_string()));
    assert_eq!(run("($let () 1 2)"), Ok("1\n2".to_string()));
    assert_eq!(
        run("($let ([$foo 12] [$bar 34]) $foo $bar)"),
        Ok("12\n34".to_string())
    );

    // shadowing
    assert_eq!(
        run("($let ([$foo 12] [$bar 34]) @$let ([$foo ($foo $bar)] [$bar ($bar $foo)]) $foo $bar)"),
        Ok("(12 34)\n(34 12)".to_string())
    );

    assert!(matches!(
        run("($let ([$foo 12 34]) $foo)"), // syntactically possible, but restricted for understandability
        Err(Error::DirectiveSyntax(_, _))
    ));
    assert!(matches!(
        run("($let ([$foo ($for1 $ [1 2] $)]) $foo)"), // ambiguous expansion is disallowed
        Err(Error::ExpansionDisallowed(_, _))
    ));
}

#[test]
fn let1() {
    assert!(matches!(run("$foo"), Err(Error::UndefinedMetaVariable(_, s)) if &s == "$foo"));
    assert!(matches!(run("($let1)"), Err(Error::DirectiveSyntax(_, _))));
    assert!(matches!(
        run("($let1 $foo)"),
        Err(Error::DirectiveSyntax(_, _))
    ));
    assert!(matches!(
        run("($let1 foo bar)"),
        Err(Error::DirectiveSyntax(_, _))
    ));
    assert_eq!(run("($let1 $foo 123)"), Ok("".to_string()));
    assert_eq!(run("($let1 $foo bar $foo)"), Ok("bar".to_string()));
    assert!(matches!(
        run("($let1 $foo bar $foo) $foo"),
        Err(Error::UndefinedMetaVariable(_, s)) if &s == "$foo"));
    assert_eq!(
        run("($let1 $foo bar @$let1 $bar foo ($foo $bar baz))"),
        Ok("(bar foo baz)".to_string())
    );
    assert_eq!(
        run("($let1 $foo (1 ($let1 $foo 2 $foo)) $foo)"),
        Ok("(1 2)".to_string())
    );
    assert!(matches!(
        run("($let1 $foo ($for1 $ [1 2] $) $foo)"), // ambiguous expansion is disallowed
        Err(Error::ExpansionDisallowed(_, _))
    ));

    // shadowing
    assert_eq!(
        run("($let1 $foo foo @$let1 $foo ($foo bar) $foo)"),
        Ok("(foo bar)".to_string())
    );
    assert_eq!(
        run("($let1 $foo foo ($let1 $foo ($foo bar) $foo) $foo)"),
        Ok("(foo bar)\nfoo".to_string())
    );
}

#[test]
fn r#for() {
    assert!(matches!(run("($for)"), Err(Error::DirectiveSyntax(_, _))));
    assert!(matches!(
        run("($for $ (1 2 3))"),
        Err(Error::DirectiveSyntax(_, _))
    ));
    assert!(matches!(
        run("($for [$] [[]])"),
        Err(Error::DirectiveSyntax(_, _))
    ));
    assert!(matches!(
        run("($for [$a $b] [[1 2] [3 4 5]])"),
        Err(Error::DirectiveSyntax(_, _))
    ));
    assert!(matches!(
        run("($for [foo] [[1] [2]])"),
        Err(Error::DirectiveSyntax(_, _))
    ));
    assert_eq!(
        run("($for [$a $b] [[1 2] [3 4]] ($a $b))"),
        Ok("(1 2)\n(3 4)".to_string())
    );
    assert!(matches!(
        run("($for [$] ($for1 $ [[] 12] $))"), // ambiguous expansion is disallowed
        Err(Error::ExpansionDisallowed(_, _))
    ));
}

#[test]
fn for1() {
    assert!(matches!(run("($for1)"), Err(Error::DirectiveSyntax(_, _))));
    assert!(matches!(
        run("($for1 $)"),
        Err(Error::DirectiveSyntax(_, _))
    ));
    assert!(matches!(
        run("($for1 $ 1)"),
        Err(Error::DirectiveSyntax(_, _))
    ));
    assert!(matches!(
        run("($for1 () ())"),
        Err(Error::DirectiveSyntax(_, _))
    ));
    assert!(matches!(
        run("($for1 $ ($for1 $ [[] 12] $))"), // ambiguous expansion is disallowed
        Err(Error::ExpansionDisallowed(_, _))
    ));
    assert_eq!(run("($for1 $ [])"), Ok("".to_string()));
    assert_eq!(run("($for1 $ [1 2])"), Ok("".to_string()));
    assert_eq!(run("($for1 $ [1 2] $)"), Ok("1\n2".to_string()));
    assert_eq!(run("($for1 $ [1 2] $ $)"), Ok("1\n1\n2\n2".to_string()));
    assert_eq!(
        run("($for1 $a [1 2] @$for1 $b [3 4] ($a $b))"),
        Ok("(1 3)\n(1 4)\n(2 3)\n(2 4)".to_string())
    );
    assert_eq!(
        run("($for1 $xs [[1 2] [3] []] @$for1 $x $xs ($x $xs))"),
        Ok("(1 (1 2))\n(2 (1 2))\n(3 (3))".to_string())
    );
}

#[test]
fn when() {
    assert_eq!(
        run("1\n($when #t 2 3)\n4\n5"),
        Ok("1\n2\n3\n4\n5".to_string())
    );
    assert_eq!(run("1\n($when #f 2 3)\n4\n5"), Ok("1\n4\n5".to_string()));
}

#[test]
fn not() {
    assert!(matches!(
        run("($not foo)"),
        Err(Error::DirectiveSyntax(_, _))
    ));
    assert!(matches!(
        run("($not #t #f)"),
        Err(Error::DirectiveSyntax(_, _))
    ));
    assert_eq!(run("#t"), Ok("#t".to_string()));
    assert_eq!(run("($not #t)"), Ok("#f".to_string()));
    assert_eq!(
        run("(foo ($not #f) ($not #t))"),
        Ok("(foo #t #f)".to_string())
    );
    assert_eq!(run("($not ($not #t))"), Ok("#t".to_string()));
}

#[test]
fn feature() {
    assert!(matches!(
        run("($feature)"),
        Err(Error::DirectiveSyntax(_, _))
    ));
    assert!(matches!(
        run("($feature foo)"),
        Err(Error::DirectiveSyntax(_, _))
    ));
    assert_eq!(
        run_with_features("($feature \"foo\")", &[]),
        Ok("#f".to_string())
    );
    assert_eq!(
        run_with_features("($feature \"foo\")", &["foo"]),
        Ok("#t".to_string())
    );
    assert_eq!(
        run_with_features("(: ($feature \"foo\") ($feature \"bar\"))", &["foo"]),
        Ok("(: #t #f)".to_string())
    );
}

#[test]
fn list_like() {
    assert_eq!(
        run("($let1 $foo (2 3) (1 4 . $foo))"),
        Ok("(1 4 2 3)".to_string())
    );
    assert_eq!(run("(($for1 $ [1 2] $) . 3)"), Ok("(1 2 . 3)".to_string()));
    assert!(matches!(
        run("(1 . ($for1 $ [1 2] $))"), // => (1 . 2 3), which is invalid
        Err(Error::ExpansionDisallowed(_, _))
    ));
}
