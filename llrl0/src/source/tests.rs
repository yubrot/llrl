use super::*;
use crate::source_loc::SourceLocator;
use std::collections::{HashMap, HashSet};

#[test]
fn resolve_dependencies() {
    let mut source = Source::from_code_text(
        "~/foo".parse::<Path>().unwrap(),
        &mut SourceLocator::temporary(),
        ")",
    );
    assert_eq!(source.path, "~/foo".parse::<Path>().unwrap());

    source.resolve_dependencies();
    assert!(source.dependencies.is_empty());
    assert!(matches!(source.errors.as_slice(), [Error::ParseFailed(_)]));

    let mut source = Source::from_code_text(
        "~/foo".parse::<Path>().unwrap(),
        &mut SourceLocator::temporary(),
        r#"
        (import "std/io" write)
        (import "~/foo/bar" baz)
    "#,
    );

    source.resolve_dependencies();
    assert_eq!(
        source.dependencies,
        vec![
            ("builtin".to_string(), Path::builtin()),
            ("std".to_string(), Path::std()),
            ("std/io".to_string(), "std/io".parse::<Path>().unwrap()),
            (
                "~/foo/bar".to_string(),
                "~/foo/bar".parse::<Path>().unwrap()
            ),
        ]
        .into_iter()
        .collect::<HashMap<_, _>>()
    );
    assert!(source.errors.is_empty());

    let mut source = Source::from_code_text(
        "hello/world".parse::<Path>().unwrap(),
        &mut SourceLocator::temporary(),
        r#"
        (import "std/io" write)
        (import "~/foo/bar" baz)
        (import "../hello")
        (import "~/world")
    "#,
    );

    source.resolve_dependencies();
    assert_eq!(
        source.dependencies,
        vec![
            ("builtin".to_string(), Path::builtin()),
            ("std/io".to_string(), "std/io".parse::<Path>().unwrap()),
            (
                "~/foo/bar".to_string(),
                "hello/foo/bar".parse::<Path>().unwrap()
            ),
            ("std".to_string(), Path::std()),
        ]
        .into_iter()
        .collect::<HashMap<_, _>>()
    );
    assert!(matches!(
        source.errors.as_slice(),
        [
            Error::InvalidImportPath(sym, _),
            Error::CannotImportModuleItself,
        ]
            if sym == "../hello"
    ));
}

#[test]
fn preprocess() {
    let mut source = Source::from_code_text(
        "~/foo".parse::<Path>().unwrap(),
        &mut SourceLocator::temporary(),
        "$foo",
    );

    source.preprocess(&Default::default());
    assert!(matches!(
        source.errors.as_slice(),
        [Error::PreprocessFailed(_)]
    ));
}

#[test]
fn collector() {
    let mut loader = Loader::new();
    for (path, text) in [
        ("builtin", ""),
        ("~", r#"(import "~/bar") (import "std/a")"#),
        ("~/foo", r#"(import "~/bar") (import "std/b")"#),
        ("~/bar", r#"(import "~/baz") (import "std")"#),
        ("std", r#"(import "~/b")"#),
        ("std/a", r#"(import "~/b")"#),
        ("std/b", r#""#),
        ("std/c", r#"(hoge"#),
        ("std", r#"(no-implicit-std)"#),
    ] {
        assert!(loader.add_source(path.parse::<Path>().unwrap(), text.to_string()));
    }

    let result = collect(
        vec![
            &"~".parse::<Path>().unwrap(),
            &"~/foo".parse::<Path>().unwrap(),
            &"std/c".parse::<Path>().unwrap(),
        ],
        &loader,
        &mut Default::default(),
        &Default::default(),
        &mut Default::default(),
    );

    assert_eq!(
        result
            .packages()
            .keys()
            .map(|name| name.to_string())
            .collect::<HashSet<_>>(),
        vec!["~", "std", "builtin"]
            .into_iter()
            .map(|s| s.to_string())
            .collect::<HashSet<_>>(),
    );

    assert_eq!(
        result
            .sources()
            .map(|source| source.path.to_string())
            .collect::<HashSet<_>>(),
        vec!["~", "~/foo", "~/bar", "~/baz", "std", "std/a", "std/b", "std/c", "std", "builtin"]
            .into_iter()
            .map(|s| s.to_string())
            .collect()
    );

    for source in result.sources() {
        match source.path.to_string().as_str() {
            "~/baz" => assert!(matches!(source.errors.as_slice(), [Error::ModuleNotFound])),
            "std/c" => assert!(matches!(source.errors.as_slice(), [Error::ParseFailed(_)])),
            _ => assert!(source.errors.is_empty()),
        }
    }
}
