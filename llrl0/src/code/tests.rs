use super::*;
use crate::report::Report;
use crate::source_loc::{SourceLocationTable, SourceLocator};
use std::collections::{HashMap, HashSet};

#[test]
fn test_from_source_code() {
    let code = Code::from_source_text(
        "~/foo".parse::<Path>().unwrap(),
        &mut SourceLocator::temporary(),
        ")",
    );

    assert_eq!(code.path, "~/foo".parse::<Path>().unwrap());
    assert!(code.dependencies.is_empty());
    assert!(matches!(code.errors.as_slice(), [Error::ParseFailed(_)]));

    let code = Code::from_source_text(
        "~/foo".parse::<Path>().unwrap(),
        &mut SourceLocator::temporary(),
        r#"
        (import "std/io" write)
        (import "~/foo/bar" baz)
    "#,
    );

    assert_eq!(code.path, "~/foo".parse::<Path>().unwrap());
    assert_eq!(
        code.dependencies,
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
    assert!(code.errors.is_empty());

    let code = Code::from_source_text(
        "hello/world".parse::<Path>().unwrap(),
        &mut SourceLocator::temporary(),
        r#"
        (import "std/io" write)
        (import "~/foo/bar" baz)
        (import "../hello")
        (import "~/world")
    "#,
    );

    assert_eq!(code.path, "hello/world".parse::<Path>().unwrap());
    assert_eq!(
        code.dependencies,
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
        code.errors.as_slice(),
        [
            Error::InvalidImportPath(sym, _),
            Error::CannotImportModuleItself,
        ]
            if sym == "../hello"
    ));
}

#[test]
fn test_collect() {
    let mut loader = Loader::new();
    for (path, text) in vec![
        ("builtin", ""),
        ("~", r#"(import "~/bar") (import "std/a")"#),
        ("~/foo", r#"(import "~/bar") (import "std/b")"#),
        ("~/bar", r#"(import "~/baz") (import "std")"#),
        ("std", r#"(import "~/b")"#),
        ("std/a", r#"(import "~/b")"#),
        ("std/b", r#""#),
        ("std/c", r#"(hoge"#),
        ("std", r#"(no-implicit-std)"#),
    ]
    .into_iter()
    {
        assert!(loader.add_source(path.parse::<Path>().unwrap(), text.to_string()));
    }

    let mut source_location_table = SourceLocationTable::new();
    let result = collect(
        vec![
            &"~".parse::<Path>().unwrap(),
            &"~/foo".parse::<Path>().unwrap(),
            &"std/c".parse::<Path>().unwrap(),
        ],
        &loader,
        &mut source_location_table,
        &mut Report::new(),
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
            .codes()
            .map(|code| code.path.to_string())
            .collect::<HashSet<_>>(),
        vec!["~", "~/foo", "~/bar", "~/baz", "std", "std/a", "std/b", "std/c", "std", "builtin"]
            .into_iter()
            .map(|s| s.to_string())
            .collect()
    );

    for code in result.codes() {
        match code.path.to_string().as_str() {
            "~/baz" => assert!(matches!(code.errors.as_slice(), [Error::ModuleNotFound])),
            "std/c" => assert!(matches!(code.errors.as_slice(), [Error::ParseFailed(_)])),
            _ => assert!(code.errors.is_empty()),
        }
    }
}
