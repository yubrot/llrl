use crate::formatting::ContextualDisplay as _;
use crate::prelude::*;
use std::collections::BTreeSet;
use std::fmt;
use std::fs;

mod error_expectation;
mod module_validation;
mod test_case;
mod test_std;

use error_expectation::ErrorExpectation;
use module_validation::ModuleValidation;
use test_case::TestCase;

#[derive(Debug)]
struct TestContext {
    loc: SourceLocation,
    source_location_table: SourceLocationTable,
    report: Report,
}

impl TestContext {
    fn header<'a>(&'a self) -> impl fmt::Display + 'a {
        self.loc.fmt_on(&self.source_location_table)
    }
}

#[test]
fn test_language() {
    let test_cases_dir_path = std::env::current_dir()
        .unwrap()
        .parent()
        .unwrap()
        .join("testcases");

    let mut report = Report::new();

    for path in fs::read_dir(&test_cases_dir_path)
        .unwrap()
        .map(|entry| entry.unwrap().path())
        .filter(|path| path.is_file() && path.extension().and_then(|s| s.to_str()) == Some("llrl"))
        .collect::<BTreeSet<_>>()
    {
        let short_path = path
            .file_name()
            .unwrap()
            .to_string_lossy()
            .parse::<Path>()
            .unwrap();
        let test_source_text = match fs::read_to_string(&path) {
            Ok(test_source) => test_source,
            Err(e) => panic!("Failed to load {}: {}", &short_path, e),
        };

        let mut source_location_table = SourceLocationTable::new();
        let test_source = {
            let mut locator = source_location_table.begin_locate(&short_path);
            let test_source = match parse::<Ss, _>(&mut locator, Lexer::new(&test_source_text))
                .aggregate_errors()
            {
                Ok(test_source) => test_source,
                Err(e) => panic!("Failed to parse {}: {}", &short_path, e),
            };
            source_location_table.complete_locate(locator);
            test_source
        };

        for s in test_source.ss {
            let mut ctx = TestContext {
                loc: s.loc,
                source_location_table: source_location_table.clone(),
                report: Report::new(),
            };

            match s.matches::<TestCase>() {
                Ok(test_case) => test_case.run(&mut ctx),
                Err(e) => panic!("{}", e.fmt_on(&source_location_table)),
            }
            report.merge(&ctx.report);
        }
    }

    println!("### report");
    println!("{}", report);
}
