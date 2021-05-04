use super::{test_std, ErrorExpectation, ModuleValidation, TestContext};
use crate::formatting::ContextualDisplay as _;
use crate::prelude::*;
use crate::sexp::matcher as m;
use itertools::Itertools;
use std::borrow::Cow;
use std::collections::HashMap;
use std::sync::Arc;

pub(super) struct TestCase {
    loader: CodeLoader,
    target: TestTarget,
    condition: TestCondition,
}

impl TestCase {
    fn new(sources: HashMap<String, Ss>, target: TestTarget, condition: TestCondition) -> Self {
        let loader = target.prepare_loader(sources);

        Self {
            loader,
            target,
            condition,
        }
    }

    pub(super) fn run(self, ctx: &mut TestContext) {
        println!("{}", ctx.header());

        let codes = collect_codes(
            &[Path::current()],
            &self.loader,
            &mut ctx.source_location_table,
            &mut ctx.report,
        );

        for (path, error) in codes.errors() {
            panic!(
                "{}: Failed to create a code {}: {}",
                ctx.header(),
                path,
                error
            );
        }

        match codes.resolve_dependencies_order() {
            Ok(codes) => self.target.run(codes, &self.condition, ctx),
            Err(paths) => {
                let paths = paths.into_iter().join(", ");
                panic!("{}: Circular dependencies: {}", ctx.header(), paths);
            }
        }
    }
}

impl<'a> m::Match<'a> for TestCase {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        fn source(s: &Sexp, ss: &[Sexp]) -> HashMap<String, Ss> {
            vec![("~".to_string(), Ss::new(s.loc, ss.to_vec()))]
                .into_iter()
                .collect()
        }

        fn sources(ss: &[Sexp]) -> Result<HashMap<String, Ss>, m::Error> {
            ss.into_iter()
                .map(|s| {
                    let (name, ss) = s.matches::<(m::String, m::Rest<m::Any>)>()?;
                    Ok((name.to_string(), Ss::new(s.loc, ss.to_vec())))
                })
                .collect()
        }

        if let Ok(("test-module", cond, ss)) =
            s.matches::<(m::Symbol, TestCondition, m::Rest<m::Any>)>()
        {
            Ok(Self::new(source(s, ss), TestTarget::Module, cond))
        } else if let Ok(("test-modules", cond, ss)) =
            s.matches::<(m::Symbol, TestCondition, m::Rest<m::Any>)>()
        {
            Ok(Self::new(sources(ss)?, TestTarget::Module, cond))
        } else if let Ok(("test-backend", cond, ss)) =
            s.matches::<(m::Symbol, TestCondition, m::Rest<m::Any>)>()
        {
            Ok(Self::new(source(s, ss), TestTarget::Backend, cond))
        } else if let Ok(("test-backends", cond, ss)) =
            s.matches::<(m::Symbol, TestCondition, m::Rest<m::Any>)>()
        {
            Ok(Self::new(sources(ss)?, TestTarget::Backend, cond))
        } else if let Ok(("test-std", ss)) = s.matches::<(m::Symbol, m::Rest<m::Any>)>() {
            let cond = TestCondition::Pass(Vec::new());
            Ok(Self::new(source(s, ss), TestTarget::Std, cond))
        } else if let Ok(("test-stds", ss)) = s.matches::<(m::Symbol, m::Rest<m::Any>)>() {
            let cond = TestCondition::Pass(Vec::new());
            Ok(Self::new(sources(ss)?, TestTarget::Std, cond))
        } else {
            Err(Self::error(s))
        }
    }

    fn expect() -> Cow<'static, str> {
        "test-case".into()
    }
}

#[derive(Debug)]
pub(super) enum TestTarget {
    Module,
    Backend,
    Std,
}

impl TestTarget {
    fn prepare_loader(&self, sources: HashMap<String, Ss>) -> CodeLoader {
        let mut loader = CodeLoader::new();
        loader.add_module(Path::builtin(), ast::builtin::module());

        for (path, source) in sources {
            assert!(loader.add_module(
                path.parse::<Path>().unwrap_or_else(|e| panic!("{}", e)),
                source,
            ));
        }

        match self {
            Self::Module => {
                assert!(loader.add_module(Path::std(), test_std::prelude_module_for_module_test()));
            }
            Self::Backend => {
                assert!(loader.add_module(Path::std(), test_std::prelude_module_for_backend_test()));
            }
            Self::Std => {
                assert!(loader.add_package(
                    PackageName::std(),
                    std::env::current_dir()
                        .unwrap()
                        .parent()
                        .unwrap()
                        .join("std"),
                ));
            }
        }

        loader
    }

    fn run(&self, codes: Vec<Code>, cond: &TestCondition, ctx: &mut TestContext) {
        match self {
            Self::Module => {
                let (modules, errors) =
                    build_modules(codes, Default::default(), &(), &mut ctx.report);
                cond.run(&modules, &errors, ctx);
            }
            Self::Backend => {
                let backend = InterpreterBackend::new();
                self.run_backend(codes.clone(), cond, backend, ctx);
                let backend = LLVMBackend::new(LLVMBackendOptions::default());
                self.run_backend(codes, cond, backend, ctx);
            }
            Self::Std => {
                let backend = LLVMBackend::new(LLVMBackendOptions::default());
                self.run_backend(codes, cond, backend, ctx);
            }
        }
    }

    fn run_backend<B: Backend>(
        &self,
        codes: Vec<Code>,
        cond: &TestCondition,
        backend: B,
        ctx: &mut TestContext,
    ) {
        let emitter = Emitter::new(backend);
        let entry_points = vec![Path::current()].into_iter().collect();
        let (modules, errors) = build_modules(codes, entry_points, &emitter, &mut ctx.report);
        cond.run(&modules, &errors, ctx);

        if matches!(cond, TestCondition::Pass(_)) {
            let mut backend = emitter.complete(&mut ctx.report);
            match backend.execute_main() {
                Ok(value) => match value.as_bool() {
                    Some(true) => {}
                    _ => panic!("{}: Expected #t but got {}", ctx.header(), value),
                },
                Err(error) => panic!("{}: Execution error:\n{}", ctx.header(), error),
            }
            backend.complete(&mut ctx.report);
        }
    }
}

#[derive(Debug)]
pub(super) enum TestCondition {
    Pass(Vec<ModuleValidation>),
    Fail(Option<ErrorExpectation>),
}

impl TestCondition {
    pub(super) fn run(
        &self,
        modules: &[Arc<Module>],
        errors: &[(Code, ModuleError)],
        ctx: &mut TestContext,
    ) {
        match self {
            Self::Pass(validations) => {
                assert!(
                    errors.is_empty(),
                    "{}: Expected success but got errors:\n{}",
                    ctx.header(),
                    errors_summary(&errors, ctx)
                );

                let modules = modules
                    .into_iter()
                    .map(|m| (m.id(), Arc::clone(m)))
                    .collect::<HashMap<_, _>>();

                let entry_module = modules
                    .values()
                    .find(|m| *m.path() == Path::current())
                    .unwrap();

                for validation in validations.iter() {
                    validation.run(&modules, entry_module, ctx);
                }
            }
            Self::Fail(None) => assert!(
                !errors.is_empty(),
                "{}: Expected failure but succeeded",
                ctx.header()
            ),
            Self::Fail(Some(error_expectation)) => match errors {
                [] => panic!(
                    "{}: Expected `{:?}` but succeeded",
                    ctx.header(),
                    error_expectation
                ),
                [(code, error)] => assert!(
                    error_expectation.matches(error),
                    "{}: Expected `{:?}` but got an error:\n{}: {}",
                    ctx.header(),
                    error_expectation,
                    code.path,
                    error.fmt_on(&ctx.source_location_table)
                ),
                errors => panic!(
                    "{}: Expected `{:?}` but got errors:\n{}",
                    ctx.header(),
                    error_expectation,
                    errors_summary(&errors, ctx)
                ),
            },
        }
    }
}

impl<'a> m::Match<'a> for TestCondition {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        if let Ok("pass") = s.matches::<m::Symbol>() {
            Ok(Self::Pass(Vec::new()))
        } else if let Ok(("pass", vs)) = s.matches::<(m::Symbol, m::Rest<ModuleValidation>)>() {
            Ok(Self::Pass(vs))
        } else if let Ok("fail") = s.matches::<m::Symbol>() {
            Ok(Self::Fail(None))
        } else if let Ok(("fail", e)) = s.matches::<(m::Symbol, ErrorExpectation)>() {
            Ok(Self::Fail(Some(e)))
        } else {
            Err(Self::error(s))
        }
    }

    fn expect() -> Cow<'static, str> {
        "<test-condition>".into()
    }
}

fn errors_summary(errors: &[(Code, ModuleError)], ctx: &TestContext) -> String {
    errors
        .into_iter()
        .map(|(code, error)| {
            format!(
                "{} (while building {})",
                error.fmt_on(&ctx.source_location_table),
                code.path
            )
        })
        .join("\n")
}
