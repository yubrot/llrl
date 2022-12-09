use super::{test_std, ErrorExpectation, ModuleValidation, TestContext};
use crate::formatting::ContextualDisplay as _;
use crate::prelude::*;
use crate::sexp::matcher as m;
use itertools::Itertools;
use std::borrow::Cow;
use std::collections::HashMap;
use std::sync::Arc;

pub(super) struct TestCase {
    loader: SourceLoader,
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

        let sources = collect_sources(
            &[Path::current()],
            &self.loader,
            &mut ctx.source_location_table,
            &mut ctx.report,
        );

        for (path, error) in sources.errors() {
            panic!(
                "{}: Failed to create a source {}: {}",
                ctx.header(),
                path,
                error
            );
        }

        match sources.resolve_dependencies_order() {
            Ok(sources) => self.target.run(sources, &self.condition, ctx),
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
    fn prepare_loader(&self, sources: HashMap<String, Ss>) -> SourceLoader {
        let mut loader = SourceLoader::new();
        loader.add_source(Path::builtin(), ast::builtin::module());

        for (path, source) in sources {
            assert!(loader.add_source(
                path.parse::<Path>().unwrap_or_else(|e| panic!("{}", e)),
                source,
            ));
        }

        match self {
            Self::Module => {
                assert!(loader.add_source(Path::std(), test_std::prelude_module_for_module_test()));
            }
            Self::Backend => {
                assert!(loader.add_source(Path::std(), test_std::prelude_module_for_backend_test()));
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

    fn run(&self, sources: Vec<Source>, cond: &TestCondition, ctx: &mut TestContext) {
        match self {
            Self::Module => {
                let (modules, errors) =
                    build_modules(sources, Default::default(), &(), &mut ctx.report);
                cond.run(&modules, &errors, ctx);
            }
            Self::Backend | Self::Std => {
                #[cfg(feature = "llvm-backend")]
                {
                    let backend = crate::backend::llvm::Backend::from(BackendOptions::default());
                    self.run_backend(&sources, cond, backend, ctx);
                }
                #[cfg(feature = "chibi-backend")]
                {
                    let backend = crate::backend::chibi::Backend::from(BackendOptions::default());
                    self.run_backend(&sources, cond, backend, ctx);
                }
            }
        }
    }

    fn run_backend<B: Backend + NativeBackend>(
        &self,
        sources: &[Source],
        cond: &TestCondition,
        backend: B,
        ctx: &mut TestContext,
    ) {
        let lowerizer = Lowerizer::new(backend);
        let entry_points = vec![Path::current()].into_iter().collect();
        let (modules, errors) =
            build_modules(sources.to_vec(), entry_points, &lowerizer, &mut ctx.report);
        cond.run(&modules, &errors, ctx);

        if matches!(cond, TestCondition::Pass(_)) {
            let mut backend = lowerizer.complete(&mut ctx.report);
            match backend.execute_main() {
                Ok(true) => {}
                Ok(false) => panic!("{}: Expected #t but got #f", ctx.header()),
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
        errors: &[(Source, ModuleError)],
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
                [(source, error)] => assert!(
                    error_expectation.matches(error),
                    "{}: Expected `{:?}` but got an error:\n{}: {}",
                    ctx.header(),
                    error_expectation,
                    source.path,
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

fn errors_summary(errors: &[(Source, ModuleError)], ctx: &TestContext) -> String {
    errors
        .into_iter()
        .map(|(source, error)| {
            format!(
                "{} (while building {})",
                error.fmt_on(&ctx.source_location_table),
                source.path
            )
        })
        .join("\n")
}
