use crate::formatting::ContextualDisplay as _;
use crate::prelude::*;
use itertools::Itertools;
use std::fmt;
use std::path;
use std::process::{Command, ExitStatus};

#[derive(thiserror::Error, Debug, Clone)]
pub enum Error {
    Source(SourceLocationTable, Vec<(Path, SourceError)>),
    CircularDependencies(Vec<Path>),
    Module(SourceLocationTable, Vec<(Source, ModuleError)>),
    ProduceExecutable(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Source(ref table, ref errors) => {
                for (path, error) in errors {
                    writeln!(f, "{}: {}", path, error.fmt_on(table))?;
                }
            }
            Self::CircularDependencies(ref paths) => {
                writeln!(f, "Circular dependencies detected:")?;
                for path in paths {
                    writeln!(f, "- {}", path)?;
                }
                writeln!(f, "Modules cannot import each other.")?;
            }
            Self::Module(ref table, ref errors) => {
                for (source, error) in errors {
                    writeln!(
                        f,
                        "{} (while building {})",
                        error.fmt_on(table),
                        source.path
                    )?;
                }
            }
            Self::ProduceExecutable(ref error) => {
                writeln!(f, "Failed to produce executable: {}", error)?;
            }
        }
        Ok(())
    }
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone)]
pub struct Pipeline {
    source_loader: SourceLoader,
    preprocessor: Preprocessor,
    entry_source_paths: Vec<Path>,
    clang_options: Vec<String>,
    optimize: bool,
    verbose: bool,
}

impl Pipeline {
    pub fn new(current_path: impl Into<path::PathBuf>) -> Self {
        let mut source_loader = SourceLoader::new();
        source_loader.add_package(PackageName::builtin(), ast::builtin::module());
        source_loader.add_package(PackageName::std(), llstd::modules());
        source_loader.add_package(PackageName::current(), current_path.into());

        Self {
            source_loader,
            preprocessor: Preprocessor::new(),
            entry_source_paths: Vec::new(),
            clang_options: Vec::new(),
            optimize: false,
            verbose: false,
        }
    }

    pub fn enable_feature(&mut self, feature: impl Into<String>) {
        self.preprocessor.enable_feature(feature);
    }

    pub fn register_package(&mut self, name: PackageName, path: impl Into<path::PathBuf>) -> bool {
        self.source_loader.add_package(name, path.into())
    }

    pub fn add_entry_path(&mut self, path: Path) {
        self.entry_source_paths.push(path);
    }

    pub fn set_optimize(&mut self, optimize: bool) {
        self.optimize = optimize;
    }

    pub fn set_verbose(&mut self, verbose: bool) {
        self.verbose = verbose;
    }

    pub fn add_clang_option(&mut self, clang_option: &str) {
        self.clang_options.push(clang_option.to_string());
    }

    pub fn run<B: Backend + NativeBackend>(
        self,
        output: &path::Path,
        run_args: Option<Vec<&str>>,
    ) -> Result<Option<ExitStatus>>
    where
        B: Backend + NativeBackend + From<BackendOptions>,
    {
        let output = path::Path::new(".").join(output);
        let mut source_location_table = SourceLocationTable::new();
        let mut report = Report::new();

        let sources = collect_sources(
            self.entry_source_paths.iter(),
            &self.source_loader,
            &mut source_location_table,
            &self.preprocessor,
            &mut report,
        );
        let source_errors = sources
            .errors()
            .map(|(path, e)| (path.clone(), e.clone()))
            .collect::<Vec<_>>();

        if !source_errors.is_empty() {
            return Err(Error::Source(source_location_table, source_errors));
        }

        let sources = sources
            .resolve_dependencies_order()
            .map_err(Error::CircularDependencies)?;

        if self.verbose {
            eprintln!("### collected sources");
            for (index, source) in sources.iter().enumerate() {
                if source.dependencies.is_empty() {
                    eprintln!("[{}] {}", index, source.path);
                } else {
                    eprintln!(
                        "[{}] {} -> {}",
                        index,
                        source.path,
                        source.dependencies.values().format(", ")
                    );
                }
            }
        }

        let lowerizer = Lowerizer::new(B::from(
            BackendOptions::default()
                .optimize(self.optimize)
                .verbose(self.verbose),
        ));

        let entry_points = self.entry_source_paths.into_iter().collect();
        let (_, module_errors) = build_modules(sources, entry_points, &lowerizer, &mut report);

        if !module_errors.is_empty() {
            return Err(Error::Module(source_location_table, module_errors));
        }

        let backend = lowerizer.complete(&mut report);
        backend
            .produce_executable(output.to_owned(), self.clang_options)
            .map_err(Error::ProduceExecutable)?;

        backend.complete(&mut report);

        if self.verbose {
            eprintln!("### pipeline report");
            eprintln!("{}", report);
        }

        if let Some(run_args) = run_args {
            let status = Command::new(output)
                .args(run_args)
                .status()
                .expect("Failed to spawn process");

            Ok(Some(status))
        } else {
            Ok(None)
        }
    }
}
