use crate::formatting::ContextualDisplay as _;
use crate::prelude::*;
use itertools::Itertools;
use std::fmt;
use std::path;
use std::process::{Command, ExitStatus};

#[derive(thiserror::Error, Debug, Clone)]
pub enum Error {
    Code(Vec<(Path, CodeError)>),
    CircularDependencies(Vec<Path>),
    Module(SourceLocationTable, Vec<(Code, ModuleError)>),
    Clang(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Code(ref errors) => {
                for (path, error) in errors {
                    writeln!(f, "{}: {}", path, error)?;
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
                for (code, error) in errors {
                    writeln!(f, "{} (while building {})", error.fmt_on(table), code.path)?;
                }
            }
            Self::Clang(ref error) => {
                writeln!(f, "Failed to compile with clang: {}", error)?;
            }
        }
        Ok(())
    }
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone)]
pub struct Pipeline {
    code_loader: CodeLoader,
    entry_code_paths: Vec<Path>,
    clang_options: Vec<String>,
    optimize: bool,
    verbose: bool,
}

impl Pipeline {
    pub fn new(current_path: impl Into<path::PathBuf>) -> Self {
        let mut code_loader = CodeLoader::new();
        code_loader.add_package(PackageName::builtin(), ast::builtin::module());
        code_loader.add_package(PackageName::std(), llstd::modules());
        code_loader.add_package(PackageName::current(), current_path.into());

        Self {
            code_loader,
            entry_code_paths: Vec::new(),
            clang_options: Vec::new(),
            optimize: false,
            verbose: false,
        }
    }

    pub fn register_package(&mut self, name: PackageName, path: impl Into<path::PathBuf>) -> bool {
        self.code_loader.add_package(name, path.into())
    }

    pub fn add_entry_path(&mut self, path: Path) {
        self.entry_code_paths.push(path);
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

    pub fn run(
        self,
        output: &path::Path,
        run_args: Option<Vec<&str>>,
    ) -> Result<Option<ExitStatus>> {
        let mut source_location_table = SourceLocationTable::new();
        let mut report = Report::new();

        let codes = collect_codes(
            self.entry_code_paths.iter(),
            &self.code_loader,
            &mut source_location_table,
            &mut report,
        );
        let code_errors = codes
            .errors()
            .map(|(path, e)| (path.clone(), e.clone()))
            .collect::<Vec<_>>();

        if !code_errors.is_empty() {
            return Err(Error::Code(code_errors));
        }

        let codes = codes
            .resolve_dependencies_order()
            .map_err(Error::CircularDependencies)?;

        if self.verbose {
            eprintln!("### collected codes");
            for (index, code) in codes.iter().enumerate() {
                if code.dependencies.is_empty() {
                    eprintln!("[{}] {}", index, code.path);
                } else {
                    eprintln!(
                        "[{}] {} -> {}",
                        index,
                        code.path,
                        code.dependencies.values().format(", ")
                    );
                }
            }
        }

        let emitter = Emitter::new(LLVMBackend::new(
            LLVMBackendOptions::new()
                .optimize(self.optimize)
                .verbose(self.verbose),
        ));

        let entry_points = self.entry_code_paths.into_iter().collect();
        let (_, module_errors) = build_modules(codes, entry_points, &emitter, &mut report);

        if !module_errors.is_empty() {
            return Err(Error::Module(source_location_table, module_errors));
        }

        let backend = emitter.complete(&mut report);
        let produce_result = backend.produce_executable(output.to_owned(), self.clang_options);

        if self.verbose {
            let stdout = String::from_utf8_lossy(&produce_result.stdout);
            eprintln!("### clang output");
            eprintln!("{}", stdout);
        }

        if !produce_result.status.success() {
            let stderr = String::from_utf8_lossy(&produce_result.stderr);
            Err(Error::Clang(stderr.into_owned()))?;
        }

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
