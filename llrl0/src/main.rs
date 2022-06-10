use llrl::prelude::*;
use std::env;
use std::io;
use std::path;
use std::process::{exit, ExitStatus};

fn main() {
    match run_pipeline() {
        Ok(status) => {
            if let Some(code) = status.and_then(|s| s.code()) {
                exit(code);
            }
        }
        Err(e) => {
            eprintln!("{}", e);
            exit(1);
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error(transparent)]
    PipelineError(#[from] PipelineError),

    #[error(transparent)]
    IoError(#[from] io::Error),
}

pub type Result<T> = std::result::Result<T, Error>;

fn run_pipeline() -> Result<Option<ExitStatus>> {
    let cli_options = CliOptions::new();
    let mut pipeline = Pipeline::new(env::current_dir()?);

    if cli_options.verbose() {
        pipeline.set_verbose(true);
    }

    for (package_name, package_path) in cli_options.packages()? {
        if !pipeline.register_package(package_name.clone(), package_path) {
            Err(io::Error::new(
                io::ErrorKind::Other,
                format!("Cannot register package: {}", package_name),
            ))?;
        }
    }

    for module_name in cli_options.entries()? {
        pipeline.add_entry_path(Path::new(PackageName::current(), module_name));
    }

    if cli_options.optimize() {
        pipeline.set_optimize(true);
    }

    for clang_option in cli_options.clang_options() {
        pipeline.add_clang_option(clang_option);
    }

    let run_args = cli_options.run_args();

    Ok(if let Some(output) = cli_options.output() {
        let run_args = if cli_options.run() || !run_args.is_empty() {
            Some(run_args)
        } else {
            None
        };
        pipeline.run(path::Path::new(output), run_args)?
    } else {
        let tmp_path = tempfile::NamedTempFile::new()?.into_temp_path();
        pipeline.run(&tmp_path, Some(run_args))?
    })
}

pub struct CliOptions<'a> {
    matches: clap::ArgMatches<'a>,
}

impl<'a> CliOptions<'a> {
    fn new() -> Self {
        let matches = clap::App::new("llrl")
            .version("0.1.0")
            .arg(Self::verbose_option())
            .arg(Self::package_option())
            .arg(Self::optimize_option())
            .arg(Self::run_option())
            .arg(Self::output_option())
            .arg(Self::clang_option_option())
            .arg(Self::entry_argument())
            .arg(Self::run_argument())
            .get_matches();
        Self { matches }
    }

    fn verbose_option() -> clap::Arg<'static, 'a> {
        clap::Arg::with_name("verbose")
            .long("verbose")
            .short("v")
            .help("Enables verbose output")
    }

    fn verbose(&self) -> bool {
        self.matches.is_present("verbose")
    }

    fn package_option() -> clap::Arg<'static, 'a> {
        clap::Arg::with_name("package")
            .long("package")
            .short("p")
            .help("Adds a package reference")
            .value_name("PATH or PACKAGE:PATH")
            .multiple(true)
            .number_of_values(1)
    }

    fn packages(&self) -> Result<Vec<(PackageName, &str)>> {
        self.matches
            .values_of("package")
            .unwrap_or_default()
            .map(|value| {
                let (package_name, package_path) = Self::interpret_package_option(value)
                    .ok_or_else(|| {
                        invalid_option(
                            "package",
                            value,
                            "--package must be specified in the form of <PATH or PACKAGE:PATH>",
                        )
                    })?;

                let package_name = PackageName::external(package_name)
                    .map_err(|e| invalid_option("package", value, &e.to_string()))?;

                Ok((package_name, package_path))
            })
            .collect()
    }

    fn interpret_package_option(option: &str) -> Option<(&str, &str)> {
        match option.split(':').collect::<Vec<_>>().as_slice() {
            [package_path] => {
                let path = path::Path::new(*package_path).file_name()?;
                let package_name = path.to_str()?;
                Some((package_name, *package_path))
            }
            [package_name, package_path] => Some((*package_name, *package_path)),
            _ => None,
        }
    }

    fn optimize_option() -> clap::Arg<'static, 'a> {
        clap::Arg::with_name("optimize")
            .long("optimize")
            .short("O")
            .help("Enables optimization")
    }

    fn optimize(&self) -> bool {
        self.matches.is_present("optimize")
    }

    fn run_option() -> clap::Arg<'static, 'a> {
        clap::Arg::with_name("run")
            .long("run")
            .short("r")
            .help("Run immediately")
    }

    fn run(&self) -> bool {
        self.matches.is_present("run")
    }

    fn output_option() -> clap::Arg<'static, 'a> {
        clap::Arg::with_name("output")
            .long("output")
            .short("o")
            .help("Sets the destination of the executable")
            .value_name("OUTPUT")
    }

    fn output(&self) -> Option<&str> {
        self.matches.value_of("output")
    }

    fn clang_option_option() -> clap::Arg<'static, 'a> {
        clap::Arg::with_name("clang_option")
            .long("clang-option")
            .short("c")
            .help("Adds an option to be passed to clang")
            .value_name("OPTION")
            .multiple(true)
            .number_of_values(1)
    }

    fn clang_options(&self) -> Vec<&str> {
        self.matches
            .values_of("clang_option")
            .unwrap_or_default()
            .into_iter()
            .collect()
    }

    fn entry_argument() -> clap::Arg<'static, 'a> {
        clap::Arg::with_name("entry")
            .help("Target to be entry-point")
            .value_name("ENTRY")
            .multiple(true)
            .required(true)
    }

    fn entries(&self) -> Result<Vec<ModuleName>> {
        self.matches
            .values_of("entry")
            .unwrap()
            .map(|value| {
                let module_name = value
                    .strip_suffix(&format!(".{}", llrl::source::SOURCE_CODE_EXTENSION))
                    .unwrap_or(value);

                let module_name = module_name
                    .parse::<ModuleName>()
                    .map_err(|e| invalid_option("entry", value, &e.to_string()))?;

                Ok(module_name)
            })
            .collect()
    }

    fn run_argument() -> clap::Arg<'static, 'a> {
        clap::Arg::with_name("arg")
            .help("Arguments to run")
            .value_name("ARG")
            .multiple(true)
            .last(true)
    }

    fn run_args(&self) -> Vec<&str> {
        self.matches
            .values_of("arg")
            .unwrap_or_default()
            .into_iter()
            .collect()
    }
}

fn invalid_option(name: &str, value: &str, description: &str) -> io::Error {
    io::Error::new(
        io::ErrorKind::Other,
        format!("Invalid option: --{}={}: {}", name, value, description),
    )
}
