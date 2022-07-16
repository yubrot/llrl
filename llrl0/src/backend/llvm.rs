use super::native::context;
use super::Options;
use crate::lowering::{self, ir};
use crate::report::{Phase, Report};
use crossbeam_channel::{bounded, unbounded, Receiver, Sender};
use itertools::Itertools;
use llvm::prelude::*;
use std::collections::HashMap;
use std::fs;
use std::io::Write;
use std::path;
use std::process;
use std::sync::Arc;

mod artifact;
mod codegen;
mod executor;
mod optimizer;
pub mod runtime;

pub use artifact::*;
pub use executor::Executor;
pub use optimizer::Optimizer;

#[derive(Debug)]
pub struct Backend {
    sender: Sender<Request>,
    handle: context::JoinHandle<Report>,
}

impl Backend {
    fn new(options: Options) -> Self {
        let (sender, receiver) = unbounded();
        let handle = context::dedicated_thread().run(move || process_requests(options, receiver));
        Self { sender, handle }
    }
}

impl lowering::Backend for Backend {
    fn put_def(&mut self, id: ir::CtId, def: Arc<ir::CtDef>) {
        self.sender.send(Request::PutDef(id, def)).unwrap();
    }

    fn put_main(&mut self, init: ir::Init) {
        self.sender.send(Request::PutMain(init)).unwrap();
    }

    fn execute_main(&mut self) -> Result<bool, String> {
        let (sender, receiver) = bounded(0);
        self.sender.send(Request::ExecuteMain(sender)).unwrap();
        receiver.recv().unwrap()
    }

    fn execute_macro(
        &mut self,
        id: ir::CtId,
        s: ir::Syntax<ir::Sexp>,
    ) -> Result<ir::Syntax<ir::Sexp>, String> {
        let (sender, receiver) = bounded(0);
        self.sender
            .send(Request::ExecuteMacro(id, s, sender))
            .unwrap();
        receiver.recv().unwrap()
    }

    fn complete(self, report: &mut Report) {
        drop(self.sender);
        report.merge(&self.handle.join());
    }
}

impl super::ProduceExecutable for Backend {
    fn produce_executable(
        &self,
        dest: path::PathBuf,
        clang_options: Vec<String>,
    ) -> Result<String, String> {
        let (sender, receiver) = bounded(0);
        self.sender
            .send(Request::ProduceExecutable(dest, clang_options, sender))
            .unwrap();
        receiver.recv().unwrap()
    }
}

impl From<Options> for Backend {
    fn from(options: super::Options) -> Self {
        Self::new(options)
    }
}

#[derive(Debug)]
enum Request {
    PutDef(ir::CtId, Arc<ir::CtDef>),
    PutMain(ir::Init),
    ExecuteMacro(
        ir::CtId,
        ir::Syntax<ir::Sexp>,
        Sender<Result<ir::Syntax<ir::Sexp>, String>>,
    ),
    ExecuteMain(Sender<Result<bool, String>>),
    ProduceExecutable(path::PathBuf, Vec<String>, Sender<Result<String, String>>),
}

fn process_requests(options: Options, receiver: Receiver<Request>) -> Report {
    let opt_level = options.optimize.map(|opt| match opt {
        true => llvm::OptLevel::Default,
        false => llvm::OptLevel::None,
    });

    let mut report = Report::new();

    let context = LLVMContext::new();
    let module_builder = ModuleBuilder::new(opt_level, options.verbose);
    let mut executor = Executor::new(&context, opt_level, None);

    let mut artifact = ContextArtifact::new(&context, executor.data_layout());
    let mut defs = HashMap::new();
    let mut main = Vec::new();
    let mut generation = 0;

    while let Ok(request) = receiver.recv() {
        match request {
            Request::PutDef(id, def) => {
                defs.insert(id, def);
            }
            Request::PutMain(init) => {
                main.push(init);
            }
            Request::ExecuteMacro(id, sexp, sender) => {
                generation += 1;

                if !defs.is_empty() {
                    report.enter_phase(Phase::Codegen);
                    let module = module_builder.build(
                        &format!("gen{}", generation),
                        &mut artifact,
                        std::mem::take(&mut defs),
                        None,
                    );
                    report.leave_phase(Phase::Codegen);

                    executor.add_module(module);
                }

                let ret = match artifact.function_symbol(id) {
                    Some(f) => {
                        report.enter_phase(Phase::JIT);
                        let ret = executor.call_macro(f, sexp);
                        report.leave_phase(Phase::JIT);
                        ret
                    }
                    None => Err(format!("macro not found: {}", id)),
                };

                let _ = sender.send(ret);
            }
            Request::ExecuteMain(sender) => {
                if !defs.is_empty() || !main.is_empty() {
                    report.enter_phase(Phase::Codegen);
                    let module = module_builder.build(
                        "entry",
                        &mut artifact,
                        std::mem::take(&mut defs),
                        Some(std::mem::take(&mut main)),
                    );
                    report.leave_phase(Phase::Codegen);

                    executor.add_module(module);
                }

                report.enter_phase(Phase::JIT);
                let ret = match artifact.main_function_symbol() {
                    Some(f) => Ok(executor.call_main(f)),
                    None => Err("main not found".to_string()),
                };
                report.leave_phase(Phase::JIT);

                let _ = sender.send(ret);
            }
            Request::ProduceExecutable(dest, clang_options, sender) => {
                main.push(ir::Init::new(
                    ir::Ct::S(32),
                    ir::Rt::Const(ir::Const::Integer(ir::Ct::S(32), true, 0)),
                ));
                generation += 1;

                let mut modules = executor.remove_modules();

                report.enter_phase(Phase::Codegen);
                modules.push(module_builder.build(
                    "entry",
                    &mut artifact,
                    std::mem::take(&mut defs),
                    Some(std::mem::take(&mut main)),
                ));
                report.leave_phase(Phase::Codegen);

                report.enter_phase(Phase::Finalize);
                let tmp_dir = tempfile::TempDir::new().unwrap();
                // TODO: Not every function in every module is needed for the main,
                // rather most of them are there for macros. This can be stripped.
                let objects = modules
                    .iter()
                    .enumerate()
                    .map(|(index, module)| {
                        let path = tmp_dir
                            .path()
                            .join(format!("{}.o", index))
                            .to_string_lossy()
                            .into_owned();
                        executor
                            .target_machine()
                            .emit_to_file(module, &path, llvm::FileType::ObjectFile)
                            .unwrap_or_else(|e| panic!("{}", e));
                        path
                    })
                    .collect::<Vec<_>>();

                fs::File::create(&tmp_dir.path().join("libllrt.a"))
                    .unwrap()
                    .write_all(llrt::ARCHIVE)
                    .unwrap();

                let mut clang_command = process::Command::new("clang");
                clang_command
                    .arg("-o")
                    .arg(&dest)
                    .args(&objects)
                    .args(&[
                        "-lgc",
                        "-lm",
                        &format!("-L{}", tmp_dir.path().display()),
                        "-lllrt",
                    ])
                    .args(&clang_options);

                if options.verbose {
                    eprintln!("### link");
                    eprintln!("{:?}", clang_command);
                }

                let output = clang_command.output().expect("Failed to execute clang");
                report.leave_phase(Phase::Finalize);

                let result = if output.status.success() {
                    Ok(String::from_utf8_lossy(&output.stdout).into_owned())
                } else {
                    Err(String::from_utf8_lossy(&output.stderr).into_owned())
                };
                let _ = sender.send(result);
            }
        }
    }

    report
}

#[derive(Debug)]
struct ModuleBuilder {
    optimizer: Optimizer,
    verbose: bool,
}

impl ModuleBuilder {
    fn new(opt_level: Option<llvm::OptLevel>, verbose: bool) -> Self {
        Self {
            optimizer: Optimizer::new(opt_level),
            verbose,
        }
    }

    fn build<'ctx>(
        &self,
        name: &str,
        ctx_artifact: &mut ContextArtifact<'ctx>,
        defs: HashMap<ir::CtId, Arc<ir::CtDef>>,
        main: Option<Vec<ir::Init>>,
    ) -> LLVMBox<LLVMModule<'ctx>> {
        ctx_artifact.add_types(&defs);

        let module = LLVMModule::new(name, ctx_artifact.context());
        module.set_target_triple(&llvm::get_default_target_triple());
        module.set_data_layout(ctx_artifact.data_layout());
        let mut artifact = ModuleArtifact::new(&module);

        if self.verbose {
            eprintln!("### adding defs");
            for (id, def) in defs.iter().sorted_by_key(|(k, _)| **k) {
                eprintln!("{} = {}", id, def);
            }
        }
        artifact.add_functions(&defs, ctx_artifact);
        if self.verbose {
            eprintln!("### added defs");
            eprintln!("{}", module);
        }

        if let Some(main) = main {
            if self.verbose {
                eprintln!("### adding main");
                for init in main.iter() {
                    eprintln!("{}", init);
                }
            }
            artifact.add_main(main, ctx_artifact);
            if self.verbose {
                eprintln!("### added main");
                eprintln!("{}", artifact.main_function().unwrap().value);
            }
        }

        if let Err(msg) = module.verify() {
            panic!("LLVM verify module failed: {}", msg);
        }

        self.optimizer.run(&module);
        if self.verbose {
            eprintln!("### optimized");
            eprintln!("{}", module);
        }

        module
    }
}
