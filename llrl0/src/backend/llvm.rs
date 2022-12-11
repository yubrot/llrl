use super::native::{execution, linking, NativeBackend};
use super::options::Options;
use crate::lowering;
use crate::lowering::ir::*;
use crate::report::{Phase, Report};
use crossbeam_channel::{bounded, unbounded, Receiver, Sender};
use itertools::Itertools;
use llvm::prelude::*;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

mod artifact;
mod codegen;
mod executor;
mod optimizer;
mod runtime;

use artifact::*;
use executor::Executor;
use optimizer::Optimizer;

#[derive(Debug)]
pub struct Backend {
    sender: Sender<Request>,
    handle: execution::JoinHandle<Report>,
}

impl Backend {
    fn new(options: Options) -> Self {
        let (sender, receiver) = unbounded();
        let handle = execution::dedicated_thread().run(move || process_requests(options, receiver));
        Self { sender, handle }
    }
}

impl lowering::Backend for Backend {
    fn put_def(&mut self, id: CtId, def: Arc<Def>) {
        self.sender.send(Request::PutDef(id, def)).unwrap();
    }

    fn put_main(&mut self, init: Init) {
        self.sender.send(Request::PutMain(init)).unwrap();
    }

    fn execute_macro(&mut self, id: CtId, s: Syntax<Sexp>) -> Result<Syntax<Sexp>, String> {
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

impl NativeBackend for Backend {
    fn produce_executable(&self, dest: PathBuf, clang_options: Vec<String>) -> Result<(), String> {
        let (sender, receiver) = bounded(0);
        self.sender
            .send(Request::ProduceExecutable(dest, clang_options, sender))
            .unwrap();
        receiver.recv().unwrap()
    }

    fn execute_main(&mut self) -> std::result::Result<bool, String> {
        let (sender, receiver) = bounded(0);
        self.sender.send(Request::ExecuteMain(sender)).unwrap();
        receiver.recv().unwrap()
    }
}

impl From<Options> for Backend {
    fn from(options: Options) -> Self {
        Self::new(options)
    }
}

#[derive(Debug)]
enum Request {
    PutDef(CtId, Arc<Def>),
    PutMain(Init),
    ExecuteMacro(CtId, Syntax<Sexp>, Sender<Result<Syntax<Sexp>, String>>),
    ExecuteMain(Sender<Result<bool, String>>),
    ProduceExecutable(PathBuf, Vec<String>, Sender<Result<(), String>>),
}

fn process_requests(options: Options, receiver: Receiver<Request>) -> Report {
    let context = LLVMContext::new();
    let mut builder = Builder::new(&context, options);

    while let Ok(request) = receiver.recv() {
        match request {
            Request::PutDef(id, def) => builder.put_def(id, def),
            Request::PutMain(init) => builder.put_main(init),
            Request::ExecuteMacro(id, sexp, sender) => {
                let _ = sender.send(builder.execute_macro(id, sexp));
            }
            Request::ExecuteMain(sender) => {
                let _ = sender.send(builder.execute_main());
            }
            Request::ProduceExecutable(dest, clang_options, sender) => {
                let _ = sender.send(builder.produce_executable(dest, clang_options));
            }
        }
    }

    builder.report
}

#[derive(Debug)]
struct Builder<'ctx> {
    verbose: bool,
    report: Report,
    executor: Executor<'ctx>,
    optimizer: Optimizer,
    artifact: ContextArtifact<'ctx>,
    queued_defs: HashMap<CtId, Arc<Def>>,
    queued_main: Vec<Init>,
    generation: i32,
}

impl<'ctx> Builder<'ctx> {
    fn new(ctx: &'ctx LLVMContext, options: Options) -> Self {
        let verbose = options.verbose;
        let report = Report::new();
        let opt_level = options.optimize.map(|opt| match opt {
            true => llvm::OptLevel::Default,
            false => llvm::OptLevel::None,
        });
        let executor = Executor::new(ctx, opt_level, None);
        let optimizer = Optimizer::new(opt_level);
        let artifact = ContextArtifact::new(ctx, executor.data_layout());

        Self {
            verbose,
            report,
            executor,
            optimizer,
            artifact,
            queued_defs: HashMap::new(),
            queued_main: Vec::new(),
            generation: 0,
        }
    }

    fn put_def(&mut self, id: CtId, def: Arc<Def>) {
        self.queued_defs.insert(id, def);
    }

    fn put_main(&mut self, init: Init) {
        self.queued_main.push(init);
    }

    fn codegen(&mut self, codegen_llrl_main: bool) {
        let defs = std::mem::take(&mut self.queued_defs);
        let main = codegen_llrl_main.then(|| Function::main(std::mem::take(&mut self.queued_main)));
        if defs.is_empty() && main.is_none() {
            return;
        }

        self.report.on(Phase::Codegen, || {
            self.generation += 1;
            let name = format!("gen{}", self.generation);

            self.artifact.add_types(&defs);
            let module = LLVMModule::new(&name, self.artifact.context());
            module.set_target_triple(&llvm::get_default_target_triple());
            module.set_data_layout(self.artifact.data_layout());
            let mut module_artifact = ModuleArtifact::new(&module);

            if self.verbose {
                eprintln!("### adding defs");
                for (id, def) in defs.iter().sorted_by_key(|(k, _)| **k) {
                    eprintln!("{} = {}", id, def);
                }
            }
            module_artifact.add_functions(&defs, &mut self.artifact);
            if self.verbose {
                eprintln!("### added defs");
                eprintln!("{}", module);
            }

            if let Some(main) = main {
                if self.verbose {
                    eprintln!("### adding main");
                    eprintln!("{}", main);
                }
                module_artifact.add_main(main, &mut self.artifact);
                if self.verbose {
                    eprintln!("### added main");
                    eprintln!("{}", module_artifact.main_function().unwrap().value);
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

            self.executor.add_module(module);
        });
    }

    fn execute_macro(&mut self, id: CtId, sexp: Syntax<Sexp>) -> Result<Syntax<Sexp>, String> {
        self.codegen(false);

        match self.artifact.function_symbol(id) {
            Some(f) => self
                .report
                .on(Phase::JIT, || self.executor.call_macro(f, sexp)),
            None => Err(format!("macro not found: {}", id)),
        }
    }

    fn execute_main(&mut self) -> Result<bool, String> {
        self.codegen(true);

        match self.artifact.main_function_symbol() {
            Some(f) => Ok(self.report.on(Phase::JIT, || self.executor.call_main(f))),
            None => Err("main not found".to_string()),
        }
    }

    fn produce_executable(
        &mut self,
        dest: PathBuf,
        clang_options: Vec<String>,
    ) -> Result<(), String> {
        self.codegen(true);
        let mut modules = self.executor.remove_modules();

        modules.push({
            let module = LLVMModule::new("c_main_adapter", self.artifact.context());
            module.set_target_triple(&llvm::get_default_target_triple());
            module.set_data_layout(self.artifact.data_layout());
            let mut module_artifact = ModuleArtifact::new(&module);
            module_artifact.add_c_main_adapter(&self.artifact);
            module
        });

        let result = self.report.on(Phase::Link, || {
            linking::link(
                &dest,
                modules,
                |path, m| {
                    self.executor
                        .target_machine()
                        .emit_to_file(&m, path.to_str().unwrap(), llvm::FileType::ObjectFile)
                        .unwrap()
                },
                clang_options.iter().map(|o| o.as_str()),
            )
        })?;

        if self.verbose {
            eprintln!("### produce executable");
            eprintln!("{:?}", result);
        }
        Ok(())
    }
}
