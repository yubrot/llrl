use super::native::execution;
use super::Options;
use crate::lowering;
use crate::lowering::ir::*;
use crate::report::{Phase, Report};
use crossbeam_channel::{bounded, unbounded, Receiver, Sender};
use itertools::Itertools;
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::PathBuf;
use std::process::Command;
use std::sync::Arc;
use xten::asm::Object;
use xten::elf::into_relocatable_object;

mod codegen;
mod context;
mod executor;

use context::{Context, FunctionSymbol};
use executor::Executor;

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

impl super::ExecuteMain for Backend {
    fn execute_main(&mut self) -> std::result::Result<bool, String> {
        let (sender, receiver) = bounded(0);
        self.sender.send(Request::ExecuteMain(sender)).unwrap();
        receiver.recv().unwrap()
    }
}

impl super::ProduceExecutable for Backend {
    fn produce_executable(
        &self,
        dest: PathBuf,
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
    PutDef(CtId, Arc<Def>),
    PutMain(Init),
    ExecuteMacro(CtId, Syntax<Sexp>, Sender<Result<Syntax<Sexp>, String>>),
    ExecuteMain(Sender<Result<bool, String>>),
    ProduceExecutable(PathBuf, Vec<String>, Sender<Result<String, String>>),
}

fn process_requests(options: Options, receiver: Receiver<Request>) -> Report {
    let mut builder = Builder::new(options);

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

struct Builder {
    verbose: bool,
    report: Report,
    executor: Executor,
    context: Context,
    queued_defs: HashMap<CtId, Arc<Def>>,
    queued_main: Vec<Init>,
    objects: Vec<Object>,
}

impl Builder {
    fn new(options: Options) -> Self {
        let verbose = options.verbose;
        let report = Report::new();
        let executor = Executor::new();
        let context = Context::new();

        Self {
            verbose,
            report,
            executor,
            context,
            queued_defs: HashMap::new(),
            queued_main: Vec::new(),
            objects: Vec::new(),
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

        if !defs.is_empty() || main.is_some() {
            self.report.enter_phase(Phase::Codegen);

            self.context.add_types(&defs);

            if self.verbose {
                eprintln!("### codegen");
                for (id, def) in defs.iter().sorted_by_key(|(k, _)| **k) {
                    eprintln!("{} = {}", id, def);
                }
                if let Some(ref main) = main {
                    eprintln!("{}", main);
                }
            }

            let obj = codegen::object(&defs, main.as_ref(), &mut self.context).unwrap();

            if self.verbose {
                eprintln!("### codegen result");
                eprintln!("{:?}", obj);
            }

            self.executor.add_object(&obj);
            self.objects.push(obj);

            self.report.leave_phase(Phase::Codegen);
        }
    }

    fn execute_macro(&mut self, id: CtId, sexp: Syntax<Sexp>) -> Result<Syntax<Sexp>, String> {
        self.codegen(false);

        match self.context.function_symbol(id) {
            Some(f) => {
                self.report.enter_phase(Phase::JIT);
                let ret = self.executor.call_macro(f, sexp);
                self.report.leave_phase(Phase::JIT);
                ret
            }
            None => Err(format!("macro not found: {}", id)),
        }
    }

    fn execute_main(&mut self) -> Result<bool, String> {
        self.codegen(true);

        match self.context.main_function_symbol() {
            Some(f) => {
                self.report.enter_phase(Phase::JIT);
                let result = self.executor.call_main(f);
                self.report.leave_phase(Phase::JIT);
                Ok(result)
            }
            None => Err("main not found".to_string()),
        }
    }

    fn produce_executable(
        &mut self,
        dest: PathBuf,
        clang_options: Vec<String>,
    ) -> Result<String, String> {
        self.codegen(true);
        let mut objects = std::mem::take(&mut self.objects);

        objects.push(codegen::c_main_adapter_object(&mut self.context).unwrap());

        self.report.enter_phase(Phase::Finalize);
        let tmp_dir = tempfile::TempDir::new().unwrap();
        // TODO: Not every function in every module is needed for the main.
        // Rather, most of them exist for macros. This can be stripped.
        let objects = objects
            .into_iter()
            .enumerate()
            .map(|(i, obj)| {
                let name = format!("{}.o", i);
                let path = tmp_dir.path().join(&name);
                let mut file = BufWriter::new(File::create(&path).unwrap());
                let obj = into_relocatable_object(&name, obj);
                obj.write(&mut file).unwrap();
                path
            })
            .collect::<Vec<_>>();

        File::create(&tmp_dir.path().join("libllrt.a"))
            .unwrap()
            .write_all(llrt::ARCHIVE)
            .unwrap();

        let mut clang_command = Command::new("clang");
        clang_command
            .arg("-o")
            .arg(&dest)
            .args(&objects)
            .args([
                "-lgc",
                "-lm",
                &format!("-L{}", tmp_dir.path().display()),
                "-lllrt",
            ])
            .args(&clang_options);

        if self.verbose {
            eprintln!("### link");
            eprintln!("{:?}", clang_command);
        }

        let output = clang_command.output().expect("Failed to execute clang");
        self.report.leave_phase(Phase::Finalize);

        if output.status.success() {
            Ok(String::from_utf8_lossy(&output.stdout).into_owned())
        } else {
            Err(String::from_utf8_lossy(&output.stderr).into_owned())
        }
    }
}
