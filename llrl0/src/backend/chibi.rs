use super::native::{linking, NativeBackend};
use super::options::Options;
use crate::lowering;
use crate::lowering::ir::*;
use crate::report::{Phase, Report};
use itertools::Itertools;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use xten::asm::Object;
use xten::elf::write_relocatable_object;

mod codegen;
mod context;
mod executor;

use context::{Context, FunctionSymbol};
use executor::Executor;

pub struct Backend {
    verbose: bool,
    report: Report,
    executor: Executor,
    context: Context,
    queued_defs: HashMap<CtId, Arc<Def>>,
    queued_main: Vec<Init>,
    objects: Vec<Object>,
}

impl Backend {
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

    fn codegen(&mut self, codegen_llrl_main: bool) {
        let defs = std::mem::take(&mut self.queued_defs);
        let main = codegen_llrl_main.then(|| Function::main(std::mem::take(&mut self.queued_main)));
        if defs.is_empty() && main.is_none() {
            return;
        }

        self.report.on(Phase::Codegen, || {
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
        });
    }
}

impl lowering::Backend for Backend {
    fn put_def(&mut self, id: CtId, def: Arc<Def>) {
        self.queued_defs.insert(id, def);
    }

    fn put_main(&mut self, init: Init) {
        self.queued_main.push(init);
    }

    fn execute_macro(&mut self, id: CtId, s: Syntax<Sexp>) -> Result<Syntax<Sexp>, String> {
        self.codegen(false);

        match self.context.function_symbol(id) {
            Some(f) => self
                .report
                .on(Phase::JIT, || self.executor.call_macro(f, s)),
            None => Err(format!("macro not found: {}", id)),
        }
    }

    fn complete(self, report: &mut Report) {
        report.merge(&self.report);
    }
}

impl NativeBackend for Backend {
    fn produce_executable(
        &mut self,
        dest: PathBuf,
        clang_options: Vec<String>,
    ) -> Result<(), String> {
        self.codegen(true);

        let mut objects = std::mem::take(&mut self.objects);

        objects.push(codegen::c_main_adapter_object(&mut self.context).unwrap());

        let result = self.report.on(Phase::Link, || {
            linking::link(
                &dest,
                objects,
                |path, obj| write_relocatable_object(path, obj).unwrap(),
                clang_options.iter().map(|o| o.as_str()),
            )
        })?;

        if self.verbose {
            eprintln!("### produce executable");
            eprintln!("{:?}", result);
        }
        Ok(())
    }

    fn execute_main(&mut self) -> std::result::Result<bool, String> {
        self.codegen(true);

        match self.context.main_function_symbol() {
            Some(f) => Ok(self.report.on(Phase::JIT, || self.executor.call_main(f))),
            None => Err("main not found".to_string()),
        }
    }
}

impl From<Options> for Backend {
    fn from(options: Options) -> Self {
        Self::new(options)
    }
}
