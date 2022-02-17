use crate::ast;
use crate::module::{Backend as ModuleBackend, Module, ModuleId};
use crate::report::{Phase, Report};
use crossbeam_channel::{bounded, unbounded, Receiver, RecvError, Sender, TryRecvError};
use derive_new::new;
use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt;
use std::sync::Arc;
use std::thread;

mod branch_expander;
mod context;
mod data_expander;
mod display;
mod heap2stack;
pub mod ir;
mod normalizer;
mod rewriter;
mod simplifier;
mod traverser;
mod value;

pub use context::Context;
pub use value::Value;

pub trait Backend: Send + 'static {
    type Value: BackendValue;

    fn put_def(&mut self, id: ir::CtId, def: Arc<ir::CtDef>);

    fn put_main(&mut self, init: ir::Init);

    fn execute_main(&mut self) -> Result<Self::Value, String>;

    fn execute_function(
        &mut self,
        id: ir::CtId,
        args: Vec<Self::Value>,
    ) -> Result<Self::Value, String>;

    fn complete(self, report: &mut Report);
}

pub trait BackendValue: Sized + Send + fmt::Display {
    fn as_bool(&self) -> Option<bool>;

    fn from_macro_src(sexp: &ir::Syntax<ir::Sexp>) -> Self;

    fn into_macro_dest(self) -> Result<ir::Syntax<ir::Sexp>, String>;
}

#[derive(Debug)]
pub struct Emitter<B: Backend> {
    sender: Sender<Request<B>>,
    handle: thread::JoinHandle<(B, Report)>,
}

impl<B: Backend> Emitter<B> {
    pub fn new(backend: B) -> Self {
        let (sender, receiver) = unbounded();
        let handle = thread::spawn(move || process_requests(backend, receiver));
        Self { sender, handle }
    }

    pub fn complete(self, report: &mut Report) -> B {
        drop(self.sender);
        let (result, emitter_report) = self.handle.join().unwrap();
        report.merge(&emitter_report);
        result
    }
}

impl<B: Backend> ModuleBackend for Emitter<B> {
    fn add_module(&self, module: Arc<Module>, is_entry_point: bool) {
        let request = Request::AddModule(module, is_entry_point);
        self.sender.send(request).unwrap();
    }

    fn execute_macro(
        &self,
        id: ast::NodeId<ast::Macro>,
        s: &ir::Syntax<ir::Sexp>,
    ) -> Result<ir::Syntax<ir::Sexp>, String> {
        let (sender, receiver) = bounded(0);
        let request = Request::ExecuteMacro(id, B::Value::from_macro_src(s), sender);
        self.sender.send(request).unwrap();
        let response = receiver.recv().unwrap();
        response.and_then(|value| value.into_macro_dest())
    }
}

#[derive(Debug)]
enum Request<B: Backend> {
    AddModule(Arc<Module>, bool),
    ExecuteMacro(
        ast::NodeId<ast::Macro>,
        B::Value,
        Sender<Result<B::Value, String>>,
    ),
}

fn process_requests<B: Backend>(mut backend: B, receiver: Receiver<Request<B>>) -> (B, Report) {
    let mut ctx = Context::new();
    let mut modules = HashMap::new();
    let mut main = MainStatements::new(HashSet::new(), VecDeque::new());
    let mut report = Report::new();

    fn populate<T, B>(
        ctx: &mut Context,
        report: &mut Report,
        backend: &mut B,
        src: &T,
        modules: &HashMap<ModuleId, Arc<Module>>,
    ) -> T::Dest
    where
        T: simplifier::Simplify,
        T::Dest: traverser::Traverse + rewriter::Rewrite,
        B: Backend,
    {
        report.enter_phase(Phase::Emit);
        let (result, defs) = ctx.populate(&src, &modules);
        report.leave_phase(Phase::Emit);

        for (id, def) in defs {
            backend.put_def(id, Arc::clone(def));
        }
        result
    }

    while match if main.is_empty() {
        // There are no scheduled tasks, recv with blocking
        match receiver.recv() {
            Ok(request) => Ok(request),
            Err(RecvError) => Err(true),
        }
    } else {
        // There is a scheduled task, recv without blocking
        match receiver.try_recv() {
            Ok(request) => Ok(request),
            Err(TryRecvError::Empty) => Err(false),
            Err(TryRecvError::Disconnected) => Err(true),
        }
    } {
        // Consume the received request
        Ok(request) => match request {
            Request::AddModule(module, is_entry_point) => {
                let mid = module.id();
                modules.insert(mid, module);
                if is_entry_point {
                    main.enqueue(mid, &modules);
                }
                true
            }
            Request::ExecuteMacro(macro_id, s, response_sender) => {
                let f = populate(&mut ctx, &mut report, &mut backend, &macro_id, &modules);
                let result = backend.execute_function(f.id(), vec![s]);
                let _ = response_sender.send(result);
                true
            }
        },
        // There are no request at the moment, consume a scheduled task
        Err(complete) => match main.dequeue() {
            Some(init) => {
                if let Some(init) = populate(&mut ctx, &mut report, &mut backend, &init, &modules) {
                    backend.put_main(init);
                }
                true
            }
            None => !complete,
        },
    } {}

    (backend, report)
}

#[derive(Debug, new)]
struct MainStatements {
    visited: HashSet<ModuleId>,
    queue: VecDeque<ast::InitExpr>,
}

impl MainStatements {
    fn is_empty(&self) -> bool {
        self.queue.is_empty()
    }

    fn enqueue(&mut self, mid: ModuleId, map: &HashMap<ModuleId, Arc<Module>>) {
        if !self.visited.insert(mid) {
            return;
        }

        for init_expr in map[&mid].ast_root().init_expressions.iter() {
            self.queue.push_back(init_expr.clone());
            if let ast::InitExpr::EnsureInitialized(mid) = init_expr {
                self.enqueue(*mid, map);
            }
        }
    }

    fn dequeue(&mut self) -> Option<ast::InitExpr> {
        self.queue.pop_front()
    }
}
