use super::{Backend, Error, Result, Value};
use crate::lowering::ir::*;
use std::cmp::Ordering;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Evaluator<'a> {
    ctx: &'a Backend,
    env: HashMap<RtId, Value>,
    last_sym: u64,
}

impl<'a> Evaluator<'a> {
    pub fn new(ctx: &'a Backend) -> Self {
        Self {
            ctx,
            env: HashMap::new(),
            last_sym: 0,
        }
    }

    pub fn eval(&mut self, rt: &Rt) -> Result<Value> {
        match rt {
            Rt::Var(id, _) => match self.env.get(id) {
                Some(value) => Ok(value.clone()),
                None => internal_error!("Undefined variable: {}", id),
            },
            Rt::LocalFun(_) => {
                internal_error!("Found Rt::LocalFun at Evaluator, this must be erased by lowerizer")
            }
            Rt::StaticFun(capture) => {
                let id = match capture.fun {
                    Ct::Id(id) => id,
                    _ => internal_error!(
                        "Unresolved Ct: {}, this must be resolved by lowerizer",
                        capture.fun
                    )?,
                };
                let env = match capture.env {
                    Some(ref env) => Some(Box::new(self.eval(env)?)),
                    None => None,
                };
                Ok(Value::Clos(id, env))
            }
            Rt::Call(call) => match call.callee {
                RtCallee::Standard(ref callee) => {
                    let callee = self.eval(callee)?;
                    let args = self.eval_all(&call.args)?;
                    self.eval_call(callee, args)
                }
                RtCallee::CDirect(_, _)
                | RtCallee::CIndirect(_, _)
                | RtCallee::MainIndirect(_)
                | RtCallee::MacroIndirect(_, _) => {
                    internal_error!("Unsupported callee: {}", call.callee)
                }
            },
            Rt::Nullary(nullary) => {
                use Nullary::*;
                match nullary {
                    Uninitialized(_) => Ok(Value::Unit),
                    GenId => {
                        self.last_sym += 1;
                        Ok(Value::String(format!("@{}", self.last_sym)))
                    }
                    op => internal_error!("Unsupported Nullary op: {}", op),
                }
            }
            Rt::Unary(unary) => {
                use Unary::*;
                let a = self.eval(&unary.1)?;
                match &unary.0 {
                    Not => Ok(Value::bool(!a.into_bool()?)),
                    Load => a.load(),
                    StructElem(_, index) => a.struct_elem(*index),
                    Reinterpret(_) => Ok(a),
                    SyntaxBody => a.syntax_body(),
                    Panic => Err(Error::Panic(format!("panic({})", a.into_string()?))),
                    op => internal_error!("Unsupported Unary op: {}", op),
                }
            }
            Rt::Binary(binary) => {
                use Binary::*;
                let a = self.eval(&binary.1)?;
                let b = self.eval(&binary.2)?;
                match &binary.0 {
                    Store => b.set(a),
                    IEq | FEq | StringEq => a.equal(b).map(Value::bool),
                    SLt | ULt | FLt => a.compare(b).map(|a| Value::bool(a == Ordering::Less)),
                    SLe | ULe | FLe => a.compare(b).map(|a| Value::bool(a != Ordering::Greater)),
                    SGt | UGt | FGt => a.compare(b).map(|a| Value::bool(a == Ordering::Greater)),
                    SGe | UGe | FGe => a.compare(b).map(|a| Value::bool(a != Ordering::Less)),
                    SAdd | UAdd | FAdd => a.add(b),
                    SSub | USub | FSub => a.sub(b),
                    SMul | UMul | FMul => a.mul(b),
                    SDiv | UDiv | FDiv => a.div(b),
                    SRem | URem | FRem => a.rem(b),
                    StringCmp => Ok(Value::S(match a.compare(b)? {
                        Ordering::Less => -1,
                        Ordering::Equal => 0,
                        Ordering::Greater => 1,
                    })),
                    StringConcat => a.concat(b),
                    op => internal_error!("Unsupported Binary op: {}", op),
                }
            }
            Rt::Ternary(ternary) => {
                let _a = self.eval(&ternary.1)?;
                let _b = self.eval(&ternary.2)?;
                let _c = self.eval(&ternary.3)?;
                let op = &ternary.0;
                internal_error!("Unsupported Ternary op: {}", op)
            }
            Rt::Alloc(alloc) => Ok(Value::r#box(self.eval(&alloc.1)?)),
            Rt::AllocArray(_) => internal_error!("AllocArray is unsupported"),
            Rt::ConstructEnv(con) => Ok(Value::Env(self.eval_all(&con.1)?)),
            Rt::ConstructData(_) => internal_error!(
                "Found Rt::ConstructData at Evaluator, this must be erased by lowerizer"
            ),
            Rt::ConstructStruct(con) => Ok(Value::Struct(self.eval_all(&con.1)?)),
            Rt::ConstructSyntax(con) => Ok(Value::Syntax(con.0, Box::new(self.eval(&con.1)?))),
            Rt::Seq(seq) => {
                for stmt in seq.0.iter() {
                    let _ = self.eval(stmt)?;
                }
                self.eval(&seq.1)
            }
            Rt::If(if_) => {
                if self.eval(&if_.0)?.into_bool()? {
                    self.eval(&if_.1)
                } else {
                    self.eval(&if_.2)
                }
            }
            Rt::While(while_) => {
                while self.eval(&while_.0)?.into_bool()? {
                    self.eval(&while_.1)?;
                }
                Ok(Value::Unit)
            }
            Rt::And(_) => {
                internal_error!("Found Rt::And at Evaluator, this must be erased by lowerizer")
            }
            Rt::Or(_) => {
                internal_error!("Found Rt::Or at Evaluator, this must be erased by lowerizer")
            }
            Rt::Match(_) => {
                internal_error!("Found Rt::Match at Evaluator, this must be erased by lowerizer")
            }
            Rt::Return(ret) => Err(Error::ReturnRequest(self.eval(ret)?)),
            Rt::ContCall(call) => Err(Error::ContRequest(call.cont, self.eval_all(&call.args)?)),
            Rt::Never => internal_error!("Reached Rt::Never"),
            Rt::LetLocalFun(_) => internal_error!(
                "Found Rt::LetLocalFun at Evaluator, this must be erased by lowerizer"
            ),
            Rt::LetVar(let_) => self.eval_let_var(&let_.0, &let_.1),
            Rt::LetCont(let_) => self.eval_let_cont(&let_.0, &let_.1),
            Rt::Const(c) => Value::from_const(c),
        }
    }

    pub fn eval_all<'b>(&mut self, rts: impl IntoIterator<Item = &'b Rt>) -> Result<Vec<Value>> {
        rts.into_iter().map(|arg| self.eval(arg)).collect()
    }

    fn eval_let_var(&mut self, vars: &Vec<RtVar>, body: &Rt) -> Result<Value> {
        for var in vars {
            let init = self.eval(&var.init)?;
            self.env.insert(var.id, init);
        }

        self.eval(body)
    }

    fn eval_let_cont(&mut self, conts: &Vec<RtCont>, body: &Rt) -> Result<Value> {
        let (id, args) = match self.eval(body) {
            Err(Error::ContRequest(id, args)) => (id, args),
            result => return result,
        };
        let cont = match conts.iter().find(|cont| cont.id == id) {
            Some(cont) => cont,
            None => return Err(Error::ContRequest(id, args)),
        };

        for (param, arg) in cont.params.iter().zip(args) {
            self.env.insert(param.id, arg);
        }

        self.eval_let_cont(conts, &cont.body)
    }

    pub fn eval_call(&mut self, callee: Value, args: Vec<Value>) -> Result<Value> {
        let (id, env) = callee.into_clos()?;

        let function = match self.ctx.defs().get(&id) {
            Some(def) => match def.as_ref() {
                Def::Function(function) => function,
                _ => internal_error!("Cannot call {}", def)?,
            },
            _ => internal_error!("Undefined static: {}", id)?,
        };

        let mut evaluator = Evaluator::new(self.ctx);

        match (&function.env, env) {
            (Some(fenv), Some(env)) => {
                let elems = env.clone().into_env()?;
                if elems.len() != fenv.elems.len() {
                    internal_error!("Env arity mismatch")?;
                }
                for (param, arg) in fenv.elems.iter().zip(elems) {
                    evaluator.env.insert(param.id, arg.clone());
                }
                evaluator.env.insert(fenv.id, *env);
            }
            (None, None) => {}
            _ => internal_error!("Function signature mismatch")?,
        }

        match (&function.params, args) {
            (params, args) if params.len() == args.len() => {
                for (param, arg) in function.params.iter().zip(args) {
                    evaluator.env.insert(param.id, arg);
                }
            }
            _ => internal_error!("Function signature mismatch")?,
        }

        match evaluator.eval(&function.body) {
            Err(Error::ReturnRequest(ret)) => Ok(ret),
            result => result,
        }
    }
}
