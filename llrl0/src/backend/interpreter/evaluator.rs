use super::{Backend, Error, Result, Value};
use crate::emitter::ir::*;
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
            Rt::Local(id) => match self.env.get(id) {
                Some(value) => Ok(value.clone()),
                None => Err(Error::Internal(format!("Undefined variable: {}", id))),
            },
            Rt::LocalFun(_, _) => Err(Error::Internal(
                "Found Rt::LocalFun at Evaluator, this should be erased by emitter".to_string(),
            )),
            Rt::Capture(Ct::Id(id), env) => {
                let env = match env {
                    Some(env) => Some(self.eval(env)?),
                    None => None,
                };
                Ok(Value::Clos(*id, env.map(Box::new)))
            }
            Rt::Capture(ct, _) => Err(Error::Internal(format!(
                "Unresolved Ct: {}, this should be resolved by emitter",
                ct
            ))),
            Rt::Call(call) => {
                let callee = self.eval(&call.0)?;
                let args = self.eval_all(&call.1)?;
                self.eval_call(callee, args)
            }
            Rt::CCall(_) => Err(Error::Internal("CCall is unsupported".to_string())),
            Rt::Nullary(nullary) => {
                use Nullary::*;
                match nullary {
                    Uninitialized(ty) => Ok(Value::Uninitialized(ty.clone())),
                    GenId => {
                        self.last_sym += 1;
                        Ok(Value::String(format!("@{}", self.last_sym)))
                    }
                    op => Err(Error::Internal(format!("Unsupported Nullary op: {}", op))),
                }
            }
            Rt::Unary(unary) => {
                use Unary::*;
                let a = self.eval(&unary.1)?;
                match &unary.0 {
                    Not => Ok(Value::bool(!a.test()?)),
                    Load => a.load(),
                    StructElem(ty, index) => a.struct_elem(ty, *index),
                    Reinterpret(from, to) => a.reinterpret(from, to),
                    SyntaxBody(ty) => a.syntax_body(ty),
                    Panic => match a {
                        Value::String(s) => Err(Error::Panic(s)),
                        v => Err(Error::Internal(format!("panic({})", v))),
                    },
                    op => Err(Error::Internal(format!("Unsupported Unary op: {}", op))),
                }
            }
            Rt::Binary(binary) => {
                use Binary::*;
                let a = self.eval(&binary.1)?;
                let b = self.eval(&binary.2)?;
                match &binary.0 {
                    Store => b.set(a),
                    IEq | FEq | StringEq | CharEq => a.equal(&b).map(Value::bool),
                    SLt | ULt | FLt => a.compare(&b).map(|a| Value::bool(a == Ordering::Less)),
                    SLe | ULe | FLe => a.compare(&b).map(|a| Value::bool(a != Ordering::Greater)),
                    SGt | UGt | FGt => a.compare(&b).map(|a| Value::bool(a == Ordering::Greater)),
                    SGe | UGe | FGe => a.compare(&b).map(|a| Value::bool(a != Ordering::Less)),
                    SAdd | UAdd | FAdd => a.add(b),
                    SSub | USub | FSub => a.sub(b),
                    SMul | UMul | FMul => a.mul(b),
                    SDiv | UDiv | FDiv => a.div(b),
                    SRem | URem | FRem => a.rem(b),
                    StringCmp => a.compare(&b).map(|a| {
                        let a = match a {
                            Ordering::Less => -1,
                            Ordering::Equal => 0,
                            Ordering::Greater => 1,
                        };
                        Value::S(Ct::S(32), a)
                    }),
                    StringConcat => a.concat(b),
                    op => Err(Error::Internal(format!("Unsupported Binary op: {}", op))),
                }
            }
            Rt::Ternary(ternary) => {
                let _a = self.eval(&ternary.1)?;
                let _b = self.eval(&ternary.2)?;
                let _c = self.eval(&ternary.3)?;
                match &ternary.0 {
                    op => Err(Error::Internal(format!("Unsupported Ternary op: {}", op))),
                }
            }
            Rt::Alloc(alloc) => Ok(Value::alloc(alloc.0, self.eval(&alloc.1)?)),
            Rt::AllocArray(_) => Err(Error::Internal("AllocArray is unsupported".to_string())),
            Rt::ConstructEnv(con) => Ok(Value::construct_env(con.0, self.eval_all(&con.1)?)),
            Rt::ConstructData(_) => Err(Error::Internal(
                "Found Rt::ConstructData at Evaluator, this should be erased by emitter"
                    .to_string(),
            )),
            Rt::ConstructStruct(con) => {
                let fields = self.eval_all(&con.1)?;
                Ok(Value::Struct(con.0.clone(), fields))
            }
            Rt::ConstructSyntax(con) => {
                let body = self.eval(&con.2)?;
                Ok(Value::construct_syntax(con.0, con.1.clone(), body))
            }
            Rt::Seq(seq) => {
                for stmt in seq.0.iter() {
                    let _ = self.eval(stmt)?;
                }
                self.eval(&seq.1)
            }
            Rt::If(if_) => {
                if self.eval(&if_.0)?.test()? {
                    self.eval(&if_.1)
                } else {
                    self.eval(&if_.2)
                }
            }
            Rt::While(while_) => {
                while self.eval(&while_.0)?.test()? {
                    self.eval(&while_.1)?;
                }
                Ok(Value::Unit)
            }
            Rt::And(_) => Err(Error::Internal(
                "Found Binary::And at Evaluator, this should be erased by emitter".to_string(),
            )),
            Rt::Or(_) => Err(Error::Internal(
                "Found Binary::Or at Evaluator, this should be erased by emitter".to_string(),
            )),
            Rt::Match(_) => Err(Error::Internal(
                "Found Rt::Match at Evaluator, this should be erased by emitter".to_string(),
            )),
            Rt::Return(ret) => Err(Error::ReturnRequest(self.eval(ret)?)),
            Rt::Cont(id, args) => Err(Error::ContRequest(*id, self.eval_all(args)?)),
            Rt::Never => Err(Error::UndefinedBehavior("Reached Rt::Never".to_string())),
            Rt::LetFunction(_) => Err(Error::Internal(
                "Found Rt::LetFunction at Evaluator, this should be erased by emitter".to_string(),
            )),
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
        let cont = match conts.into_iter().find(|cont| cont.id == id) {
            Some(cont) => cont,
            None => return Err(Error::ContRequest(id, args)),
        };

        for (param, arg) in cont.params.iter().zip(args) {
            self.env.insert(param.id, arg);
        }

        self.eval_let_cont(conts, &cont.body)
    }

    pub fn eval_call(&mut self, callee: Value, args: Vec<Value>) -> Result<Value> {
        callee.exclude_uninitialized_as_ub("Evaluator::eval_call")?;

        let (id, env) = match callee {
            Value::Clos(id, env) => (id, env),
            v => Err(Error::Internal(format!("Evaluator::eval_call({}, ..)", v)))?,
        };

        let function = match self.ctx.defs().get(&id) {
            Some(def) => match def.as_ref() {
                CtDef::Function(function) => function,
                _ => Err(Error::Internal(format!("Cannot call {}", def)))?,
            },
            _ => Err(Error::Internal(format!("Undefined static: {}", id)))?,
        };

        let mut evaluator = Evaluator::new(self.ctx);

        match (&function.env, env) {
            (Some(fenv), Some(env)) => match *env {
                Value::Env(ref args) if fenv.elems.len() == args.len() => {
                    for (elem, arg) in fenv.elems.iter().zip(args) {
                        evaluator.env.insert(elem.id, arg.clone());
                    }
                    evaluator.env.insert(fenv.id, *env);
                }
                v => Err(Error::Internal(format!("Cannot treat {} as env", v)))?,
            },
            (None, None) => {}
            _ => Err(Error::Internal("Function signature mismatch".to_string()))?,
        }

        match (&function.params, args) {
            (params, args) if params.len() == args.len() => {
                for (param, arg) in function.params.iter().zip(args) {
                    evaluator.env.insert(param.id, arg);
                }
            }
            _ => Err(Error::Internal("Function signature mismatch".to_string()))?,
        }

        match evaluator.eval(&function.body) {
            Err(Error::ReturnRequest(ret)) => Ok(ret),
            result => result,
        }
    }
}
