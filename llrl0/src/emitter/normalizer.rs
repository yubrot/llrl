//! Normalizes the IR by evaluating Ct.
//! All generic CtDef instantiations and LocalFun definitions are lifted to the top level.

use super::{ir::*, rewriter, traverser};
use if_chain::if_chain;
use std::collections::{BTreeMap, HashMap};
use std::sync::Arc;

pub fn normalize(src: &mut impl rewriter::Rewrite, env: &mut impl Env) {
    let _ = rewriter::rewrite(
        src,
        &mut Normalizer {
            env,
            local_var_types: HashMap::new(),
        },
    );
}

pub trait Env: Sized {
    fn instantiate(&mut self, id: CtId, args: Vec<Ct>) -> CtId;

    fn get_processing_ct_def(&mut self, id: CtId) -> Option<ProcessingCtDef>;

    fn alloc_ct(&mut self) -> CtId;

    fn define_ct(&mut self, id: CtId, def: CtDef);

    fn alloc_rt(&mut self) -> RtId;
}

#[derive(Debug)]
pub struct ProcessingCtDef {
    pub is_normalized: bool,
    pub def: Arc<CtDef>,
}

#[derive(Debug)]
struct Normalizer<'e, E> {
    env: &'e mut E,
    local_var_types: HashMap<RtId, Ct>,
}

impl<'e, E: Env> rewriter::Rewriter for Normalizer<'e, E> {
    type Error = ();

    fn after_ct(&mut self, ct: &mut Ct) -> Result<(), ()> {
        match ct {
            Ct::Id(id) => if_chain! {
                if let Some(ProcessingCtDef { is_normalized, def }) = self.env.get_processing_ct_def(*id);
                if let CtDef::Alias(x) = def.as_ref();
                then {
                    *ct = x.clone();
                    if !is_normalized {
                        self.rewrite(ct)?;
                    }
                }
            },
            Ct::GenericInst(inst) => if_chain! {
                if let Ct::Id(generic_id) = inst.0;
                let args = std::mem::take(&mut inst.1);
                let id = self.env.instantiate(generic_id, args);
                then {
                    *ct = Ct::Id(id);
                    self.rewrite(ct)?;
                } else {
                    panic!("Attempt to generic-inst on {}", ct)
                }
            },
            Ct::TableGet(get) => if_chain! {
                if let Ct::Id(table_id) = get.0;
                if let Some(ProcessingCtDef { is_normalized, def }) = self.env.get_processing_ct_def(table_id);
                if let CtDef::AliasTable(table) = def.as_ref();
                then {
                    if let Some(x) = table.get(get.1) {
                        *ct = x.clone();
                        if !is_normalized {
                            self.rewrite(ct)?;
                        }
                    } else {
                        panic!("Cannot get {} from table {}", get.1, table_id)
                    }
                } else {
                    panic!("Attempt to table-get on {}", ct)
                }
            },
            _ => {}
        }
        Ok(())
    }

    fn before_rt(&mut self, rt: &mut Rt) -> Result<bool, ()> {
        match rt {
            Rt::LetFunction(let_) => {
                let funs = std::mem::take(&mut let_.0);
                let body = std::mem::take(&mut let_.1);
                *rt = ClosureConversion::run(funs, body, self)?;
            }
            _ => {}
        }
        Ok(true)
    }

    fn after_rt(&mut self, rt: &mut Rt) -> Result<(), ()> {
        if_chain! {
            if let Rt::Call(call) = rt;
            if let Rt::StaticFun(Ct::Id(id), None) = call.0;
            if let Some(ProcessingCtDef { is_normalized, def }) = self.env.get_processing_ct_def(id);
            if let CtDef::Function(f) = def.as_ref();
            if f.kind == FunctionKind::Transparent;
            let args = std::mem::take(&mut call.1);
            then {
                *rt = call_transparent(f, args);
                if !is_normalized {
                    self.rewrite(rt)?;
                } else {
                    self.after_rt(rt)?;
                }
            }
        }
        Ok(())
    }

    fn after_rt_def(&mut self, id: RtId, ty: impl FnOnce() -> Ct) -> Result<(), ()> {
        self.local_var_types.insert(id, ty());
        Ok(())
    }
}

#[derive(Debug)]
struct ClosureConversion {
    lifted_funs: HashMap<RtId, CtId>,
    captured_vars: BTreeMap<RtId, RtId>,
    captured_env: Option<RtId>,
}

impl ClosureConversion {
    fn new(funs: &Vec<RtFunction>, env: &mut impl Env) -> Self {
        let lifted_funs = funs
            .iter()
            .map(|f| (f.id, env.alloc_ct()))
            .collect::<HashMap<_, _>>();
        let captured_vars = traverser::captured_vars::<_, Vec<_>>(funs)
            .into_iter()
            .map(|id| (id, env.alloc_rt()))
            .collect::<BTreeMap<_, _>>();
        let captured_env = match captured_vars.is_empty() {
            true => None,
            false => Some(env.alloc_rt()),
        };
        Self {
            lifted_funs,
            captured_vars,
            captured_env,
        }
    }

    fn run(
        funs: Vec<RtFunction>,
        mut body: Rt,
        normalizer: &mut Normalizer<impl Env>,
    ) -> Result<Rt, ()> {
        let mut cc = Self::new(&funs, normalizer.env);

        let env_params = cc
            .captured_vars
            .iter()
            .map(|(var, id)| FunctionParam::new(*id, normalizer.local_var_types[var].clone()))
            .collect::<Vec<_>>();
        let env_args = cc
            .captured_vars
            .iter()
            .map(|(var, _)| Rt::Local(*var))
            .collect();

        for mut fun in funs {
            rewriter::rewrite(&mut fun.body, &mut cc)?;
            let id = cc.lifted_funs[&fun.id];
            let env = cc
                .captured_env
                .map(|id| FunctionEnv::new(id, env_params.clone()));
            let def = CtDef::generic(
                fun.ct_params,
                CtDef::Function(Function::new(
                    env,
                    fun.params,
                    fun.ret,
                    fun.body,
                    FunctionKind::Standard,
                )),
            );
            normalizer.env.define_ct(id, def);
        }

        cc.captured_vars.clear();
        rewriter::rewrite(&mut body, &mut cc)?;
        Ok(match cc.captured_env {
            Some(id) => Rt::let_var(
                vec![RtVar::new(
                    id,
                    Ct::Env,
                    Rt::construct_env(Location::Heap, env_args),
                )],
                body,
            ),
            None => body,
        })
    }
}

impl rewriter::Rewriter for ClosureConversion {
    type Error = ();

    fn after_rt(&mut self, rt: &mut Rt) -> Result<(), ()> {
        match rt {
            Rt::Local(id) => {
                if let Some(id) = self.captured_vars.get(id) {
                    *rt = Rt::Local(*id);
                }
            }
            Rt::LocalFun(id, args) => {
                if let Some(id) = self.lifted_funs.get(id) {
                    let args = std::mem::take(args);
                    let f = Ct::generic_inst(Ct::Id(*id), args);
                    *rt = Rt::capture(f, self.captured_env.map(Rt::Local));
                }
            }
            _ => {}
        }
        Ok(())
    }
}

fn call_transparent(f: &Function, args: Vec<Rt>) -> Rt {
    assert!(f.env.is_none());
    assert_eq!(f.params.len(), args.len());
    let params = f.params.iter().map(|param| param.id);
    let mut rt = f.body.clone();
    rewriter::replace_rt(&mut rt, params.zip(args).collect());
    rt
}
