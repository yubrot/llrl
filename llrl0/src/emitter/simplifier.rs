//! Translates module constructs into the emitter IR.

use super::ir::*;
use crate::ast;
use crate::module::{ModuleMap, TextualUnit};

pub fn simplify<'m, T: Simplify>(src: &T, env: &mut impl Env<'m>) -> T::Dest {
    Simplify::simplify(src, env)
}

pub trait Env<'m>: Sized {
    type ModuleMap: ModuleMap + 'm;

    fn module_map(&self) -> &'m Self::ModuleMap;

    fn alloc_ct(&mut self) -> CtId;

    fn issue_ct(&mut self, construct: impl Into<ast::Construct>) -> CtId;

    fn alloc_rt(&mut self) -> RtId;

    fn issue_rt(&mut self, construct: impl Into<ast::Construct>) -> RtId;

    fn simplify<T: Simplify + ?Sized>(&mut self, src: &T) -> T::Dest {
        src.simplify(self)
    }

    fn simplify_def(&mut self, src: ast::Construct) -> Option<CtDef> {
        let map = self.module_map();
        match src {
            ast::Construct::Function(id) => Some(self.simplify(map.ast(id).unwrap())),
            ast::Construct::CFunction(id) => Some(self.simplify(map.ast(id).unwrap())),
            ast::Construct::BuiltinOp(id) => Some(self.simplify(map.ast(id).unwrap())),
            ast::Construct::Macro(id) => Some(self.simplify(map.ast(id).unwrap())),
            ast::Construct::DataTypeCon(id) => Some(self.simplify(&map.ast(id).unwrap())),
            ast::Construct::DataValueCon(id) => Some(self.simplify(map.ast(id).unwrap())),
            ast::Construct::BuiltinTypeCon(id) => Some(self.simplify(map.ast(id).unwrap().con)),
            ast::Construct::BuiltinValueCon(id) => Some(self.simplify(map.ast(id).unwrap())),
            ast::Construct::ClassMethod(id) => self.simplify(map.ast(id).unwrap()),
            ast::Construct::InstanceMethod(id) => Some(self.simplify(map.ast(id).unwrap())),
            ast::Construct::InstanceCon(id) => Some(self.simplify(&map.ast(id).unwrap())),
            _ => None,
        }
    }

    fn simplify_scheme(
        &mut self,
        signature: Option<Option<&Vec<ast::Parameter>>>,
        scheme: &ast::Scheme,
    ) -> (Vec<CtId>, Vec<FunctionParam>, Ct) {
        let mut ct_params = self.simplify(&scheme.ty_params);
        ct_params.append(&mut self.simplify(&scheme.s_params));

        let (params, ret_ty) = match (scheme.body.matches_fun(), signature) {
            (_, Some(None)) | (None, None) => (Vec::new(), self.simplify(&scheme.body)),
            (Some((param_tys, ret_ty)), Some(Some(params))) => {
                let params = params
                    .iter()
                    .zip(param_tys)
                    .map(|(param, ty)| {
                        let id = self.simplify(param);
                        let ty = self.simplify(ty);
                        FunctionParam::new(id, ty)
                    })
                    .collect();
                let ret_ty = self.simplify(ret_ty);
                (params, ret_ty)
            }
            (Some((param_tys, ret_ty)), None) => {
                let params = self
                    .simplify(param_tys)
                    .into_iter()
                    .map(|ty| FunctionParam::new(self.alloc_rt(), ty))
                    .collect();
                let ret_ty = self.simplify(ret_ty);
                (params, ret_ty)
            }
            (None, Some(Some(_))) => panic!("scheme does not match signature"),
        };

        (ct_params, params, ret_ty)
    }

    fn simplify_const(&mut self, ty: Ct, const_: &ast::Const) -> Const {
        match const_ {
            ast::Const::Integer(signed, v) => Const::Integer(ty, *signed, *v),
            ast::Const::FPNumber(v) => Const::FPNumber(ty, *v),
            ast::Const::String(s) => Const::String(s.clone()),
            ast::Const::Char(c) => Const::Char(*c),
            ast::Const::SyntaxSexp(s) => self.simplify_const_sexp(s.as_ref().clone()),
        }
    }

    fn simplify_const_sexp(&mut self, syntax_sexp: Syntax<Sexp>) -> Const {
        let ty = self.issue_ct(ast::builtin::SEXP);
        Const::SyntaxSexp(Ct::Id(ty), Box::new(syntax_sexp))
    }
}

pub trait Simplify {
    type Dest;

    fn simplify<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest;
}

impl<T: Simplify> Simplify for [T] {
    type Dest = Vec<T::Dest>;

    fn simplify<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        self.iter().map(|a| env.simplify(a)).collect()
    }
}

impl<T: Simplify> Simplify for &'_ T {
    type Dest = T::Dest;

    fn simplify<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        env.simplify(&**self)
    }
}

impl<T: Simplify> Simplify for Vec<T> {
    type Dest = Vec<T::Dest>;

    fn simplify<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        self.iter().map(|a| env.simplify(a)).collect()
    }
}

impl<T: Simplify> Simplify for Option<T> {
    type Dest = Option<T::Dest>;

    fn simplify<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        self.as_ref().map(|a| env.simplify(a))
    }
}

impl Simplify for ast::NodeId<ast::Function> {
    type Dest = Ct;

    fn simplify<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        Ct::Id(env.issue_ct(*self))
    }
}

impl Simplify for ast::Function {
    type Dest = CtDef;

    fn simplify<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        let scheme = env.module_map().scheme_of(self.id).unwrap();

        let (ct_params, params, ret_ty) = env.simplify_scheme(Some(self.params.as_ref()), scheme);
        let body = env.simplify(&self.body);
        let kind = if self.transparent {
            FunctionKind::Transparent
        } else {
            FunctionKind::Standard
        };

        CtDef::generic(
            ct_params,
            CtDef::Function(Function::new(None, params, ret_ty, body, kind)),
        )
    }
}

impl Simplify for ast::Parameter {
    type Dest = RtId;

    fn simplify<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        env.issue_rt(self.id)
    }
}

impl Simplify for ast::CFunction {
    type Dest = CtDef;

    fn simplify<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        let (params, ret_ty) = match self.ann.body.matches_fun() {
            Some((param_tys, ret_ty)) => {
                let params = env
                    .simplify(param_tys)
                    .into_iter()
                    .map(|ty| FunctionParam::new(env.alloc_rt(), ty))
                    .collect();
                let ret_ty = env.simplify(ret_ty);
                (params, ret_ty)
            }
            None => (Vec::new(), env.simplify(&self.ann.body)),
        };

        let rt = Rt::c_call(
            self.c_name.clone(),
            Ct::clos(
                params.iter().map(|p| p.ty.clone()).collect(),
                ret_ty.clone(),
            ),
            params.iter().map(|p| Rt::Local(p.id)).collect(),
        );

        CtDef::Function(Function::new(
            None,
            params,
            ret_ty,
            rt,
            FunctionKind::Standard,
        ))
    }
}

impl Simplify for ast::BuiltinOp {
    type Dest = CtDef;

    fn simplify<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        let unit = env.module_map().textual_unit_of(self.id).unwrap();
        let (ct_params, params, ret_ty) = env.simplify_scheme(None, &self.ann.body);

        let ct_args = ct_params.iter().map(|p| Ct::Id(*p)).collect();
        let args = params.iter().map(|p| Rt::Local(p.id)).collect();
        let rt = builtin_rt(unit, &self.builtin_name, ct_args, args);

        CtDef::generic(
            ct_params,
            CtDef::Function(Function::new(
                None,
                params,
                ret_ty,
                rt,
                FunctionKind::Transparent,
            )),
        )
    }
}

impl Simplify for ast::NodeId<ast::Macro> {
    type Dest = Ct;

    fn simplify<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        Ct::Id(env.issue_ct(*self))
    }
}

impl Simplify for ast::Macro {
    type Dest = CtDef;

    fn simplify<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        let src_ty = env.simplify(&ast::Macro::src_ty());
        let dest_ty = env.simplify(&ast::Macro::dest_ty());

        let param = FunctionParam::new(env.simplify(&self.param), src_ty);
        let rt = env.simplify(&self.body);

        CtDef::Function(Function::new(
            None,
            vec![param],
            dest_ty,
            rt,
            FunctionKind::Macro,
        ))
    }
}

impl<'a> Simplify for ast::DataType<'a> {
    type Dest = CtDef;

    fn simplify<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        let repr = env.simplify(&self.con.repr);
        let params = env.simplify(&self.con.ty_params);
        let cons = self
            .value_cons()
            .map(|value_con| env.simplify(&value_con.fields).unwrap_or_default())
            .collect();

        CtDef::generic(params, CtDef::Data(Data::new(repr, cons)))
    }
}

impl Simplify for ast::DataRepr {
    type Dest = DataRepr;

    fn simplify<'m>(&self, _env: &mut impl Env<'m>) -> Self::Dest {
        match self {
            Self::Default => DataRepr::Boxed,
            Self::Value => DataRepr::Value,
            Self::C => DataRepr::C,
        }
    }
}

impl<'a> Simplify for ast::DataValueCon {
    type Dest = CtDef;

    fn simplify<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        let index = {
            let type_con = env.module_map().ast(self.type_con).unwrap();
            type_con.con.index_of(self.id).unwrap()
        };

        let scheme = env.module_map().scheme_of(self.id).unwrap();

        let (ct_params, params, ret_ty) = env.simplify_scheme(None, scheme);

        let rt = Rt::construct_data(
            ret_ty.clone(),
            index,
            params.iter().map(|p| Rt::Local(p.id)).collect(),
        );

        CtDef::generic(
            ct_params,
            CtDef::Function(Function::new(
                None,
                params,
                ret_ty,
                rt,
                FunctionKind::Transparent,
            )),
        )
    }
}

impl Simplify for ast::BuiltinTypeCon {
    type Dest = CtDef;

    fn simplify<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        let params = env.simplify(&self.ty_params);
        let ty = builtin_ct(&self.builtin_name, &params);
        CtDef::generic(params, CtDef::Alias(ty))
    }
}

impl Simplify for ast::BuiltinValueCon {
    type Dest = CtDef;

    fn simplify<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        let unit = env.module_map().textual_unit_of(self.id).unwrap();
        let scheme = env.module_map().scheme_of(self.id).unwrap();

        let (ct_params, params, ty) = env.simplify_scheme(None, scheme);

        let ct_args = ct_params.iter().map(|p| Ct::Id(*p)).collect();
        let args = params.iter().map(|p| Rt::Local(p.id)).collect();
        let rt = builtin_rt(unit, &self.builtin_name, ct_args, args);

        CtDef::generic(
            ct_params,
            CtDef::Function(Function::new(
                None,
                params,
                ty,
                rt,
                FunctionKind::Transparent,
            )),
        )
    }
}

impl Simplify for ast::ValueConField {
    type Dest = Ct;

    fn simplify<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        env.simplify(&self.ty)
    }
}

impl<'a> Simplify for ast::Instance<'a> {
    type Dest = CtDef;

    fn simplify<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        let ast::ConstraintRep::Class(ref class_id, ref class_args) = self.con.target.rep;
        let class = env.module_map().ast(*class_id.get_resolved()).unwrap();

        let mut params = env.simplify(&self.con.ty_params);
        params.append(&mut env.simplify(&self.con.s_params));

        let inst_args = params.iter().copied().map(Ct::Id).collect::<Vec<_>>();

        let default_impl_args = {
            let this = Ct::generic_inst(Ct::Id(env.issue_ct(self.con.id)), inst_args.clone());
            let mut args = env.simplify(class_args);
            args.push(this);
            args
        };

        let mut table = AliasTable::new();

        for method in class.methods() {
            let class_method_id = env.issue_ct(method.id);

            let (impl_, args) = match self.find_method(method.id) {
                Some(inst_method) => (Ct::Id(env.issue_ct(inst_method.id)), inst_args.clone()),
                None => (Ct::Id(class_method_id), default_impl_args.clone()),
            };

            table.insert(class_method_id, Ct::generic_inst(impl_, args));
        }

        let superclass_cs = &class.con.superclasses;
        let instance_inst = env.module_map().instantiation_of(self.con.id).unwrap();
        for (c, s) in superclass_cs.iter().zip(instance_inst.s_args.iter()) {
            let c = env.simplify(c);
            let s = env.simplify(s);
            table.insert(c, s);
        }

        CtDef::generic(params, CtDef::AliasTable(table))
    }
}

impl<'a> Simplify for ast::ClassMethod {
    type Dest = Option<CtDef>;

    fn simplify<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        if let Some(ref body) = self.default_body {
            let class = env.module_map().ast(self.class_con).unwrap();

            let mut class_ct_params = env.simplify(&class.con.ty_params);
            class_ct_params.push(env.simplify(&class.con.constraint()));

            let (method_ct_params, params, ret_ty) =
                env.simplify_scheme(Some(self.params.as_ref()), &self.ann.body);

            let body = env.simplify(body);

            Some(CtDef::generic(
                class_ct_params,
                CtDef::generic(
                    method_ct_params,
                    CtDef::Function(Function::new(
                        None,
                        params,
                        ret_ty,
                        body,
                        FunctionKind::Standard,
                    )),
                ),
            ))
        } else {
            None
        }
    }
}

impl<'a> Simplify for ast::InstanceMethod {
    type Dest = CtDef;

    fn simplify<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        let inst = env.module_map().ast(self.instance_con).unwrap();
        let scheme = env.module_map().scheme_of(self.id).unwrap();

        let mut inst_ct_params = env.simplify(&inst.con.ty_params);
        inst_ct_params.append(&mut env.simplify(&inst.con.s_params));

        let (method_ct_params, params, ret_ty) =
            env.simplify_scheme(Some(self.params.as_ref()), scheme);

        let body = env.simplify(&self.body);
        let kind = if self.transparent {
            FunctionKind::Transparent
        } else {
            FunctionKind::Standard
        };

        CtDef::generic(
            inst_ct_params,
            CtDef::generic(
                method_ct_params,
                CtDef::Function(Function::new(None, params, ret_ty, body, kind)),
            ),
        )
    }
}

impl Simplify for ast::InitExpr {
    type Dest = Option<Init>;

    fn simplify<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        match self {
            ast::InitExpr::Eval(expr) => {
                let ty = env.simplify(env.module_map().type_of(expr.id).unwrap());
                let expr = env.simplify(expr);
                Some(Init::new(ty, expr))
            }
            ast::InitExpr::EnsureInitialized(_) => None,
        }
    }
}

impl Simplify for ast::Expr {
    type Dest = Rt;

    fn simplify<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        let map = env.module_map();
        match self.rep {
            ast::ExprRep::Use(ref use_) => match *use_.get_resolved() {
                ast::Value::Function(id) => {
                    let f = Ct::Id(env.issue_ct(id));
                    let ct_args = env.simplify(map.instantiation_of(self.id).unwrap());
                    let autocall = map.ast(id).unwrap().params.is_none();
                    Rt::autocall(Rt::Capture(Ct::generic_inst(f, ct_args), None), autocall)
                }
                ast::Value::CFunction(id) => {
                    let f = Ct::Id(env.issue_ct(id));
                    let autocall = !map.ast(id).unwrap().ann.body.is_fun();
                    Rt::autocall(Rt::Capture(f, None), autocall)
                }
                ast::Value::BuiltinOp(id) => {
                    let f = Ct::Id(env.issue_ct(id));
                    let ct_args = env.simplify(map.instantiation_of(self.id).unwrap());
                    let autocall = !map.ast(id).unwrap().ann.body.body.is_fun();
                    Rt::autocall(Rt::Capture(Ct::generic_inst(f, ct_args), None), autocall)
                }
                ast::Value::ClassMethod(id) => {
                    let method = map.ast(id).unwrap();
                    let (instance_inst, method_inst) = method.expand_external_instantiation(
                        map.instantiation_of(self.id).unwrap().clone(),
                    );
                    debug_assert_eq!(instance_inst.s_args.len(), 1);

                    let instance = env.simplify(&instance_inst.s_args[0]);
                    let f = Ct::table_get(instance, env.issue_ct(id));
                    let args = env.simplify(&method_inst);
                    let autocall = method.params.is_none();
                    Rt::autocall(Rt::Capture(Ct::generic_inst(f, args), None), autocall)
                }
                ast::Value::Parameter(id) => Rt::Local(env.issue_rt(id)),
                ast::Value::LocalVar(id) => Rt::Local(env.issue_rt(id)),
                ast::Value::LocalFun(id) => {
                    let id = env.issue_rt(id);
                    let ct_args = env.simplify(map.instantiation_of(self.id).unwrap());
                    Rt::LocalFun(id, ct_args)
                }
                ast::Value::PatternVar(id) => Rt::Local(env.issue_rt(id)),
            },
            ast::ExprRep::Con(con) => {
                let (f, autocall) = match con {
                    ast::ValueCon::Data(id) => (
                        Ct::Id(env.issue_ct(id)),
                        map.ast(id).unwrap().fields.is_none(),
                    ),
                    ast::ValueCon::Builtin(id) => (
                        Ct::Id(env.issue_ct(id)),
                        map.ast(id).unwrap().fields.is_none(),
                    ),
                };
                let ct_args = env.simplify(map.instantiation_of(self.id).unwrap());
                Rt::autocall(Rt::Capture(Ct::generic_inst(f, ct_args), None), autocall)
            }
            ast::ExprRep::Const(ref lit) => {
                let ty = env.simplify(map.type_of(self.id).unwrap());
                Rt::Const(env.simplify_const(ty, lit))
            }
            ast::ExprRep::App(ref apply) => {
                let args = env.simplify(&apply.args);
                if let ast::ExprRep::Con(con) = apply.callee.rep {
                    if con == ast::ValueCon::SYNTAX && args.len() == 1 {
                        let unit = map.textual_unit_of(self.id).unwrap();
                        let body = args.into_iter().next().unwrap();
                        let mut ct_args =
                            env.simplify(map.instantiation_of(apply.callee.id).unwrap());
                        assert_eq!(ct_args.len(), 1);
                        return Rt::construct_syntax(unit.loc, ct_args.remove(0), body);
                    }
                }
                let callee = env.simplify(&apply.callee);
                Rt::call(callee, args)
            }
            ast::ExprRep::Capture(ref use_) => {
                let construct = *use_.get_resolved();
                let unit = map.textual_unit_of(self.id).unwrap();
                Rt::Const(env.simplify_const_sexp(Sexp::Use(construct).pack(unit.loc)))
            }
            ast::ExprRep::Annotate(ref annotate) => env.simplify(&annotate.body),
            ast::ExprRep::Let(ref let_) => {
                let funs = let_.local_functions().map(|f| env.simplify(f)).collect();
                let vars = let_.local_vars().map(|v| env.simplify(v)).collect();
                let body = env.simplify(&let_.body);
                Rt::let_function(funs, Rt::let_var(vars, body))
            }
            ast::ExprRep::Seq(ref seq) => {
                let stmts = env.simplify(&seq.stmts);
                let ret = env.simplify(&seq.ret);
                Rt::seq(stmts, ret)
            }
            ast::ExprRep::If(ref if_) => {
                let cond = env.simplify(&if_.cond);
                let then = env.simplify(&if_.then);
                let else_ = env.simplify(&if_.else_);
                Rt::if_(cond, then, else_)
            }
            ast::ExprRep::While(ref while_) => {
                let cond = env.simplify(&while_.cond);
                let body = env.simplify(&while_.body);
                Rt::while_(cond, body)
            }
            ast::ExprRep::Match(ref match_) => {
                let target = env.simplify(&match_.target);
                let clauses = env.simplify(&match_.clauses);
                Rt::match_(target, clauses)
            }
            ast::ExprRep::Return(ref ret) => {
                let ret = env.simplify(&**ret).unwrap_or(Rt::Const(Const::Unit));
                Rt::return_(ret)
            }
        }
    }
}

impl Simplify for (ast::Pattern, ast::Expr) {
    type Dest = RtClause;

    fn simplify<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        let pat = env.simplify(&self.0);
        let body = env.simplify(&self.1);
        RtClause::new(pat, body)
    }
}

impl Simplify for ast::LocalFun {
    type Dest = RtFunction;

    fn simplify<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        let id = env.issue_rt(self.id);
        let scheme = env.module_map().scheme_of(self.id).unwrap();

        let (ct_params, params, ret_ty) = env.simplify_scheme(Some(Some(&self.params)), scheme);

        let body = env.simplify(&self.body);

        RtFunction::new(id, ct_params, params, ret_ty, body)
    }
}

impl Simplify for ast::LocalVar {
    type Dest = RtVar;

    fn simplify<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        let id = env.issue_rt(self.id);
        let ty = env.simplify(env.module_map().type_of(self.id).unwrap());
        let init = env.simplify(&self.init);
        RtVar::new(id, ty, init)
    }
}

impl Simplify for ast::Pattern {
    type Dest = RtPat;

    fn simplify<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        let map = env.module_map();
        match self.rep {
            ast::PatternRep::Var(ref var) => {
                let id = env.issue_rt(var.id);
                let ty = env.simplify(env.module_map().type_of(var.id).unwrap());
                let as_pat = env.simplify(&var.as_pat);
                RtPat::Var(id, ty, as_pat.map(Box::new))
            }
            ast::PatternRep::Wildcard => RtPat::Wildcard,
            ast::PatternRep::Decon(ref decon) => {
                let args = env.simplify(&decon.fields).unwrap_or_default();
                match *decon.use_.get_resolved() {
                    ast::ValueCon::Data(id) => {
                        let type_con = map.ast(map.ast(id).unwrap().type_con).unwrap();
                        let con = Ct::Id(env.issue_ct(type_con.con.id));
                        let ct_args = env.simplify(map.instantiation_of(self.id).unwrap());
                        let index = type_con.con.index_of(id).unwrap_or_default();
                        RtPat::Data(Ct::generic_inst(con, ct_args), index, args)
                    }
                    ast::ValueCon::Builtin(id) => {
                        let con = map.ast(id).unwrap();
                        let ct_args = env.simplify(map.instantiation_of(self.id).unwrap());
                        builtin_rt_pat(&con.builtin_name, ct_args, args)
                    }
                }
            }
            ast::PatternRep::Const(ref lit) => {
                let ty = env.simplify(map.type_of(self.id).unwrap());
                RtPat::Const(env.simplify_const(ty, lit))
            }
        }
    }
}

impl Simplify for ast::Type {
    type Dest = Ct;

    fn simplify<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        match self {
            Self::Use(use_) => match *use_.get_resolved() {},
            Self::Con(con) => Ct::Id(env.simplify(con)),
            Self::App(callee, args) => {
                let callee = env.simplify(&**callee);
                let args = env.simplify(args);
                Ct::generic_inst(callee, args)
            }
            Self::Gen(id) => Ct::Id(env.issue_ct(*id)),
            Self::Error(e) => panic!("Found Type::Error at translator: {}", e),
        }
    }
}

impl Simplify for ast::TypeCon {
    type Dest = CtId;

    fn simplify<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        match self {
            Self::Data(id) => env.issue_ct(*id),
            Self::Builtin(id) => env.issue_ct(*id),
        }
    }
}

impl Simplify for ast::TypeParameter {
    type Dest = CtId;

    fn simplify<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        env.issue_ct(self.id)
    }
}

impl Simplify for ast::Constraint {
    type Dest = CtId;

    fn simplify<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        env.issue_ct(self.id)
    }
}

impl Simplify for ast::Satisfaction {
    type Dest = Ct;

    fn simplify<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        match self {
            Self::ByPremise(by_premise) => env.simplify(by_premise),
            Self::ByInstance(by_inst) => env.simplify(by_inst),
            Self::Error(e) => panic!("Found Satisfaction::Error at translator: {}", e),
        }
    }
}

impl Simplify for ast::SatisfactionByPremise {
    type Dest = Ct;

    fn simplify<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        let s = Ct::Id(env.issue_ct(self.id));
        self.path
            .iter()
            .fold(s, |s, c| Ct::table_get(s, env.issue_ct(*c)))
    }
}

impl Simplify for ast::SatisfactionByInstance {
    type Dest = Ct;

    fn simplify<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        let s = Ct::Id(env.issue_ct(*self.use_.get_resolved()));
        let args = env.simplify(&self.instantiation);
        Ct::generic_inst(s, args)
    }
}

impl Simplify for ast::Instantiation {
    type Dest = Vec<Ct>;

    fn simplify<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        let mut args = env.simplify(&self.ty_args);
        args.append(&mut env.simplify(&self.s_args));
        args
    }
}

fn builtin_ct(name: &str, args: &[CtId]) -> Ct {
    match (name, args) {
        ("fun", [args @ .., ret]) => {
            let args = args.into_iter().copied().map(Ct::Id).collect::<Vec<_>>();
            let ret = Ct::Id(*ret);
            Ct::clos(args, ret)
        }
        ("i8", []) => Ct::S(8),
        ("i16", []) => Ct::S(16),
        ("i32", []) => Ct::S(32),
        ("i64", []) => Ct::S(64),
        ("u8", []) => Ct::U(8),
        ("u16", []) => Ct::U(16),
        ("u32", []) => Ct::U(32),
        ("u64", []) => Ct::U(64),
        ("f32", []) => Ct::F32,
        ("f64", []) => Ct::F64,
        ("ptr", [id]) => Ct::ptr(Ct::Id(*id)),
        ("string", []) => Ct::String,
        ("char", []) => Ct::Char,
        ("array", [id]) => Ct::array(Ct::Id(*id)),
        ("captured-use", []) => Ct::CapturedUse,
        ("syntax", [id]) => Ct::syntax(Ct::Id(*id)),
        (name, _) => panic!("Unsupported builtin-type {} (args={})", name, args.len()),
    }
}

fn builtin_rt(unit: &TextualUnit, name: &str, mut ct_args: Vec<Ct>, mut args: Vec<Rt>) -> Rt {
    use std::mem::take;

    match (name, ct_args.as_mut_slice(), args.as_mut_slice()) {
        ("no-op", _, [a]) => take(a),
        ("size-of", [ty], _) => Rt::nullary(Nullary::SizeOf(take(ty))),
        ("align-of", [ty], _) => Rt::nullary(Nullary::AlignOf(take(ty))),
        ("and", [], [a, b]) => Rt::and(take(a), take(b)),
        ("or", [], [a, b]) => Rt::or(take(a), take(b)),
        ("not", [], [a]) => Rt::unary(Unary::Not, take(a)),
        ("bitcast", [_, ty], [a]) => Rt::unary(Unary::BitCast(take(ty)), take(a)),
        ("integer.eq", _, [a, b]) => Rt::binary(Binary::IEq, take(a), take(b)),
        ("integer.shl", _, [a, b]) => Rt::binary(Binary::IShl, take(a), take(b)),
        ("integer.ashr", _, [a, b]) => Rt::binary(Binary::IAShr, take(a), take(b)),
        ("integer.lshr", _, [a, b]) => Rt::binary(Binary::ILShr, take(a), take(b)),
        ("integer.and", _, [a, b]) => Rt::binary(Binary::IAnd, take(a), take(b)),
        ("integer.or", _, [a, b]) => Rt::binary(Binary::IOr, take(a), take(b)),
        ("integer.xor", _, [a, b]) => Rt::binary(Binary::IXor, take(a), take(b)),
        ("integer.complement", _, [a]) => Rt::unary(Unary::IComplement, take(a)),
        ("integer.popcount", _, [a]) => Rt::unary(Unary::IPopCount, take(a)),
        ("integer.trunc", [_, ty], [a]) => Rt::unary(Unary::ITrunc(take(ty)), take(a)),
        ("signed.lt", _, [a, b]) => Rt::binary(Binary::SLt, take(a), take(b)),
        ("signed.le", _, [a, b]) => Rt::binary(Binary::SLe, take(a), take(b)),
        ("signed.gt", _, [a, b]) => Rt::binary(Binary::SGt, take(a), take(b)),
        ("signed.ge", _, [a, b]) => Rt::binary(Binary::SGe, take(a), take(b)),
        ("signed.add", _, [a, b]) => Rt::binary(Binary::SAdd, take(a), take(b)),
        ("signed.sub", _, [a, b]) => Rt::binary(Binary::SSub, take(a), take(b)),
        ("signed.mul", _, [a, b]) => Rt::binary(Binary::SMul, take(a), take(b)),
        ("signed.div", _, [a, b]) => Rt::binary(Binary::SDiv, take(a), take(b)),
        ("signed.rem", _, [a, b]) => Rt::binary(Binary::SRem, take(a), take(b)),
        ("signed.ext", [_, ty], [a]) => Rt::unary(Unary::SExt(take(ty)), take(a)),
        ("signed.to-float", [_, ty], [a]) => Rt::unary(Unary::SToF(take(ty)), take(a)),
        ("unsigned.lt", _, [a, b]) => Rt::binary(Binary::ULt, take(a), take(b)),
        ("unsigned.le", _, [a, b]) => Rt::binary(Binary::ULe, take(a), take(b)),
        ("unsigned.gt", _, [a, b]) => Rt::binary(Binary::UGt, take(a), take(b)),
        ("unsigned.ge", _, [a, b]) => Rt::binary(Binary::UGe, take(a), take(b)),
        ("unsigned.add", _, [a, b]) => Rt::binary(Binary::UAdd, take(a), take(b)),
        ("unsigned.sub", _, [a, b]) => Rt::binary(Binary::USub, take(a), take(b)),
        ("unsigned.mul", _, [a, b]) => Rt::binary(Binary::UMul, take(a), take(b)),
        ("unsigned.div", _, [a, b]) => Rt::binary(Binary::UDiv, take(a), take(b)),
        ("unsigned.rem", _, [a, b]) => Rt::binary(Binary::URem, take(a), take(b)),
        ("unsigned.ext", [_, ty], [a]) => Rt::unary(Unary::UExt(take(ty)), take(a)),
        ("unsigned.to-float", [_, ty], [a]) => Rt::unary(Unary::UToF(take(ty)), take(a)),
        ("float.eq", _, [a, b]) => Rt::binary(Binary::FEq, take(a), take(b)),
        ("float.lt", _, [a, b]) => Rt::binary(Binary::FLt, take(a), take(b)),
        ("float.le", _, [a, b]) => Rt::binary(Binary::FLe, take(a), take(b)),
        ("float.gt", _, [a, b]) => Rt::binary(Binary::FGt, take(a), take(b)),
        ("float.ge", _, [a, b]) => Rt::binary(Binary::FGe, take(a), take(b)),
        ("float.add", _, [a, b]) => Rt::binary(Binary::FAdd, take(a), take(b)),
        ("float.sub", _, [a, b]) => Rt::binary(Binary::FSub, take(a), take(b)),
        ("float.mul", _, [a, b]) => Rt::binary(Binary::FMul, take(a), take(b)),
        ("float.div", _, [a, b]) => Rt::binary(Binary::FDiv, take(a), take(b)),
        ("float.rem", _, [a, b]) => Rt::binary(Binary::FRem, take(a), take(b)),
        ("float.to-signed", [_, ty], [a]) => Rt::unary(Unary::FToS(take(ty)), take(a)),
        ("float.to-unsigned", [_, ty], [a]) => Rt::unary(Unary::FToU(take(ty)), take(a)),
        ("float.trunc", [_, ty], [a]) => Rt::unary(Unary::FTrunc(take(ty)), take(a)),
        ("float.ext", [_, ty], [a]) => Rt::unary(Unary::FExt(take(ty)), take(a)),
        ("real.ceil", _, [a]) => Rt::unary(Unary::RealCeil, take(a)),
        ("real.floor", _, [a]) => Rt::unary(Unary::RealFloor, take(a)),
        ("real.trunc", _, [a]) => Rt::unary(Unary::RealTrunc, take(a)),
        ("real.round", _, [a]) => Rt::unary(Unary::RealRound, take(a)),
        ("math.sqrt", _, [a]) => Rt::unary(Unary::MathSqrt, take(a)),
        ("math.sin", _, [a]) => Rt::unary(Unary::MathSin, take(a)),
        ("math.cos", _, [a]) => Rt::unary(Unary::MathCos, take(a)),
        ("math.pow", _, [a, b]) => Rt::binary(Binary::MathPow, take(a), take(b)),
        ("math.exp", _, [a]) => Rt::unary(Unary::MathExp, take(a)),
        ("math.log", _, [a]) => Rt::unary(Unary::MathLog, take(a)),
        ("string.genid", _, []) => Rt::nullary(Nullary::GenId),
        ("string.construct", _, [a, b]) => Rt::binary(Binary::StringConstruct, take(a), take(b)),
        ("string.eq", _, [a, b]) => Rt::binary(Binary::StringEq, take(a), take(b)),
        ("string.cmp", _, [a, b]) => Rt::binary(Binary::StringCmp, take(a), take(b)),
        ("string.concat", _, [a, b]) => Rt::binary(Binary::StringConcat, take(a), take(b)),
        ("string.ptr", _, [a]) => Rt::unary(Unary::StringPtr, take(a)),
        ("string.length", _, [a]) => Rt::unary(Unary::StringLength, take(a)),
        ("ptr", [_], [a]) => Rt::alloc(Location::Heap, take(a)),
        ("non-null", [_], [a]) => Rt::alloc(Location::Heap, take(a)),
        ("null", [ty], []) => Rt::nullary(Nullary::Null(take(ty))),
        ("ptr.eq", _, [a, b]) => Rt::binary(Binary::PtrEq, take(a), take(b)),
        ("ptr.lt", _, [a, b]) => Rt::binary(Binary::PtrLt, take(a), take(b)),
        ("ptr.le", _, [a, b]) => Rt::binary(Binary::PtrLe, take(a), take(b)),
        ("ptr.gt", _, [a, b]) => Rt::binary(Binary::PtrGt, take(a), take(b)),
        ("ptr.ge", _, [a, b]) => Rt::binary(Binary::PtrGe, take(a), take(b)),
        ("ptr.load", [_], [a]) => Rt::unary(Unary::Load, take(a)),
        ("ptr.store", [_], [a, b]) => Rt::binary(Binary::Store, take(a), take(b)),
        ("ptr.offset", [_], [a, b]) => Rt::binary(Binary::Offset, take(a), take(b)),
        ("ptr.cast", [_, ty], [a]) => Rt::unary(Unary::BitCast(Ct::ptr(take(ty))), take(a)),
        ("ptr.copy", _, [a, b, c]) => Rt::ternary(Ternary::PtrCopy, take(a), take(b), take(c)),
        ("ptr.move", _, [a, b, c]) => Rt::ternary(Ternary::PtrMove, take(a), take(b), take(c)),
        ("ptr.to-integer", _, [ptr]) => Rt::unary(Unary::PtrToI, take(ptr)),
        ("array.construct", _, [a, b]) => Rt::binary(Binary::ArrayConstruct, take(a), take(b)),
        ("array.ptr", _, [a]) => Rt::unary(Unary::ArrayPtr, take(a)),
        ("array.length", _, [a]) => Rt::unary(Unary::ArrayLength, take(a)),
        ("array.load", _, [a, b]) => Rt::binary(Binary::ArrayLoad, take(a), take(b)),
        ("array.store", _, [a, b, c]) => {
            Rt::ternary(Ternary::ArrayStore, take(a), take(b), take(c))
        }
        ("array.alloc", [ty], [a]) => Rt::alloc_array(Location::Heap, take(ty), take(a)),
        ("array.stackalloc", [ty], [a]) => Rt::alloc_array(Location::Stack, take(ty), take(a)),
        ("integer.to-ptr", [ty], [a]) => Rt::unary(Unary::IToPtr(take(ty)), take(a)),
        ("syntax", [ty], [a]) => Rt::construct_syntax(unit.loc, take(ty), take(a)),
        ("panic", _, [a]) => Rt::unary(Unary::Panic, take(a)),
        (name, _, _) => panic!(
            "Unsupported builtin-value {} (ct_args={}, args={})",
            name,
            ct_args.len(),
            args.len(),
        ),
    }
}

fn builtin_rt_pat(name: &str, mut ct_args: Vec<Ct>, mut args: Vec<RtPat>) -> RtPat {
    use std::mem::take;

    match (name, ct_args.as_mut_slice(), args.as_mut_slice()) {
        ("ptr", [_], [a]) => RtPat::deref(take(a)),
        ("non-null", [ty], [a]) => RtPat::non_null(take(ty), take(a)),
        ("null", [ty], []) => RtPat::Null(take(ty)),
        ("syntax", [ty], [a]) => RtPat::Syntax(take(ty), Box::new(take(a))),
        (name, _, _) => panic!(
            "Unsupported builtin-pattern {} (ct_args={}, args={})",
            name,
            ct_args.len(),
            args.len(),
        ),
    }
}
