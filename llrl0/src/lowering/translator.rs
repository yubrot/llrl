//! Translates module constructs into the lowering IR.

use super::ir::*;
use crate::ast;
use crate::module::{ModuleSet, Symbol};

pub fn translate<'m, T: Translate>(src: &T, env: &mut impl Env<'m>) -> T::Dest {
    Translate::translate(src, env)
}

pub trait Env<'m>: Sized {
    type ModuleSet: ModuleSet + 'm;

    fn module_set(&self) -> &'m Self::ModuleSet;

    fn alloc_ct(&mut self) -> CtId;

    fn issue_ct(&mut self, construct: impl Into<ast::Construct>) -> CtId;

    fn alloc_rt(&mut self) -> RtId;

    fn issue_rt(&mut self, construct: impl Into<ast::Construct>) -> RtId;

    fn translate<T: Translate + ?Sized>(&mut self, src: &T) -> T::Dest {
        src.translate(self)
    }

    fn translate_def(&mut self, src: ast::Construct) -> Option<Def> {
        let set = self.module_set();
        match src {
            ast::Construct::Function(id) => Some(self.translate(set.ast(id).unwrap())),
            ast::Construct::CFunction(id) => Some(self.translate(set.ast(id).unwrap())),
            ast::Construct::BuiltinOp(id) => Some(self.translate(set.ast(id).unwrap())),
            ast::Construct::Macro(id) => Some(self.translate(set.ast(id).unwrap())),
            ast::Construct::DataTypeCon(id) => Some(self.translate(&set.ast(id).unwrap())),
            ast::Construct::DataValueCon(id) => Some(self.translate(set.ast(id).unwrap())),
            ast::Construct::BuiltinTypeCon(id) => Some(self.translate(set.ast(id).unwrap().con)),
            ast::Construct::BuiltinValueCon(id) => Some(self.translate(set.ast(id).unwrap())),
            ast::Construct::ClassMethod(id) => self.translate(set.ast(id).unwrap()),
            ast::Construct::InstanceMethod(id) => Some(self.translate(set.ast(id).unwrap())),
            ast::Construct::InstanceCon(id) => Some(self.translate(&set.ast(id).unwrap())),
            _ => None,
        }
    }

    fn translate_scheme(
        &mut self,
        signature: Option<Option<&Vec<ast::Parameter>>>,
        scheme: &ast::Scheme,
    ) -> (Vec<CtId>, Vec<RtParam>, Ct) {
        let mut ct_params = self.translate(&scheme.ty_params);
        ct_params.append(&mut self.translate(&scheme.s_params));

        let (params, ret_ty) = match (scheme.body.matches_fun(), signature) {
            (_, Some(None)) | (None, None) => (Vec::new(), self.translate(&scheme.body)),
            (Some((param_tys, ret_ty)), Some(Some(params))) => {
                let params = params
                    .iter()
                    .zip(param_tys)
                    .map(|(param, ty)| {
                        let id = self.translate(param);
                        let ty = self.translate(ty);
                        RtParam::new(id, ty)
                    })
                    .collect();
                let ret_ty = self.translate(ret_ty);
                (params, ret_ty)
            }
            (Some((param_tys, ret_ty)), None) => {
                let params = self
                    .translate(param_tys)
                    .into_iter()
                    .map(|ty| RtParam::new(self.alloc_rt(), ty))
                    .collect();
                let ret_ty = self.translate(ret_ty);
                (params, ret_ty)
            }
            (None, Some(Some(_))) => panic!("scheme does not match signature"),
        };

        (ct_params, params, ret_ty)
    }

    fn translate_const(&mut self, ty: Ct, const_: &ast::Const) -> Const {
        match const_ {
            ast::Const::Integer(signed, v) => Const::Integer(ty, *signed, *v),
            ast::Const::FPNumber(v) => Const::FPNumber(ty, *v),
            ast::Const::String(s) => Const::String(s.clone()),
            ast::Const::Char(c) => Const::Char(*c),
            ast::Const::SyntaxSexp(s) => self.translate_const_syntax_sexp(s.as_ref().clone()),
        }
    }

    fn translate_const_syntax_sexp(&mut self, syntax_sexp: Syntax<Sexp>) -> Const {
        let ty = self.issue_ct(ast::builtin::SEXP);
        Const::SyntaxSexp(Ct::Id(ty), Box::new(syntax_sexp))
    }
}

pub trait Translate {
    type Dest;

    fn translate<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest;
}

impl<T: Translate> Translate for [T] {
    type Dest = Vec<T::Dest>;

    fn translate<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        self.iter().map(|a| env.translate(a)).collect()
    }
}

impl<T: Translate> Translate for &'_ T {
    type Dest = T::Dest;

    fn translate<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        env.translate(&**self)
    }
}

impl<T: Translate> Translate for Vec<T> {
    type Dest = Vec<T::Dest>;

    fn translate<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        self.iter().map(|a| env.translate(a)).collect()
    }
}

impl<T: Translate> Translate for Option<T> {
    type Dest = Option<T::Dest>;

    fn translate<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        self.as_ref().map(|a| env.translate(a))
    }
}

impl Translate for ast::NodeId<ast::Function> {
    type Dest = Ct;

    fn translate<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        Ct::Id(env.issue_ct(*self))
    }
}

impl Translate for ast::Function {
    type Dest = Def;

    fn translate<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        let scheme = env.module_set().scheme_of(self.id).unwrap();

        let (ct_params, params, ret_ty) = env.translate_scheme(Some(self.params.as_ref()), scheme);
        let body = env.translate(&self.body);

        Def::generic(
            ct_params,
            Def::Function(Function::standard(
                None,
                params,
                ret_ty,
                body,
                self.transparent,
            )),
        )
    }
}

impl Translate for ast::Parameter {
    type Dest = RtId;

    fn translate<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        env.issue_rt(self.id)
    }
}

impl Translate for ast::CFunction {
    type Dest = Def;

    fn translate<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        let (params, ret_ty) = match self.ann.body.matches_fun() {
            Some((param_tys, ret_ty)) => {
                let params = env
                    .translate(param_tys)
                    .into_iter()
                    .map(|ty| RtParam::new(env.alloc_rt(), ty))
                    .collect();
                let ret_ty = env.translate(ret_ty);
                (params, ret_ty)
            }
            None => (Vec::new(), env.translate(&self.ann.body)),
        };

        let args = params.iter().map(|p| Rt::Var(p.id, p.ty.clone())).collect();
        let callee = RtCallee::CDirect(self.c_name.clone(), ret_ty.clone());

        Def::Function(Function::standard(
            None,
            params,
            ret_ty,
            Rt::call(callee, args),
            // Set to false to keep the evaluation order consistent, but it could be set to true:
            false,
        ))
    }
}

impl Translate for ast::BuiltinOp {
    type Dest = Def;

    fn translate<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        let symbol = env.module_set().symbol_of(self.id).unwrap();
        let (ct_params, params, ret_ty) = env.translate_scheme(None, &self.ann.body);

        let ct_args = ct_params.iter().map(|p| Ct::Id(*p)).collect();
        let args = params.iter().map(|p| Rt::Var(p.id, p.ty.clone())).collect();
        let rt = builtin_rt(symbol, &self.builtin_name, ct_args, args, &ret_ty);

        Def::generic(
            ct_params,
            Def::Function(Function::standard(None, params, ret_ty, rt, true)),
        )
    }
}

impl Translate for ast::NodeId<ast::Macro> {
    type Dest = Ct;

    fn translate<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        Ct::Id(env.issue_ct(*self))
    }
}

impl Translate for ast::Macro {
    type Dest = Def;

    fn translate<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        let src_ty = env.translate(&ast::Macro::src_ty());
        let dest_ty = env.translate(&ast::Macro::dest_ty());

        let param = RtParam::new(env.translate(&self.param), src_ty);
        let rt = env.translate(&self.body);

        Def::Function(Function::r#macro(param, dest_ty, rt))
    }
}

impl<'a> Translate for ast::DataType<'a> {
    type Dest = Def;

    fn translate<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        let repr = env.translate(&self.con.repr);
        let params = env.translate(&self.con.ty_params);
        let cons = self
            .value_cons()
            .map(|value_con| env.translate(&value_con.fields).unwrap_or_default())
            .collect();

        Def::generic(params, Def::Data(Data::new(repr, cons)))
    }
}

impl Translate for ast::DataRepr {
    type Dest = DataRepr;

    fn translate<'m>(&self, _env: &mut impl Env<'m>) -> Self::Dest {
        match self {
            Self::Default => DataRepr::Boxed,
            Self::Value => DataRepr::Value,
            Self::C => DataRepr::C,
        }
    }
}

impl Translate for ast::DataValueCon {
    type Dest = Def;

    fn translate<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        let index = {
            let type_con = env.module_set().ast(self.type_con).unwrap();
            type_con.con.index_of(self.id).unwrap()
        };

        let scheme = env.module_set().scheme_of(self.id).unwrap();

        let (ct_params, params, ret_ty) = env.translate_scheme(None, scheme);

        let args = params.iter().map(|p| Rt::Var(p.id, p.ty.clone())).collect();
        let rt = Rt::construct_data(ret_ty.clone(), index, args);

        Def::generic(
            ct_params,
            Def::Function(Function::standard(None, params, ret_ty, rt, true)),
        )
    }
}

impl Translate for ast::BuiltinTypeCon {
    type Dest = Def;

    fn translate<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        let params = env.translate(&self.ty_params);
        let ty = builtin_ct(&self.builtin_name, &params);
        Def::generic(params, Def::Alias(ty))
    }
}

impl Translate for ast::BuiltinValueCon {
    type Dest = Def;

    fn translate<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        let symbol = env.module_set().symbol_of(self.id).unwrap();
        let scheme = env.module_set().scheme_of(self.id).unwrap();

        let (ct_params, params, ty) = env.translate_scheme(None, scheme);

        let ct_args = ct_params.iter().map(|p| Ct::Id(*p)).collect();
        let args = params.iter().map(|p| Rt::Var(p.id, p.ty.clone())).collect();
        let rt = builtin_rt(symbol, &self.builtin_name, ct_args, args, &ty);

        Def::generic(
            ct_params,
            Def::Function(Function::standard(None, params, ty, rt, true)),
        )
    }
}

impl Translate for ast::ValueConField {
    type Dest = Ct;

    fn translate<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        env.translate(&self.ty)
    }
}

impl<'a> Translate for ast::Instance<'a> {
    type Dest = Def;

    fn translate<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        let ast::ConstraintRep::Class(ref class_id, ref class_args) = self.con.target.rep;
        let class = env.module_set().ast(*class_id.get_resolved()).unwrap();

        let mut params = env.translate(&self.con.ty_params);
        params.append(&mut env.translate(&self.con.s_params));

        let inst_args = params.iter().copied().map(Ct::Id).collect::<Vec<_>>();

        let default_impl_args = {
            let this = Ct::generic_inst(Ct::Id(env.issue_ct(self.con.id)), inst_args.clone());
            let mut args = env.translate(class_args);
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
        let instance_inst = env.module_set().instantiation_of(self.con.id).unwrap();
        for (c, s) in superclass_cs.iter().zip(instance_inst.s_args.iter()) {
            let c = env.translate(c);
            let s = env.translate(s);
            table.insert(c, s);
        }

        Def::generic(params, Def::AliasTable(table))
    }
}

impl Translate for ast::ClassMethod {
    type Dest = Option<Def>;

    fn translate<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        if let Some(ref body) = self.default_body {
            let class = env.module_set().ast(self.class_con).unwrap();

            let mut class_ct_params = env.translate(&class.con.ty_params);
            class_ct_params.push(env.translate(&class.con.constraint()));

            let (method_ct_params, params, ret_ty) =
                env.translate_scheme(Some(self.params.as_ref()), &self.ann.body);

            let body = env.translate(body);

            Some(Def::generic(
                class_ct_params,
                Def::generic(
                    method_ct_params,
                    Def::Function(Function::standard(
                        None,
                        params,
                        ret_ty,
                        body,
                        self.transparent,
                    )),
                ),
            ))
        } else {
            None
        }
    }
}

impl Translate for ast::InstanceMethod {
    type Dest = Def;

    fn translate<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        let inst = env.module_set().ast(self.instance_con).unwrap();
        let scheme = env.module_set().scheme_of(self.id).unwrap();

        let mut inst_ct_params = env.translate(&inst.con.ty_params);
        inst_ct_params.append(&mut env.translate(&inst.con.s_params));

        let (method_ct_params, params, ret_ty) =
            env.translate_scheme(Some(self.params.as_ref()), scheme);

        let body = env.translate(&self.body);

        Def::generic(
            inst_ct_params,
            Def::generic(
                method_ct_params,
                Def::Function(Function::standard(
                    None,
                    params,
                    ret_ty,
                    body,
                    self.transparent,
                )),
            ),
        )
    }
}

impl Translate for ast::InitExpr {
    type Dest = Option<Init>;

    fn translate<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        match self {
            ast::InitExpr::Eval(expr) => {
                let ty = env.translate(env.module_set().type_of(expr.id).unwrap());
                let expr = env.translate(expr);
                Some(Init::new(ty, expr))
            }
            ast::InitExpr::EnsureInitialized(_) => None,
        }
    }
}

impl Translate for ast::Expr {
    type Dest = Rt;

    fn translate<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        let set = env.module_set();
        match self.rep {
            ast::ExprRep::Use(ref use_) => {
                let ty = env.translate(set.type_of(self.id).unwrap());
                match *use_.get_resolved() {
                    ast::Value::Function(id) => {
                        let ct_args = env.translate(set.instantiation_of(self.id).unwrap());
                        Rt::autocall(
                            Ct::generic_inst(Ct::Id(env.issue_ct(id)), ct_args),
                            ty,
                            set.ast(id).unwrap().params.is_none(),
                        )
                    }
                    ast::Value::CFunction(id) => Rt::autocall(
                        Ct::Id(env.issue_ct(id)),
                        ty,
                        !set.ast(id).unwrap().ann.body.is_fun(),
                    ),
                    ast::Value::BuiltinOp(id) => {
                        let ct_args = env.translate(set.instantiation_of(self.id).unwrap());
                        Rt::autocall(
                            Ct::generic_inst(Ct::Id(env.issue_ct(id)), ct_args),
                            ty,
                            !set.ast(id).unwrap().ann.body.body.is_fun(),
                        )
                    }
                    ast::Value::ClassMethod(id) => {
                        let method = set.ast(id).unwrap();
                        let (instance_inst, method_inst) = method.expand_external_instantiation(
                            set.instantiation_of(self.id).unwrap().clone(),
                        );
                        debug_assert_eq!(instance_inst.s_args.len(), 1);

                        let instance = env.translate(&instance_inst.s_args[0]);
                        let args = env.translate(&method_inst);
                        Rt::autocall(
                            Ct::generic_inst(Ct::table_get(instance, env.issue_ct(id)), args),
                            ty,
                            method.params.is_none(),
                        )
                    }
                    ast::Value::Parameter(id) => Rt::Var(env.issue_rt(id), ty),
                    ast::Value::LocalVar(id) => Rt::Var(env.issue_rt(id), ty),
                    ast::Value::LocalFun(id) => {
                        let id = env.issue_rt(id);
                        let args = env.translate(set.instantiation_of(self.id).unwrap());
                        Rt::local_fun(id, args, ty)
                    }
                    ast::Value::PatternVar(id) => Rt::Var(env.issue_rt(id), ty),
                }
            }
            ast::ExprRep::Con(con) => {
                let ty = env.translate(set.type_of(self.id).unwrap());
                let ct_args = env.translate(set.instantiation_of(self.id).unwrap());
                let f = match con {
                    ast::ValueCon::Data(id) => Ct::Id(env.issue_ct(id)),
                    ast::ValueCon::Builtin(id) => Ct::Id(env.issue_ct(id)),
                };
                let autocall = match con {
                    ast::ValueCon::Data(id) => set.ast(id).unwrap().fields.is_none(),
                    ast::ValueCon::Builtin(id) => set.ast(id).unwrap().fields.is_none(),
                };
                Rt::autocall(Ct::generic_inst(f, ct_args), ty, autocall)
            }
            ast::ExprRep::Const(ref lit) => {
                let ty = env.translate(set.type_of(self.id).unwrap());
                Rt::Const(env.translate_const(ty, lit))
            }
            ast::ExprRep::App(ref apply) => {
                let args = env.translate(&apply.args);
                if let ast::ExprRep::Con(con) = apply.callee.rep {
                    if con == ast::ValueCon::SYNTAX && args.len() == 1 {
                        let symbol = set.symbol_of(self.id).unwrap();
                        let body = args.into_iter().next().unwrap();
                        return Rt::construct_syntax(symbol.loc, body);
                    }
                }
                let callee = env.translate(&apply.callee);
                Rt::call(RtCallee::Standard(callee), args)
            }
            ast::ExprRep::Capture(ref use_) => {
                let construct = *use_.get_resolved();
                let symbol = set.symbol_of(self.id).unwrap();
                Rt::Const(env.translate_const_syntax_sexp(Sexp::Use(construct).pack(symbol.loc)))
            }
            ast::ExprRep::Annotate(ref annotate) => env.translate(&annotate.body),
            ast::ExprRep::Let(ref let_) => {
                let funs = let_.local_functions().map(|f| env.translate(f)).collect();
                let vars = let_.local_vars().map(|v| env.translate(v)).collect();
                let body = env.translate(&let_.body);
                Rt::let_local_fun(funs, Rt::let_var(vars, body))
            }
            ast::ExprRep::Seq(ref seq) => {
                let stmts = env.translate(&seq.stmts);
                let ret = env.translate(&seq.ret);
                Rt::seq(stmts, ret)
            }
            ast::ExprRep::If(ref if_) => {
                let cond = env.translate(&if_.cond);
                let then = env.translate(&if_.then);
                let else_ = env.translate(&if_.else_);
                Rt::if_(cond, then, else_)
            }
            ast::ExprRep::While(ref while_) => {
                let cond = env.translate(&while_.cond);
                let body = env.translate(&while_.body);
                Rt::while_(cond, body)
            }
            ast::ExprRep::Match(ref match_) => {
                let target = env.translate(&match_.target);
                let clauses = env.translate(&match_.clauses);
                Rt::match_(target, clauses)
            }
            ast::ExprRep::Return(ref ret) => {
                let ret = env.translate(&**ret).unwrap_or(Rt::Const(Const::Unit));
                Rt::return_(ret)
            }
        }
    }
}

impl Translate for (ast::Pattern, ast::Expr) {
    type Dest = RtClause;

    fn translate<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        let pat = env.translate(&self.0);
        let body = env.translate(&self.1);
        RtClause::new(pat, body)
    }
}

impl Translate for ast::LocalFun {
    type Dest = RtLocalFun;

    fn translate<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        let id = env.issue_rt(self.id);
        let scheme = env.module_set().scheme_of(self.id).unwrap();

        let (ct_params, params, ret_ty) = env.translate_scheme(Some(Some(&self.params)), scheme);

        let body = env.translate(&self.body);

        RtLocalFun::new(id, ct_params, params, ret_ty, body)
    }
}

impl Translate for ast::LocalVar {
    type Dest = RtVar;

    fn translate<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        let id = env.issue_rt(self.id);
        let ty = env.translate(env.module_set().type_of(self.id).unwrap());
        let init = env.translate(&self.init);
        RtVar::new(id, ty, init)
    }
}

impl Translate for ast::Pattern {
    type Dest = RtPat;

    fn translate<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        let set = env.module_set();
        match self.rep {
            ast::PatternRep::Var(ref var) => {
                let id = env.issue_rt(var.id);
                let ty = env.translate(set.type_of(var.id).unwrap());
                let as_pat = env.translate(&var.as_pat);
                RtPat::Var(id, ty, as_pat.map(Box::new))
            }
            ast::PatternRep::Wildcard => {
                let ty = env.translate(set.type_of(self.id).unwrap());
                RtPat::Wildcard(ty)
            }
            ast::PatternRep::Decon(ref decon) => {
                let args = env.translate(&decon.fields).unwrap_or_default();
                match *decon.use_.get_resolved() {
                    ast::ValueCon::Data(id) => {
                        let type_con = set.ast(set.ast(id).unwrap().type_con).unwrap();
                        let con = Ct::Id(env.issue_ct(type_con.con.id));
                        let ct_args = env.translate(set.instantiation_of(self.id).unwrap());
                        let index = type_con.con.index_of(id).unwrap_or_default();
                        RtPat::Data(Ct::generic_inst(con, ct_args), index, args)
                    }
                    ast::ValueCon::Builtin(id) => {
                        let con = set.ast(id).unwrap();
                        let ct_args = env.translate(set.instantiation_of(self.id).unwrap());
                        builtin_rt_pat(&con.builtin_name, ct_args, args)
                    }
                }
            }
            ast::PatternRep::Const(ref lit) => {
                let ty = env.translate(set.type_of(self.id).unwrap());
                RtPat::Const(env.translate_const(ty, lit))
            }
        }
    }
}

impl Translate for ast::Type {
    type Dest = Ct;

    fn translate<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        match self {
            Self::Use(use_) => match *use_.get_resolved() {},
            Self::Con(con) => Ct::Id(env.translate(con)),
            Self::App(callee, args) => {
                let callee = env.translate(&**callee);
                let args = env.translate(args);
                Ct::generic_inst(callee, args)
            }
            Self::Gen(id) => Ct::Id(env.issue_ct(*id)),
            Self::Error(e) => panic!("Found Type::Error at translator: {}", e),
        }
    }
}

impl Translate for ast::TypeCon {
    type Dest = CtId;

    fn translate<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        match self {
            Self::Data(id) => env.issue_ct(*id),
            Self::Builtin(id) => env.issue_ct(*id),
        }
    }
}

impl Translate for ast::TypeParameter {
    type Dest = CtId;

    fn translate<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        env.issue_ct(self.id)
    }
}

impl Translate for ast::Constraint {
    type Dest = CtId;

    fn translate<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        env.issue_ct(self.id)
    }
}

impl Translate for ast::Satisfaction {
    type Dest = Ct;

    fn translate<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        match self {
            Self::ByPremise(by_premise) => env.translate(by_premise),
            Self::ByInstance(by_inst) => env.translate(by_inst),
            Self::Error(e) => panic!("Found Satisfaction::Error at translator: {}", e),
        }
    }
}

impl Translate for ast::SatisfactionByPremise {
    type Dest = Ct;

    fn translate<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        let s = Ct::Id(env.issue_ct(self.id));
        self.path
            .iter()
            .fold(s, |s, c| Ct::table_get(s, env.issue_ct(*c)))
    }
}

impl Translate for ast::SatisfactionByInstance {
    type Dest = Ct;

    fn translate<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        let s = Ct::Id(env.issue_ct(*self.use_.get_resolved()));
        let args = env.translate(&self.instantiation);
        Ct::generic_inst(s, args)
    }
}

impl Translate for ast::Instantiation {
    type Dest = Vec<Ct>;

    fn translate<'m>(&self, env: &mut impl Env<'m>) -> Self::Dest {
        let mut args = env.translate(&self.ty_args);
        args.append(&mut env.translate(&self.s_args));
        args
    }
}

fn builtin_ct(name: &str, args: &[CtId]) -> Ct {
    match (name, args) {
        ("fun", [args @ .., ret]) => {
            let args = args.iter().copied().map(Ct::Id).collect::<Vec<_>>();
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

fn builtin_rt(
    symbol: &Symbol,
    name: &str,
    mut ct_args: Vec<Ct>,
    mut args: Vec<Rt>,
    ret_ty: &Ct,
) -> Rt {
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
        ("ptr.temporary", [_], [a]) => Rt::alloc(Location::StackStatic, take(a)),
        ("ptr.load", [_], [a]) => Rt::unary(Unary::Load, take(a)),
        ("ptr.store", [_], [a, b]) => Rt::binary(Binary::Store, take(a), take(b)),
        ("ptr.offset", [_], [a, b]) => Rt::binary(Binary::Offset, take(a), take(b)),
        ("ptr.cast", [_, ty], [a]) => Rt::unary(Unary::BitCast(Ct::ptr(take(ty))), take(a)),
        ("ptr.copy", _, [a, b, c]) => Rt::ternary(Ternary::PtrCopy, take(a), take(b), take(c)),
        ("ptr.move", _, [a, b, c]) => Rt::ternary(Ternary::PtrMove, take(a), take(b), take(c)),
        ("ptr.to-integer", _, [ptr]) => Rt::unary(Unary::PtrToI, take(ptr)),
        ("reinterpret", [_, to], [a]) => Rt::unary(Unary::Reinterpret(take(to)), take(a)),
        ("array.construct", _, [a, b]) => Rt::binary(Binary::ArrayConstruct, take(a), take(b)),
        ("array.ptr", _, [a]) => Rt::unary(Unary::ArrayPtr, take(a)),
        ("array.length", _, [a]) => Rt::unary(Unary::ArrayLength, take(a)),
        ("array.load", _, [a, b]) => Rt::binary(Binary::ArrayLoad, take(a), take(b)),
        ("array.store", _, [a, b, c]) => {
            Rt::ternary(Ternary::ArrayStore, take(a), take(b), take(c))
        }
        ("array.alloc", [ty], [a]) => Rt::alloc_array(Location::Heap, take(ty), take(a)),
        ("array.stackalloc", [ty], [a]) => {
            Rt::alloc_array(Location::StackDynamic, take(ty), take(a))
        }
        ("integer.to-ptr", [ty], [a]) => Rt::unary(Unary::IToPtr(take(ty)), take(a)),
        ("syntax", [_], [a]) => Rt::construct_syntax(symbol.loc, take(a)),
        ("panic", _, [a]) => Rt::unary(Unary::Panic, take(a)),
        ("call.main", _, [a]) => Rt::call(RtCallee::MainIndirect(take(a)), Vec::new()),
        ("call.macro", _, [a, s]) => Rt::call(
            RtCallee::MacroIndirect(take(a), ret_ty.clone()),
            vec![take(s)],
        ),
        ("call.c", _, [_, ..]) => {
            let addr = args.remove(0);
            let args = std::mem::take(&mut args);
            Rt::call(RtCallee::CIndirect(addr, ret_ty.clone()), args)
        }
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
        ("non-null", [_], [a]) => RtPat::non_null(take(a)),
        ("null", [ty], []) => RtPat::Null(take(ty)),
        ("syntax", [_], [a]) => RtPat::syntax(take(a)),
        (name, _, _) => panic!(
            "Unsupported builtin-pattern {} (ct_args={}, args={})",
            name,
            ct_args.len(),
            args.len(),
        ),
    }
}
