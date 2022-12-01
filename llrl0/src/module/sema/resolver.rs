use super::{
    Error, External, LocalScope, LocatedConstruct, Module, ModuleSet, Result, Scope, Symbol,
    SymbolMap,
};
use crate::ast::*;
use either::*;
use std::collections::HashMap;

pub fn run(module: &mut Module, external: &impl External) -> Result<()> {
    let mut ctx = ContextImpl {
        symbol_map: &module.symbol_map,
        scope: &mut module.top_level,
    };
    ctx.resolve(&mut module.ast_root)?;

    for con in module.ast_root.instance_cons.values() {
        module.available_instances.add(con);
    }

    for inst in module.ast_root.instance_cons.values() {
        let inst_symbol = module.symbol_map.get(inst.id).unwrap();
        let module_set = (&*module, external);
        let (class, mut method_table) = {
            let ConstraintRep::Class(ref class, _) = inst.target.rep;
            let class = *class.get_resolved();
            let module = module_set.module_of(class.module());
            let class = module.ast_root().get(class).unwrap();
            let method_table = class
                .methods()
                .map(|method| {
                    let symbol = module.symbol_map.get(method.id).unwrap();
                    let info = (method.id, method.arity(), method.default_body.is_none());
                    (symbol.name.to_string(), info)
                })
                .collect::<HashMap<_, _>>();
            (class, method_table)
        };

        if class.con.is_sealed && class.con.id.module() != inst.id.module() {
            Err(Error::CannotDeclareSealedClassInstanceInAnotherModule(
                inst_symbol.loc,
            ))?;
        }

        for method in inst.methods.iter() {
            let method = module.ast_root.instance_methods.get_mut(method).unwrap();
            if let Use::Unresolved(id) = method.class_method {
                let symbol = module.symbol_map.get(id).unwrap();
                if let Some((id, arity, _)) = method_table.remove(&symbol.name) {
                    if arity != method.arity() {
                        Err(Error::ArityMismatch(arity, method.arity()).on(method.id))?;
                    }
                    method.class_method.set_resolved(id);
                } else {
                    Err(Error::unresolved(symbol.loc, "class-method", &symbol.name))?
                }
            }
        }

        for (name, (_, _, is_required)) in method_table.iter() {
            if *is_required {
                Err(Error::unresolved(inst_symbol.loc, "class-method", name))?;
            }
        }
    }

    debug_assert!(format!("{:?}", module.ast_root)
        .find("Unresolved")
        .is_none());

    Ok(())
}

struct ContextImpl<'a, S> {
    symbol_map: &'a SymbolMap,
    scope: S,
}

impl<'a, S: Scope> Context for ContextImpl<'a, S> {
    fn get<T>(&self, use_: NodeId<Use<T>>, kind: &'static str) -> Result<(Construct, &Symbol)>
    where
        Construct: From<NodeId<Use<T>>>,
    {
        let symbol = self.symbol_map.get(use_).unwrap();
        match self.scope.get(&symbol.name) {
            Some(c) => Ok((c.construct, symbol)),
            None => Err(Error::unresolved(symbol.loc, kind, &symbol.name)),
        }
    }

    type LocalContext<'s> = ContextImpl<'a, LocalScope<'s>>
    where
        Self: 's;

    fn on<'s>(&'s mut self, bind: &impl Bind) -> Result<Self::LocalContext<'s>> {
        let symbol_map = self.symbol_map;
        let mut scope = self.scope.enter_scope();
        bind.traverse_defs(&mut |def| {
            let symbol = symbol_map.get(def).unwrap();
            scope.define(&symbol.name, LocatedConstruct::new(symbol.loc, def))
        })?;
        Ok(ContextImpl { symbol_map, scope })
    }
}

trait Context: Sized {
    fn get<T>(&self, use_: NodeId<Use<T>>, kind: &'static str) -> Result<(Construct, &Symbol)>
    where
        Construct: From<NodeId<Use<T>>>;

    type LocalContext<'a>: Context
    where
        Self: 'a;

    fn on<'a>(&'a mut self, bind: &impl Bind) -> Result<Self::LocalContext<'a>>;

    fn resolve(&mut self, target: &mut impl Resolve) -> Result<()> {
        Resolve::resolve(self, target)
    }
}

trait Resolve {
    fn resolve(ctx: &mut impl Context, target: &mut Self) -> Result<()>;
}

impl<L: Resolve, R: Resolve> Resolve for Either<L, R> {
    fn resolve(ctx: &mut impl Context, target: &mut Self) -> Result<()> {
        match target {
            Left(l) => ctx.resolve(l),
            Right(r) => ctx.resolve(r),
        }
    }
}

impl<T: Resolve> Resolve for Option<T> {
    fn resolve(ctx: &mut impl Context, target: &mut Self) -> Result<()> {
        match target {
            Some(target) => ctx.resolve(target),
            None => Ok(()),
        }
    }
}

impl<T: Resolve> Resolve for Vec<T> {
    fn resolve(ctx: &mut impl Context, target: &mut Self) -> Result<()> {
        for target in target.iter_mut() {
            ctx.resolve(target)?;
        }
        Ok(())
    }
}

impl<T: Resolve> Resolve for Annotation<T> {
    fn resolve(ctx: &mut impl Context, target: &mut Self) -> Result<()> {
        ctx.resolve(&mut target.body)
    }
}

impl Resolve for Root {
    fn resolve(ctx: &mut impl Context, target: &mut Self) -> Result<()> {
        for function in target.functions.values_mut() {
            ctx.resolve(function)?;
        }

        for c_function in target.c_functions.values_mut() {
            ctx.resolve(c_function)?;
        }

        for builtin_op in target.builtin_ops.values_mut() {
            ctx.resolve(builtin_op)?;
        }

        for macro_ in target.macros.values_mut() {
            ctx.resolve(macro_)?;
        }

        for data_type_con in target.data_type_cons.values_mut() {
            ctx.resolve(data_type_con)?;
            let mut ctx = ctx.on(&data_type_con.ty_params)?;
            for id in data_type_con.value_cons.iter() {
                ctx.resolve(target.data_value_cons.get_mut(id).unwrap())?;
            }
        }

        for builtin_type_con in target.builtin_type_cons.values_mut() {
            ctx.resolve(builtin_type_con)?;
            let mut ctx = ctx.on(&builtin_type_con.ty_params)?;
            for id in builtin_type_con.value_cons.iter() {
                ctx.resolve(target.builtin_value_cons.get_mut(id).unwrap())?;
            }
        }

        for class_con in target.class_cons.values_mut() {
            ctx.resolve(class_con)?;
            let mut ctx = ctx.on(&class_con.ty_params)?;
            for id in class_con.methods.iter() {
                ctx.resolve(target.class_methods.get_mut(id).unwrap())?;
            }
        }

        for instance in target.instance_cons.values_mut() {
            ctx.resolve(instance)?;
            let mut ctx = ctx.on(&instance.ty_params)?;
            for id in instance.methods.iter() {
                ctx.resolve(target.instance_methods.get_mut(id).unwrap())?;
            }
        }

        for expr in target.init_expressions.iter_mut() {
            ctx.resolve(expr)?;
        }

        Ok(())
    }
}

impl Resolve for Function {
    fn resolve(ctx: &mut impl Context, target: &mut Self) -> Result<()> {
        ctx.resolve(&mut target.ann)?;
        let mut ctx = ctx.on(&target.ann)?;
        ctx.resolve(&mut target.params)?;
        let mut ctx = ctx.on(&target.params)?;
        ctx.resolve(&mut target.body)
    }
}

impl Resolve for CFunction {
    fn resolve(ctx: &mut impl Context, target: &mut Self) -> Result<()> {
        ctx.resolve(&mut target.ann)
    }
}

impl Resolve for BuiltinOp {
    fn resolve(ctx: &mut impl Context, target: &mut Self) -> Result<()> {
        ctx.resolve(&mut target.ann)
    }
}

impl Resolve for Macro {
    fn resolve(ctx: &mut impl Context, target: &mut Self) -> Result<()> {
        ctx.resolve(&mut target.param)?;
        let mut ctx = ctx.on(&target.param)?;
        ctx.resolve(&mut target.body)
    }
}

impl Resolve for DataTypeCon {
    fn resolve(ctx: &mut impl Context, target: &mut Self) -> Result<()> {
        ctx.resolve(&mut target.ty_params)
    }
}

impl Resolve for DataValueCon {
    fn resolve(ctx: &mut impl Context, target: &mut Self) -> Result<()> {
        ctx.resolve(&mut target.fields)
    }
}

impl Resolve for BuiltinTypeCon {
    fn resolve(ctx: &mut impl Context, target: &mut Self) -> Result<()> {
        ctx.resolve(&mut target.ty_params)
    }
}

impl Resolve for BuiltinValueCon {
    fn resolve(ctx: &mut impl Context, target: &mut Self) -> Result<()> {
        ctx.resolve(&mut target.fields)
    }
}

impl Resolve for ValueConField {
    fn resolve(ctx: &mut impl Context, target: &mut Self) -> Result<()> {
        ctx.resolve(&mut target.ty)
    }
}

impl Resolve for ClassCon {
    fn resolve(ctx: &mut impl Context, target: &mut Self) -> Result<()> {
        ctx.resolve(&mut target.ty_params)?;
        let mut ctx = ctx.on(&target.ty_params)?;
        ctx.resolve(&mut target.superclasses)
    }
}

impl Resolve for ClassMethod {
    fn resolve(ctx: &mut impl Context, target: &mut Self) -> Result<()> {
        ctx.resolve(&mut target.ann)?;
        let mut ctx = ctx.on(&target.ann)?;
        ctx.resolve(&mut target.params)?;
        let mut ctx = ctx.on(&target.params)?;
        ctx.resolve(&mut target.default_body)
    }
}

impl Resolve for InstanceCon {
    fn resolve(ctx: &mut impl Context, target: &mut Self) -> Result<()> {
        ctx.resolve(&mut target.ty_params)?;
        let mut ctx = ctx.on(&target.ty_params)?;
        ctx.resolve(&mut target.s_params)?;
        ctx.resolve(&mut target.target)
    }
}

impl Resolve for InstanceMethod {
    fn resolve(ctx: &mut impl Context, target: &mut Self) -> Result<()> {
        ctx.resolve(&mut target.ann)?;
        let mut ctx = ctx.on(&target.ann)?;
        ctx.resolve(&mut target.params)?;
        let mut ctx = ctx.on(&target.params)?;
        ctx.resolve(&mut target.body)
    }
}

impl Resolve for Parameter {
    fn resolve(_: &mut impl Context, _target: &mut Self) -> Result<()> {
        Ok(())
    }
}

impl Resolve for TypeParameter {
    fn resolve(ctx: &mut impl Context, target: &mut Self) -> Result<()> {
        ctx.resolve(&mut target.ann)
    }
}

impl Resolve for Scheme {
    fn resolve(ctx: &mut impl Context, target: &mut Self) -> Result<()> {
        ctx.resolve(&mut target.ty_params)?;
        let mut ctx = ctx.on(&target.ty_params)?;
        ctx.resolve(&mut target.s_params)?;
        ctx.resolve(&mut target.body)
    }
}

impl Resolve for Constraint {
    fn resolve(ctx: &mut impl Context, target: &mut Self) -> Result<()> {
        let ConstraintRep::Class(ref mut class, ref mut class_args) = target.rep;
        if let Use::Unresolved(id) = class {
            let (construct, unit) = ctx.get(*id, "class")?;

            if let Ok(id) = construct.try_into() {
                class.set_resolved(id);
            } else {
                Err(Error::unresolved(unit.loc, "class", &unit.name))?
            }
        }

        ctx.resolve(class_args)
    }
}

impl Resolve for Expr {
    fn resolve(ctx: &mut impl Context, target: &mut Self) -> Result<()> {
        match target.rep {
            ExprRep::Use(ref mut use_) => {
                if let Use::Unresolved(id) = *use_ {
                    let (construct, unit) = ctx.get(id, "value")?;

                    if let Ok(value) = construct.try_into() {
                        use_.set_resolved(value);
                    } else if let Ok(value_con) = construct.try_into() {
                        target.rep = ExprRep::Con(value_con);
                    } else {
                        Err(Error::unresolved(unit.loc, "value", &unit.name))?;
                    }
                }
                Ok(())
            }
            ExprRep::Con(_) => Ok(()),
            ExprRep::Const(_) => Ok(()),
            ExprRep::App(ref mut apply) => {
                ctx.resolve(&mut apply.callee)?;
                ctx.resolve(&mut apply.args)
            }
            ExprRep::Capture(ref mut use_) => {
                if let Use::Unresolved(id) = *use_ {
                    let (construct, _) = ctx.get(id, "construct")?;

                    use_.set_resolved(construct);
                }
                Ok(())
            }
            ExprRep::Annotate(ref mut annotate) => {
                ctx.resolve(&mut annotate.body)?;
                ctx.resolve(&mut annotate.ann)
            }
            ExprRep::Let(ref mut let_) => {
                {
                    let mut ctx = ctx.on(&let_.let_binding_context())?;
                    ctx.resolve(&mut let_.defs)?;
                }
                let mut ctx = ctx.on(&let_.defs)?;
                ctx.resolve(&mut let_.body)
            }
            ExprRep::Seq(ref mut seq) => {
                ctx.resolve(&mut seq.stmts)?;
                ctx.resolve(&mut seq.ret)
            }
            ExprRep::If(ref mut if_) => {
                ctx.resolve(&mut if_.cond)?;
                ctx.resolve(&mut if_.then)?;
                ctx.resolve(&mut if_.else_)
            }
            ExprRep::While(ref mut while_) => {
                ctx.resolve(&mut while_.cond)?;
                ctx.resolve(&mut while_.body)
            }
            ExprRep::Match(ref mut match_) => {
                ctx.resolve(&mut match_.target)?;
                for (pat, body) in match_.clauses.iter_mut() {
                    ctx.resolve(pat)?;
                    let mut ctx = ctx.on(pat)?;
                    ctx.resolve(body)?;
                }
                Ok(())
            }
            ExprRep::Return(ref mut expr) => {
                for expr in expr.iter_mut() {
                    ctx.resolve(expr)?;
                }
                Ok(())
            }
        }
    }
}

impl Resolve for LocalVar {
    fn resolve(ctx: &mut impl Context, target: &mut Self) -> Result<()> {
        ctx.resolve(&mut target.ann)?;
        ctx.resolve(&mut target.init)
    }
}

impl Resolve for LocalFun {
    fn resolve(ctx: &mut impl Context, target: &mut Self) -> Result<()> {
        ctx.resolve(&mut target.ann)?;
        let mut ctx = ctx.on(&target.ann)?;
        ctx.resolve(&mut target.params)?;
        let mut ctx = ctx.on(&target.params)?;
        ctx.resolve(&mut target.body)
    }
}

impl Resolve for InitExpr {
    fn resolve(ctx: &mut impl Context, target: &mut Self) -> Result<()> {
        match target {
            InitExpr::Eval(e) => ctx.resolve(e),
            InitExpr::EnsureInitialized(_) => Ok(()),
        }
    }
}

impl Resolve for Pattern {
    fn resolve(ctx: &mut impl Context, target: &mut Self) -> Result<()> {
        match target.rep {
            PatternRep::Var(ref mut var) => ctx.resolve(&mut var.as_pat),
            PatternRep::Wildcard => Ok(()),
            PatternRep::Decon(ref mut decon) => {
                if let Use::Unresolved(id) = decon.use_ {
                    let (construct, unit) = ctx.get(id, "constructor")?;

                    if let Ok(value_con) = construct.try_into() {
                        decon.use_.set_resolved(value_con);
                    } else {
                        Err(Error::unresolved(unit.loc, "constructor", &unit.name))?;
                    }
                }
                ctx.resolve(&mut decon.fields)
            }
            PatternRep::Const(_) => Ok(()),
        }
    }
}

impl Resolve for Type {
    fn resolve(ctx: &mut impl Context, target: &mut Self) -> Result<()> {
        match target {
            Type::Use(use_) => {
                if let Use::Unresolved(id) = *use_ {
                    let (construct, unit) = ctx.get(id, "type")?;

                    if let Ok(value_con) = construct.try_into() {
                        *target = Type::Con(value_con);
                    } else if let Ok(id) = construct.try_into() {
                        *target = Type::Gen(id);
                    } else {
                        Err(Error::unresolved(unit.loc, "type", &unit.name))?;
                    }
                }
                Ok(())
            }
            Type::Con(_) => Ok(()),
            Type::App(callee, args) => {
                ctx.resolve(&mut **callee)?;
                ctx.resolve(args)
            }
            Type::Gen(_) => Ok(()),
            Type::Error(e) => panic!("Found Type::Error at resolve: {}", e),
        }
    }
}

impl Resolve for Kind {
    fn resolve(ctx: &mut impl Context, target: &mut Self) -> Result<()> {
        match target {
            Kind::Use(use_) => {
                if let Use::Unresolved(id) = *use_ {
                    let (_, unit) = ctx.get(id, "kind")?;

                    // There are no resolvable Kind components
                    Err(Error::unresolved(unit.loc, "kind", &unit.name))?;
                }
                Ok(())
            }
            Kind::Type => Ok(()),
            Kind::Constraint => Ok(()),
            Kind::Satisfaction => Ok(()),
            Kind::Value => Ok(()),
            Kind::Macro => Ok(()),
            Kind::Fun(args, ret) => {
                ctx.resolve(args)?;
                ctx.resolve(&mut **ret)
            }
            Kind::Error(e) => panic!("Found Kind::Error at resolve: {}", e),
        }
    }
}

trait Bind {
    fn traverse_defs(&self, f: &mut impl FnMut(Construct) -> Result<()>) -> Result<()>;
}

impl<T: Bind> Bind for &'_ T {
    fn traverse_defs(&self, f: &mut impl FnMut(Construct) -> Result<()>) -> Result<()> {
        T::traverse_defs(*self, f)
    }
}

impl<T: Bind> Bind for Option<T> {
    fn traverse_defs(&self, f: &mut impl FnMut(Construct) -> Result<()>) -> Result<()> {
        match self {
            Some(a) => a.traverse_defs(f),
            _ => Ok(()),
        }
    }
}

impl<L: Bind, R: Bind> Bind for Either<L, R> {
    fn traverse_defs(&self, f: &mut impl FnMut(Construct) -> Result<()>) -> Result<()> {
        match self {
            Left(a) => a.traverse_defs(f),
            Right(b) => b.traverse_defs(f),
        }
    }
}

impl<T: Bind> Bind for Vec<T> {
    fn traverse_defs(&self, f: &mut impl FnMut(Construct) -> Result<()>) -> Result<()> {
        for a in self {
            a.traverse_defs(f)?;
        }
        Ok(())
    }
}

impl<T: Bind> Bind for Annotation<T> {
    fn traverse_defs(&self, f: &mut impl FnMut(Construct) -> Result<()>) -> Result<()> {
        self.body.traverse_defs(f)
    }
}

impl Bind for Parameter {
    fn traverse_defs(&self, f: &mut impl FnMut(Construct) -> Result<()>) -> Result<()> {
        f(self.id.into())
    }
}

impl Bind for TypeParameter {
    fn traverse_defs(&self, f: &mut impl FnMut(Construct) -> Result<()>) -> Result<()> {
        f(self.id.into())
    }
}

impl Bind for LocalVar {
    fn traverse_defs(&self, f: &mut impl FnMut(Construct) -> Result<()>) -> Result<()> {
        f(self.id.into())
    }
}

impl Bind for LocalFun {
    fn traverse_defs(&self, f: &mut impl FnMut(Construct) -> Result<()>) -> Result<()> {
        f(self.id.into())
    }
}

impl Bind for PatternVar {
    fn traverse_defs(&self, f: &mut impl FnMut(Construct) -> Result<()>) -> Result<()> {
        f(self.id.into())
    }
}

impl Bind for Scheme {
    fn traverse_defs(&self, f: &mut impl FnMut(Construct) -> Result<()>) -> Result<()> {
        self.ty_params.traverse_defs(f)
    }
}

impl<'a> Bind for ExprLetBindingContext<'a> {
    fn traverse_defs(&self, f: &mut impl FnMut(Construct) -> Result<()>) -> Result<()> {
        for a in self.defs() {
            a.traverse_defs(f)?;
        }
        Ok(())
    }
}

impl Bind for Pattern {
    fn traverse_defs(&self, f: &mut impl FnMut(Construct) -> Result<()>) -> Result<()> {
        self.dfs(|p| match p.rep {
            PatternRep::Var(ref var) => var.traverse_defs(f),
            _ => Ok(()),
        })
    }
}
