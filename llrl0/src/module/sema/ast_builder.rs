use super::{Error, External, Module, Phase, Result, Scope};
use crate::ast::*;
use crate::sexp::{Match, Sexp, SexpRep, Ss};
use crate::source_loc::SourceLocation;
use crate::syntax;
use either::*;
use std::borrow::Cow;

pub fn run(module: &mut Module, code: &Ss, external: &impl External) -> Result<()> {
    let mut ctx = ContextImpl { module, external };

    for s in code.ss.iter() {
        define(s, &mut ctx)?;
    }

    debug_assert!(ctx.module.ast_root.verify_relations().is_ok());
    Ok(())
}

fn define<'a, E: External>(s: &Sexp, ctx: &mut ContextImpl<'a, E>) -> Result<()> {
    let s = ctx.expand_macro(s)?;
    let decl = ctx.matches::<syntax::Decl>(&s)?;
    match decl {
        syntax::Decl::NoImplicitStd => {}
        syntax::Decl::Import(_) => {}
        syntax::Decl::Export(_) => {}
        syntax::Decl::Function(function) => {
            let function = ctx.build(function)?;
            ctx.module.define_function(function)?;
        }
        syntax::Decl::CFunction(c_function) => {
            let c_function = ctx.build(c_function)?;
            ctx.module.define_c_function(c_function)?;
        }
        syntax::Decl::BuiltinOp(builtin_op) => {
            let builtin_op = ctx.build(builtin_op)?;
            ctx.module.define_builtin_op(builtin_op)?;
        }
        syntax::Decl::Macro(macro_) => {
            let macro_ = ctx.build(macro_)?;
            ctx.module.define_macro(macro_)?;
        }
        syntax::Decl::Data(data) => {
            let (type_con, value_cons) = ctx.build(data)?;
            ctx.module.define_data_type_con(type_con)?;
            for value_con in value_cons {
                ctx.module.define_data_value_con(value_con)?;
            }
        }
        syntax::Decl::BuiltinType(builtin_type) => {
            let (type_con, value_cons) = ctx.build(builtin_type)?;
            ctx.module.define_builtin_type_con(type_con)?;
            for value_con in value_cons {
                ctx.module.define_builtin_value_con(value_con)?;
            }
        }
        syntax::Decl::Class(class) => {
            let (class_con, methods) = ctx.build(class)?;
            ctx.module.define_class_con(class_con)?;
            for method in methods {
                ctx.module.define_class_method(method)?;
            }
        }
        syntax::Decl::Instance(instance) => {
            let (instance_con, methods) = ctx.build(instance)?;
            ctx.module.define_instance_con(instance_con)?;
            for method in methods {
                ctx.module.add_instance_method(method)?;
            }
        }
        syntax::Decl::Begin(decls) => {
            for decl in decls {
                define(decl, ctx)?;
            }
        }
        syntax::Decl::TopLevelExpr(s) => {
            let expr = ctx.build(s)?;
            ctx.module.add_init_expr(InitExpr::Eval(expr));
        }
    }

    Ok(())
}

struct ContextImpl<'a, E> {
    module: &'a mut Module,
    external: &'a E,
}

impl<'a, E: External> Context for ContextImpl<'a, E> {
    fn next_id<T: ?Sized>(&mut self, loc: SourceLocation, name: impl Into<String>) -> NodeId<T>
    where
        NodeId<T>: Into<Construct>,
        Construct: TryInto<NodeId<T>>,
    {
        let name = name.into();
        let id = if self.module.ast_root.id.module().is_builtin() {
            builtin::RESERVED_CONSTRUCTS
                .get(&name)
                .copied()
                .and_then(|id| id.try_into().ok())
        } else {
            None
        };
        let id = id.unwrap_or_else(|| self.module.ast_id_generator.generate());
        self.module.symbol_map.set(id, loc, name);
        id
    }

    fn reinterpret_id<T: ?Sized, S: ?Sized>(&mut self, id: NodeId<T>) -> NodeId<S>
    where
        NodeId<T>: Into<Construct>,
        NodeId<S>: Into<Construct>,
        Construct: TryInto<NodeId<T>> + TryInto<NodeId<S>>,
    {
        let new_id = id.reinterpret_unchecked::<S>();
        let symbol = self.module.symbol_map.get(id).unwrap().clone();
        self.module.symbol_map.set(new_id, symbol.loc, symbol.name);
        new_id
    }

    fn expand_macro<'c>(&mut self, s: impl Into<Cow<'c, Sexp>>) -> Result<Cow<'c, Sexp>> {
        let s = s.into();
        if let Ok(apply) = Sexp::matches::<syntax::MacroApply>(&s) {
            if let Some(Construct::Macro(id)) = match apply.callee {
                syntax::Use::Name(name) => self.module.top_level.get(name.sym).map(|b| b.construct),
                syntax::Use::Native(ref native) => native.rep.downcast_ref::<Construct>().copied(),
            } {
                if id.module() != self.module.id() {
                    self.module.report.leave_phase(Phase::BuildAst);
                    let s = self
                        .external
                        .execute_macro(id, s.as_ref())
                        .map_err(|e| Error::MacroExpansionFailed(s.loc, e))?;
                    self.module.report.enter_phase(Phase::BuildAst);
                    return self.expand_macro(s);
                } else {
                    Err(Error::CannotUseMacroDefinedInTheSameModule(
                        apply.callee.loc(),
                    ))?;
                }
            }
        }
        Ok(s)
    }
}

trait Context: Sized {
    fn next_id<T>(&mut self, loc: SourceLocation, name: impl Into<String>) -> NodeId<T>
    where
        NodeId<T>: Into<Construct>,
        Construct: TryInto<NodeId<T>>;

    fn reinterpret_id<T: ?Sized, S: ?Sized>(&mut self, id: NodeId<T>) -> NodeId<S>
    where
        NodeId<T>: Into<Construct>,
        NodeId<S>: Into<Construct>,
        Construct: TryInto<NodeId<T>> + TryInto<NodeId<S>>;

    fn expand_macro<'a>(&mut self, s: impl Into<Cow<'a, Sexp>>) -> Result<Cow<'a, Sexp>>;

    fn new_expr(&mut self, loc: SourceLocation, rep: ExprRep) -> Expr {
        let id = self.next_id(loc, "");
        Expr::new(id, rep)
    }

    fn new_pattern(&mut self, loc: SourceLocation, rep: PatternRep) -> Pattern {
        let id = self.next_id(loc, "");
        Pattern::new(id, rep)
    }

    fn matches<'a, T: Match<'a>>(&self, s: &'a Sexp) -> Result<T::Result> {
        Ok(s.matches::<T>()?)
    }

    fn build<T: Build<S>, S>(&mut self, src: S) -> Result<T> {
        T::build(self, src)
    }
}

trait Build<S>: Sized {
    fn build(ctx: &mut impl Context, src: S) -> Result<Self>;
}

impl<T: Build<S>, S> Build<Option<S>> for Option<T> {
    fn build(ctx: &mut impl Context, src: Option<S>) -> Result<Self> {
        src.map(|src| T::build(ctx, src)).transpose()
    }
}

impl<T: Build<IntoIter::Item>, IntoIter: IntoIterator> Build<IntoIter> for Vec<T> {
    fn build(ctx: &mut impl Context, src: IntoIter) -> Result<Self> {
        src.into_iter().map(|src| T::build(ctx, src)).collect()
    }
}

impl Build<syntax::Function<'_>> for Function {
    fn build(ctx: &mut impl Context, src: syntax::Function) -> Result<Self> {
        let params = ctx.build(src.params)?;
        let scheme = ctx.build(src.scheme)?;
        let body = ctx.build((src.loc, src.body))?;
        Ok(Function {
            id: ctx.next_id(src.name.loc, src.name.sym),
            transparent: src.transparent,
            params,
            ann: scheme,
            body,
        })
    }
}

impl Build<syntax::CFunction<'_>> for CFunction {
    fn build(ctx: &mut impl Context, src: syntax::CFunction) -> Result<Self> {
        let scheme: Annotation<Scheme> = ctx.build(src.scheme)?;
        let ty = if scheme.body.is_monomorphic() {
            Annotation::new(ctx.reinterpret_id(scheme.id), scheme.body.body)
        } else {
            Err(Error::CannotGeneralize(src.loc))?
        };

        Ok(CFunction {
            id: ctx.next_id(src.name.loc, src.name.sym),
            ann: ty,
            c_name: src.c_name.to_string(),
        })
    }
}

impl Build<syntax::BuiltinOp<'_>> for BuiltinOp {
    fn build(ctx: &mut impl Context, src: syntax::BuiltinOp) -> Result<Self> {
        let scheme = ctx.build(src.scheme)?;
        Ok(BuiltinOp {
            id: ctx.next_id(src.name.loc, src.name.sym),
            ann: scheme,
            builtin_name: src.builtin_name.to_string(),
        })
    }
}

impl Build<syntax::Macro<'_>> for Macro {
    fn build(ctx: &mut impl Context, src: syntax::Macro) -> Result<Self> {
        let param = ctx.build(src.param)?;
        let body = ctx.build((src.loc, src.body))?;
        Ok(Macro {
            id: ctx.next_id(src.name.loc, src.name.sym),
            param,
            body,
        })
    }
}

impl Build<syntax::Data<'_>> for (DataTypeCon, Vec<DataValueCon>) {
    fn build(ctx: &mut impl Context, src: syntax::Data) -> Result<Self> {
        let mut type_con = {
            let ty_params = ctx.build(src.ty_params.unwrap_or_default())?;
            let repr = match src.repr {
                syntax::DataRepr::Default => DataRepr::Default,
                syntax::DataRepr::Value => DataRepr::Value,
                syntax::DataRepr::C => DataRepr::C,
            };
            DataTypeCon {
                id: ctx.next_id(src.name.loc, src.name.sym),
                repr,
                ty_params,
                value_cons: Vec::new(),
            }
        };

        let value_cons = src
            .value_cons
            .iter()
            .map(|src| {
                let src = ctx.matches::<syntax::DataValueConstructor>(src)?;
                let fields = ctx.build(src.fields)?;
                let value_con = DataValueCon {
                    id: ctx.next_id(src.name.loc, src.name.sym),
                    fields,
                    type_con: type_con.id,
                };
                type_con.value_cons.push(value_con.id);
                Ok(value_con)
            })
            .collect::<Result<Vec<_>>>()?;

        Ok((type_con, value_cons))
    }
}

impl Build<syntax::BuiltinType<'_>> for (BuiltinTypeCon, Vec<BuiltinValueCon>) {
    fn build(ctx: &mut impl Context, src: syntax::BuiltinType) -> Result<Self> {
        let mut type_con = {
            let ty_params = ctx.build(src.ty_params.unwrap_or_default())?;
            BuiltinTypeCon {
                id: ctx.next_id(src.name.loc, src.name.sym),
                ty_params,
                builtin_name: src.builtin_name.to_string(),
                value_cons: Vec::new(),
            }
        };

        let value_cons = src
            .value_cons
            .iter()
            .map(|src| {
                let src = ctx.matches::<syntax::BuiltinValueConstructor>(src)?;
                let fields = ctx.build(src.fields)?;
                let value_con = BuiltinValueCon {
                    id: ctx.next_id(src.name.loc, src.name.sym),
                    builtin_name: src.builtin_name.to_string(),
                    fields,
                    type_con: type_con.id,
                };
                type_con.value_cons.push(value_con.id);
                Ok(value_con)
            })
            .collect::<Result<Vec<_>>>()?;

        Ok((type_con, value_cons))
    }
}

impl Build<&'_ Sexp> for ValueConField {
    fn build(ctx: &mut impl Context, src: &'_ Sexp) -> Result<Self> {
        let ty = ctx.build(src)?;
        Ok(ValueConField {
            id: ctx.next_id(src.loc, ""),
            ty,
        })
    }
}

impl Build<syntax::Class<'_>> for (ClassCon, Vec<ClassMethod>) {
    fn build(ctx: &mut impl Context, src: syntax::Class<'_>) -> Result<Self> {
        let mut class_con = {
            let ty_params = ctx.build(src.ty_params.unwrap_or_default())?;
            let superclasses = ctx.build(src.superclasses)?;
            let constraint_id = ctx.next_id(src.name.loc, src.name.sym);
            ClassCon {
                id: ctx.next_id(src.name.loc, src.name.sym),
                constraint_id,
                ty_params,
                superclasses,
                methods: Vec::new(),
                is_sealed: src.is_sealed,
            }
        };

        let class_methods = src
            .methods
            .iter()
            .map(|src| {
                let src = ctx.matches::<syntax::Function>(src)?;
                let scheme = match src.scheme {
                    Some(ann) => ctx.build(ann)?,
                    None => Err(Error::ClassMethodTypeSchemeUnspecified(src.loc))?,
                };
                let params = ctx.build(src.params)?;
                let default_body = if src.body.is_empty() {
                    None
                } else {
                    Some(ctx.build((src.loc, src.body))?)
                };
                let class_method = ClassMethod {
                    id: ctx.next_id(src.name.loc, src.name.sym),
                    transparent: src.transparent,
                    ann: scheme,
                    params,
                    default_body,
                    class_con: class_con.id,
                };
                class_con.methods.push(class_method.id);
                Ok(class_method)
            })
            .collect::<Result<Vec<_>>>()?;

        Ok((class_con, class_methods))
    }
}

impl Build<syntax::Instance<'_>> for (InstanceCon, Vec<InstanceMethod>) {
    fn build(ctx: &mut impl Context, src: syntax::Instance<'_>) -> Result<Self> {
        let mut instance_con = {
            let ty_params = ctx.build(src.ty_params)?;
            let s_params = ctx.build(src.s_params)?;
            let target = ctx.build(src.target)?;
            InstanceCon {
                id: ctx.next_id(src.name.loc, src.name.sym),
                ty_params,
                s_params,
                target,
                methods: Vec::new(),
            }
        };

        let instance_methods = src
            .method_impls
            .iter()
            .map(|src| {
                let src = ctx.matches::<syntax::Function>(src)?;
                let params = ctx.build(src.params)?;
                let scheme = ctx.build(src.scheme)?;
                let body = ctx.build((src.loc, src.body))?;
                let class_method = Use::Unresolved(ctx.next_id(src.name.loc, src.name.sym));
                let instance_method = InstanceMethod {
                    id: ctx.next_id(src.name.loc, src.name.sym),
                    transparent: src.transparent,
                    params,
                    ann: scheme,
                    body,
                    class_method,
                    instance_con: instance_con.id,
                };
                instance_con.methods.push(instance_method.id);
                Ok(instance_method)
            })
            .collect::<Result<Vec<_>>>()?;

        Ok((instance_con, instance_methods))
    }
}

impl Build<syntax::Parameter<'_>> for Parameter {
    fn build(ctx: &mut impl Context, src: syntax::Parameter) -> Result<Self> {
        Ok(Parameter {
            id: ctx.next_id(src.name.loc, src.name.sym),
        })
    }
}

impl Build<(SourceLocation, &'_ [Sexp])> for Expr {
    fn build(ctx: &mut impl Context, (loc, ss): (SourceLocation, &'_ [Sexp])) -> Result<Self> {
        let rep = ctx.build(ss)?;
        Ok(ctx.new_expr(loc, rep))
    }
}

impl Build<&'_ [Sexp]> for ExprRep {
    fn build(ctx: &mut impl Context, src: &'_ [Sexp]) -> Result<Self> {
        match src {
            [] => Ok(ExprRep::unit()),
            [s] => {
                let expr: Expr = ctx.build(s)?;
                Ok(expr.rep)
            }
            [ss @ .., s] => {
                let ss = ctx.build(ss)?;
                let s = ctx.build(s)?;
                Ok(ExprRep::seq(ss, s))
            }
        }
    }
}

impl Build<&'_ Sexp> for Expr {
    fn build(ctx: &mut impl Context, src: &'_ Sexp) -> Result<Self> {
        let loc = src.loc;
        let src = ctx.expand_macro(src)?;
        let src = ctx.matches::<syntax::Expr>(&src)?;
        let rep = ctx.build(src)?;
        Ok(ctx.new_expr(loc, rep))
    }
}

impl Build<syntax::Expr<'_>> for ExprRep {
    fn build(ctx: &mut impl Context, src: syntax::Expr<'_>) -> Result<Self> {
        match src {
            syntax::Expr::Begin(ss) => ctx.build(ss.body),
            syntax::Expr::Let(let_) => {
                let vars = ctx.build(let_.defs)?;
                let body = ctx.build((let_.loc, let_.body))?;
                Ok(ExprRep::let_(vars, body))
            }
            syntax::Expr::If(if_) => {
                let cond = ctx.build(if_.cond)?;
                let then = ctx.build(if_.then)?;
                let else_ = ctx.build(if_.else_)?;
                Ok(ExprRep::if_(cond, then, else_))
            }
            syntax::Expr::While(while_) => {
                let cond = ctx.build(while_.cond)?;
                let body = ctx.build((while_.loc, while_.body))?;
                Ok(ExprRep::while_(cond, body))
            }
            syntax::Expr::Match(match_) => {
                let target = ctx.build(match_.target)?;
                let clauses = ctx.build(match_.clauses)?;
                Ok(ExprRep::match_(target, clauses))
            }
            syntax::Expr::Return(expr) => {
                let expr = ctx.build(expr)?;
                Ok(ExprRep::Return(Box::new(expr)))
            }
            syntax::Expr::Tuple(tuple) => {
                if tuple.exprs.is_empty() {
                    Ok(ExprRep::unit())
                } else {
                    let callee = ctx.new_expr(tuple.loc, ExprRep::tuple_con(tuple.exprs.len()));
                    let exprs = ctx.build(tuple.exprs)?;
                    Ok(ExprRep::app(callee, exprs))
                }
            }
            syntax::Expr::Unit => Ok(ExprRep::unit()),
            syntax::Expr::Use(syntax::Use::Name(name)) => {
                let use_ = ctx.next_id(name.loc, name.sym);
                Ok(ExprRep::use_(use_))
            }
            syntax::Expr::Use(syntax::Use::Native(native)) => {
                if let Some(&construct) = native.rep.downcast_ref::<Construct>() {
                    if let Ok(value) = TryInto::<Value>::try_into(construct) {
                        if value.is_static() {
                            Ok(ExprRep::Use(Use::Resolved(value, None)))
                        } else {
                            Err(Error::unresolved(native.loc, "value", "<captured use>"))
                        }
                    } else if let Ok(value_con) = construct.try_into() {
                        Ok(ExprRep::Con(value_con))
                    } else {
                        Err(Error::unresolved(native.loc, "value", "<captured use>"))
                    }
                } else {
                    panic!("Unknown native {}", native.rep)
                }
            }
            syntax::Expr::App(apply) => {
                let callee = ctx.build(apply.callee)?;
                let args = ctx.build(apply.args)?;
                Ok(ExprRep::app(callee, args))
            }
            syntax::Expr::Quote(expr) => {
                Ok(ExprRep::Const(Const::SyntaxSexp(Box::new(expr.clone()))))
            }
            syntax::Expr::Capture(name) => {
                let use_ = ctx.next_id(name.loc, name.sym);
                Ok(ExprRep::capture(use_))
            }
            syntax::Expr::Annotate(annotate) => {
                let body = ctx.build(annotate.expr)?;
                let scheme: Annotation<Scheme> = ctx.build(annotate.scheme)?;
                if !scheme.body.is_monomorphic() {
                    Err(Error::CannotGeneralize(annotate.loc))?;
                }
                let ty = Annotation::new(ctx.reinterpret_id(scheme.id), scheme.body.body);
                Ok(ExprRep::annotate(body, ty))
            }
            syntax::Expr::Literal(s) => match s.rep {
                SexpRep::Bool(v) => Ok(ExprRep::bool(v)),
                _ => Ok(ExprRep::Const(ctx.build(s)?)),
            },
        }
    }
}

impl Build<&'_ Sexp> for Either<LocalVar, LocalFun> {
    fn build(ctx: &mut impl Context, src: &'_ Sexp) -> Result<Self> {
        let src = ctx.matches::<syntax::LocalDef>(src)?;
        ctx.build(src)
    }
}

impl Build<syntax::LocalDef<'_>> for Either<LocalVar, LocalFun> {
    fn build(ctx: &mut impl Context, src: syntax::LocalDef<'_>) -> Result<Self> {
        let params = ctx.build(src.params)?;
        let scheme = ctx.build(src.scheme)?;
        let body = ctx.build((src.loc, src.body))?;
        match params {
            Some(params) => Ok(Right(LocalFun {
                id: ctx.next_id(src.name.loc, src.name.sym),
                params,
                ann: scheme,
                body,
            })),
            None => {
                let ty = match scheme {
                    Some(scheme) if scheme.body.is_monomorphic() => Some(Annotation::new(
                        ctx.reinterpret_id(scheme.id),
                        scheme.body.body,
                    )),
                    Some(_) => Err(Error::CannotGeneralize(src.loc))?,
                    None => None,
                };

                Ok(Left(LocalVar {
                    id: ctx.next_id(src.name.loc, src.name.sym),
                    ann: ty,
                    init: body,
                }))
            }
        }
    }
}

impl Build<&'_ Sexp> for Const {
    fn build(_: &mut impl Context, s: &'_ Sexp) -> Result<Self> {
        match s.rep {
            SexpRep::Integer(signed, v) => Ok(Const::Integer(signed, v)),
            SexpRep::FPNumber(v) => Ok(Const::FPNumber(v)),
            SexpRep::String(ref s) => Ok(Const::String(s.to_string())),
            SexpRep::Char(c) => Ok(Const::Char(c)),
            _ => panic!("Cannot treat this expression as a constant: {}", s),
        }
    }
}

impl Build<&'_ Sexp> for Pattern {
    fn build(ctx: &mut impl Context, src: &'_ Sexp) -> Result<Self> {
        let loc = src.loc;
        let src = ctx.expand_macro(src)?;
        let src = ctx.matches::<syntax::Pattern>(&src)?;
        let rep = ctx.build(src)?;
        Ok(ctx.new_pattern(loc, rep))
    }
}

impl Build<syntax::Pattern<'_>> for PatternRep {
    fn build(ctx: &mut impl Context, src: syntax::Pattern<'_>) -> Result<Self> {
        match src {
            syntax::Pattern::Const(s) => match s.rep {
                SexpRep::Bool(v) => Ok(PatternRep::decon(ValueCon::bool(v), None)),
                _ => Ok(PatternRep::Const(ctx.build(s)?)),
            },
            syntax::Pattern::Var(var) => {
                let id = ctx.next_id(var.name.loc, var.name.sym);
                let as_pat = ctx.build(var.as_pat)?;
                Ok(PatternRep::var(id, as_pat))
            }
            syntax::Pattern::Tuple(tuple) => {
                let patterns = ctx.build(tuple.patterns)?;
                Ok(PatternRep::tuple(patterns))
            }
            syntax::Pattern::Unit => Ok(PatternRep::tuple(Vec::new())),
            syntax::Pattern::Decon(decon) => {
                let use_ = match decon.con {
                    syntax::Use::Name(name) => Use::Unresolved(ctx.next_id(name.loc, name.sym)),
                    syntax::Use::Native(native) => {
                        if let Some(&construct) = native.rep.downcast_ref::<Construct>() {
                            if let Ok(value_con) = construct.try_into() {
                                Use::Resolved(value_con, None)
                            } else {
                                Err(Error::unresolved(
                                    native.loc,
                                    "constructor",
                                    "<captured use>",
                                ))?
                            }
                        } else {
                            panic!("Unknown native {}", native.rep)
                        }
                    }
                };
                let fields = ctx.build(decon.fields)?;
                Ok(PatternRep::decon(use_, fields))
            }
            syntax::Pattern::Wildcard => Ok(PatternRep::Wildcard),
        }
    }
}

impl Build<&'_ Sexp> for (Pattern, Expr) {
    fn build(ctx: &mut impl Context, src: &'_ Sexp) -> Result<Self> {
        let src = ctx.expand_macro(src)?;
        let src = ctx.matches::<syntax::MatchClause>(&src)?;
        let pat = ctx.build(src.pat)?;
        let body = ctx.build((src.loc, src.body))?;
        Ok((pat, body))
    }
}

impl Build<&'_ Sexp> for Type {
    fn build(ctx: &mut impl Context, src: &'_ Sexp) -> Result<Self> {
        let src = ctx.expand_macro(src)?;
        let src = ctx.matches::<syntax::Type>(&src)?;
        ctx.build(src)
    }
}

impl Build<syntax::Type<'_>> for Type {
    fn build(ctx: &mut impl Context, src: syntax::Type<'_>) -> Result<Self> {
        match src {
            syntax::Type::Fun(function) => {
                let args = ctx.build(function.args)?;
                let ret = ctx.build(function.ret)?;
                Ok(build_type!(Ast, (-> ...{args} {ret})))
            }
            syntax::Type::Tuple(tuple) => {
                let tys = ctx.build(tuple.tys)?;
                Ok(build_type!(Ast, (: ...{tys})))
            }
            syntax::Type::Unit => Ok(build_type!(Ast, unit)),
            syntax::Type::Use(syntax::Use::Name(name)) => {
                Ok(build_type!(Ast, (use {ctx.next_id(name.loc, name.sym)})))
            }
            syntax::Type::Use(syntax::Use::Native(native)) => {
                if let Some(&construct) = native.rep.downcast_ref::<Construct>() {
                    if let Ok(type_con) = construct.try_into() {
                        Ok(build_type!(Ast, (con { type_con })))
                    } else {
                        Err(Error::unresolved(native.loc, "type", "<captured use>"))
                    }
                } else {
                    panic!("Unknown native {}", native.rep)
                }
            }
            syntax::Type::App(apply) => {
                let callee = ctx.build(apply.callee)?;
                let args = ctx.build(apply.args)?;
                Ok(build_type!(Ast, ({callee} ...{args})))
            }
        }
    }
}

impl Build<syntax::Constraint<'_>> for Constraint {
    fn build(ctx: &mut impl Context, src: syntax::Constraint<'_>) -> Result<Self> {
        let class = match src.target {
            syntax::Use::Name(name) => Use::Unresolved(ctx.next_id(name.loc, name.sym)),
            syntax::Use::Native(native) => {
                if let Some(&construct) = native.rep.downcast_ref::<Construct>() {
                    if let Ok(class_con) = construct.try_into() {
                        Use::Resolved(class_con, None)
                    } else {
                        Err(Error::unresolved(native.loc, "class", "<captured use>"))?
                    }
                } else {
                    panic!("Unknown native {}", native.rep)
                }
            }
        };
        let class_args = ctx.build(src.args.unwrap_or_default())?;
        Ok(Constraint::class(
            ctx.next_id(src.loc, ""),
            class,
            class_args,
        ))
    }
}

impl Build<syntax::TypeParameter<'_>> for TypeParameter {
    fn build(ctx: &mut impl Context, src: syntax::TypeParameter<'_>) -> Result<Self> {
        let kind = ctx.build(src.kind_ann)?;
        Ok(TypeParameter {
            id: ctx.next_id(src.name.loc, src.name.sym),
            ann: kind,
        })
    }
}

impl Build<syntax::Scheme<'_>> for Annotation<Scheme> {
    fn build(ctx: &mut impl Context, src: syntax::Scheme<'_>) -> Result<Self> {
        let ty_params = ctx.build(src.ty_params)?;
        let s_params = ctx.build(src.s_params)?;
        let body = ctx.build(src.body)?;
        Ok(Annotation::new(
            ctx.next_id(src.loc, ""),
            Scheme::new(ty_params, s_params, body),
        ))
    }
}

impl Build<&'_ Sexp> for Annotation<Kind> {
    fn build(ctx: &mut impl Context, src: &'_ Sexp) -> Result<Self> {
        let kind = ctx.build(src)?;
        Ok(Annotation::new(ctx.next_id(src.loc, ""), kind))
    }
}

impl Build<&'_ Sexp> for Kind {
    fn build(ctx: &mut impl Context, src: &'_ Sexp) -> Result<Self> {
        let src = ctx.expand_macro(src)?;
        let src = ctx.matches::<syntax::Kind>(&src)?;
        ctx.build(src)
    }
}

impl Build<syntax::Kind<'_>> for Kind {
    fn build(ctx: &mut impl Context, src: syntax::Kind<'_>) -> Result<Self> {
        match src {
            syntax::Kind::Fun(function) => {
                let args = ctx.build(function.args)?;
                let ret = ctx.build(function.ret)?;
                Ok(build_kind!(Ast, (-> ...{args} {ret})))
            }
            syntax::Kind::Type => Ok(build_kind!(Ast, type)),
            syntax::Kind::Constraint => Ok(build_kind!(Ast, constraint)),
            syntax::Kind::Satisfaction => Ok(build_kind!(Ast, satisfaction)),
            syntax::Kind::Value => Ok(build_kind!(Ast, value)),
            syntax::Kind::Macro => Ok(build_kind!(Ast, macro)),
        }
    }
}
