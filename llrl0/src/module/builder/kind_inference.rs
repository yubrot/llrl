use super::{Error, External, InferredKinds, Module, ModuleId, Result};
use crate::ast::{self, Dfs as _};
use crate::topological_sort;
use crate::unification::kind as u;
use std::collections::HashMap;

pub fn run(module: &mut Module, external: &impl External) -> Result<()> {
    let mut ctx = Context {
        mid: module.id(),
        u_ctx: u::Context::new(),
        u_kinds: HashMap::new(),
        external,
        inferred_kinds: &mut module.inferred_kinds,
    };

    ctx.infer(&module.ast_root)
}

#[derive(Debug)]
struct Context<'a, E> {
    mid: ModuleId,
    u_ctx: u::Context,
    u_kinds: HashMap<ast::Construct, u::Kind>,
    external: &'a E,
    inferred_kinds: &'a mut InferredKinds,
}

impl<'a, E: External> Context<'a, E> {
    fn unify<A: Copy, B: Copy>(&mut self, a: A, b: B) -> Result<()>
    where
        u::Context: u::Resolve<A> + u::Resolve<B>,
    {
        match self.u_ctx.unify(a, b) {
            Ok(()) => Ok(()),
            Err(e) => {
                let a = self.u_ctx.export(a, &mut ast::Ast);
                let b = self.u_ctx.export(b, &mut ast::Ast);
                Err(Error::CannotUnifyKind(a, b, e))
            }
        }
    }

    fn unify_on<A: Copy, B: Copy>(
        &mut self,
        construct: impl Into<ast::Construct>,
        a: A,
        b: B,
    ) -> Result<()>
    where
        u::Context: u::Resolve<A> + u::Resolve<B>,
    {
        self.unify(a, b).map_err(|e| e.on(construct))
    }

    fn kind_of(&mut self, construct: impl Into<ast::Construct>) -> u::Kind {
        let construct = construct.into();
        let current_mid = self.mid;
        let u_ctx = &mut self.u_ctx;
        let external = self.external;

        *self.u_kinds.entry(construct).or_insert_with(|| {
            if construct.module() == current_mid {
                // Inferring kind
                u_ctx.new_var()
            } else {
                // Import the external construct's kind
                let kind = external
                    .module(construct.module())
                    .inferred_kinds
                    .get(construct)
                    .unwrap_or_else(|| panic!("Uninferred kind: {}", construct));
                u_ctx.import(kind.as_ref())
            }
        })
    }

    fn fix_kind_of(
        &mut self,
        construct: impl Into<ast::Construct>,
        ensure_first_class: bool,
    ) -> Result<()> {
        let construct = construct.into();
        let kind = self.kind_of(construct);
        let kind = self.u_ctx.export(kind, &mut ast::Ast);
        assert!(!kind.contains_error(), "Undetermined kind: {:?}", kind);
        if ensure_first_class && !kind.is_first_class() {
            return Err(Error::UnsupportedKind(kind).on(construct));
        }
        self.inferred_kinds.set(construct, kind);
        Ok(())
    }

    fn setup_generic(
        &mut self,
        construct: impl Into<ast::Construct>,
        generic: &impl ast::Generic,
        ret: u::Kind,
    ) -> Result<u::Kind> {
        let construct = construct.into();
        let a = self.kind_of(construct);
        let b = if generic.is_monomorphic() {
            ret
        } else {
            let mut params = generic
                .generic_types()
                .iter()
                .map(|tp| {
                    let kind = self.kind_of(tp.id);
                    if let Some(ref kind_ann) = tp.ann {
                        let ann_kind = self.u_ctx.import(&kind_ann.body);
                        self.unify_on(kind_ann.id, kind, ann_kind)?;
                    }
                    Ok(kind)
                })
                .collect::<Result<Vec<_>>>()?;
            for _ in 0..generic.generic_constraints().len() {
                params.push(u::Kind::Satisfaction);
            }
            build_kind!(self.u_ctx, (-> ...{params} {ret}))
        };
        self.unify_on(construct, a, b)?;
        Ok(a)
    }

    fn fix_generic(
        &mut self,
        construct: impl Into<ast::Construct>,
        generic: &impl ast::Generic,
    ) -> Result<()> {
        for tp in generic.generic_types().iter() {
            let kind = self.kind_of(tp.id);
            self.u_ctx.default_vars(kind, u::Kind::Type).unwrap();
            self.fix_kind_of(tp.id, true)?;
        }
        self.fix_kind_of(construct, false)
    }
}

pub trait Infer<T: ?Sized> {
    type Result;

    fn infer(&mut self, target: &T) -> Result<Self::Result>;

    fn infer_on(
        &mut self,
        construct: impl Into<ast::Construct>,
        target: &T,
    ) -> Result<Self::Result> {
        self.infer(target).map_err(|e| e.on(construct))
    }
}

impl<'a, E: External> Infer<ast::Root> for Context<'a, E> {
    type Result = ();

    fn infer(&mut self, target: &ast::Root) -> Result<Self::Result> {
        for builtin_type in target.builtin_types() {
            self.infer(builtin_type.con)?;
        }

        for group in topological_sort::run(target.data_types().map(|ty| (ty.con.id, ty))) {
            self.infer(group.as_slice())?;
        }

        for builtin_type in target.builtin_types() {
            for builtin_value_con in builtin_type.value_cons() {
                self.infer(builtin_value_con)?;
            }
        }

        for group in topological_sort::run(target.classes().map(|cls| (cls.con.id, cls))) {
            self.infer(group.as_slice())?;
        }

        for function in target.functions.values() {
            self.infer(function)?;
        }

        for c_function in target.c_functions.values() {
            self.infer(c_function)?;
        }

        for builtin_op in target.builtin_ops.values() {
            self.infer(builtin_op)?;
        }

        for m in target.macros.values() {
            self.infer(m)?;
        }

        for instance in target.instances() {
            self.infer(&instance)?;
        }

        self.infer(&target.init_expressions)?;

        Ok(())
    }
}

impl<'a, E, T> Infer<Option<T>> for Context<'a, E>
where
    Self: Infer<T>,
{
    type Result = Option<<Self as Infer<T>>::Result>;

    fn infer(&mut self, target: &Option<T>) -> Result<Self::Result> {
        target.as_ref().map(|target| self.infer(target)).transpose()
    }
}

impl<'a, E, T> Infer<Vec<T>> for Context<'a, E>
where
    Self: Infer<T>,
{
    type Result = Vec<<Self as Infer<T>>::Result>;

    fn infer(&mut self, target: &Vec<T>) -> Result<Self::Result> {
        target
            .iter()
            .map(|target| self.infer(target))
            .collect::<Result<Vec<_>>>()
    }
}

impl<'a, E: External> Infer<[ast::DataType<'_>]> for Context<'a, E> {
    type Result = ();

    fn infer(&mut self, target: &[ast::DataType<'_>]) -> Result<Self::Result> {
        for ty in target.iter() {
            self.setup_generic(ty.con.id, ty.con, u::Kind::Type)?;
        }

        for field in target.iter().flat_map(|ty| ty.fields_on_every_value_cons()) {
            let kind = self.infer_on(field.id, &field.ty)?;
            self.unify_on(field.id, kind, u::Kind::Type)?;
        }

        for ty in target.iter() {
            self.fix_generic(ty.con.id, ty.con)?;
        }

        Ok(())
    }
}

impl<'a, E: External> Infer<ast::BuiltinTypeCon> for Context<'a, E> {
    type Result = ();

    fn infer(&mut self, target: &ast::BuiltinTypeCon) -> Result<Self::Result> {
        // If the builtin-type type parameter's kind is not *, it must always be explicit.
        self.setup_generic(target.id, target, u::Kind::Type)?;
        self.fix_generic(target.id, target)?;

        Ok(())
    }
}

impl<'a, E: External> Infer<ast::BuiltinValueCon> for Context<'a, E> {
    type Result = ();

    fn infer(&mut self, target: &ast::BuiltinValueCon) -> Result<Self::Result> {
        for field in target.fields.iter().flat_map(|fields| fields.iter()) {
            let kind = self.infer_on(field.id, &field.ty)?;
            self.unify_on(field.id, kind, u::Kind::Type)?;
        }

        Ok(())
    }
}

impl<'a, E: External> Infer<[ast::Class<'_>]> for Context<'a, E> {
    type Result = ();

    fn infer(&mut self, target: &[ast::Class<'_>]) -> Result<Self::Result> {
        for cls in target.iter() {
            self.setup_generic(cls.con.id, cls.con, u::Kind::Constraint)?;
            for method in cls.methods() {
                self.setup_generic(method.id, &method.ann.body, u::Kind::Value)?;
            }
        }

        for cls in target.iter() {
            self.infer(&cls.con.superclasses)?;
            for method in cls.methods() {
                self.infer(&method.ann)?;
            }
        }

        for cls in target.iter() {
            self.fix_generic(cls.con.id, cls.con)?;
            for method in cls.methods() {
                self.fix_generic(method.id, &method.ann.body)?;
            }
        }

        for method in target.iter().flat_map(|cls| cls.methods()) {
            self.infer(&method.default_body)?;
        }

        Ok(())
    }
}

impl<'a, E: External> Infer<ast::Instance<'_>> for Context<'a, E> {
    type Result = ();

    fn infer(&mut self, inst: &ast::Instance<'_>) -> Result<Self::Result> {
        self.setup_generic(inst.con.id, inst.con, u::Kind::Satisfaction)?;
        for method in inst.methods() {
            if let Some(ref scheme) = method.ann {
                self.setup_generic(method.id, &scheme.body, u::Kind::Value)?;
            }
        }

        self.infer(&inst.con.s_params)?;
        self.infer(&inst.con.target)?;

        for method in inst.methods() {
            if let Some(ref scheme) = method.ann {
                self.infer(scheme)?;
            }
        }

        self.fix_generic(inst.con.id, inst.con)?;
        for method in inst.methods() {
            if let Some(ref scheme) = method.ann {
                self.fix_generic(method.id, &scheme.body)?;
            }
        }

        for method in inst.methods() {
            self.infer(&method.body)?;
        }

        Ok(())
    }
}

impl<'a, E: External> Infer<ast::Function> for Context<'a, E> {
    type Result = ();

    fn infer(&mut self, target: &ast::Function) -> Result<Self::Result> {
        if let Some(ref scheme) = target.ann {
            self.setup_generic(target.id, &scheme.body, u::Kind::Value)?;
            self.infer(scheme)?;
            self.fix_generic(target.id, &scheme.body)?;
        }
        self.infer(&target.body)
    }
}

impl<'a, E: External> Infer<ast::CFunction> for Context<'a, E> {
    type Result = ();

    fn infer(&mut self, target: &ast::CFunction) -> Result<Self::Result> {
        self.infer(&target.ann)
    }
}

impl<'a, E: External> Infer<ast::BuiltinOp> for Context<'a, E> {
    type Result = ();

    fn infer(&mut self, target: &ast::BuiltinOp) -> Result<Self::Result> {
        self.setup_generic(target.id, &target.ann.body, u::Kind::Value)?;
        self.infer(&target.ann)?;
        self.fix_generic(target.id, &target.ann.body)
    }
}

impl<'a, E: External> Infer<ast::Macro> for Context<'a, E> {
    type Result = ();

    fn infer(&mut self, target: &ast::Macro) -> Result<Self::Result> {
        self.infer(&target.body)
    }
}

impl<'a, E: External> Infer<ast::LocalFun> for Context<'a, E> {
    type Result = ();

    fn infer(&mut self, target: &ast::LocalFun) -> Result<Self::Result> {
        if let Some(ref scheme) = target.ann {
            self.setup_generic(target.id, &scheme.body, u::Kind::Value)?;
            self.infer(scheme)?;
            self.fix_generic(target.id, &scheme.body)?;
        }
        self.infer(&target.body)
    }
}

impl<'a, E: External> Infer<ast::Expr> for Context<'a, E> {
    type Result = ();

    fn infer(&mut self, target: &ast::Expr) -> Result<Self::Result> {
        target
            .same_level()
            .dfs(|same_level| match same_level.expr.rep {
                ast::ExprRep::Let(ref let_) => {
                    for local_function in let_.local_functions() {
                        self.infer(local_function)?;
                    }
                    for local_var in let_.local_vars() {
                        if let Some(ref ty) = local_var.ann {
                            self.infer(ty)?;
                        }
                    }
                    Ok(())
                }
                ast::ExprRep::Annotate(ref annotate) => self.infer(&annotate.ann),
                _ => Ok(()),
            })
    }
}

impl<'a, E: External> Infer<ast::InitExpr> for Context<'a, E> {
    type Result = ();

    fn infer(&mut self, target: &ast::InitExpr) -> Result<Self::Result> {
        match target {
            ast::InitExpr::Eval(e) => self.infer(e),
            ast::InitExpr::EnsureInitialized(_) => Ok(()),
        }
    }
}

impl<'a, E: External> Infer<ast::Annotation<ast::Type>> for Context<'a, E> {
    type Result = ();

    fn infer(&mut self, target: &ast::Annotation<ast::Type>) -> Result<Self::Result> {
        let kind = self.infer_on(target.id, &target.body)?;
        self.unify_on(target.id, kind, u::Kind::Type)?;
        Ok(())
    }
}

impl<'a, E: External> Infer<ast::Type> for Context<'a, E> {
    type Result = u::Kind;

    fn infer(&mut self, target: &ast::Type) -> Result<Self::Result> {
        match target {
            ast::Type::Use(use_) => match *use_.get_resolved() {},
            ast::Type::Con(con) => match *con {
                ast::TypeCon::Data(id) => Ok(self.kind_of(id)),
                ast::TypeCon::Builtin(id) => Ok(self.kind_of(id)),
            },
            ast::Type::App(callee, args) => {
                let callee = self.infer(&**callee)?;
                let args = self.infer(args)?;
                let ret = self.u_ctx.new_var();
                let fun = build_kind!(self.u_ctx, (-> ...{args} {ret}));
                self.unify(callee, fun)?;
                Ok(ret)
            }
            ast::Type::Gen(id) => Ok(self.kind_of(*id)),
            ast::Type::Error(e) => panic!("Found Type::Error at kind_inference: {}", e),
        }
    }
}

impl<'a, E: External> Infer<ast::Constraint> for Context<'a, E> {
    type Result = ();

    fn infer(&mut self, target: &ast::Constraint) -> Result<Self::Result> {
        match target.rep {
            ast::ConstraintRep::Class(ref class, ref class_args) => {
                let con_kind = self.kind_of(*class.get_resolved());
                let kind = {
                    let class_args = self.infer_on(target.id, class_args)?;
                    build_kind!(self.u_ctx, (-> ...{class_args} constraint))
                };
                self.unify_on(target.id, kind, con_kind)
            }
        }
    }
}

impl<'a, E: External> Infer<ast::Annotation<ast::Scheme>> for Context<'a, E> {
    type Result = ();

    fn infer(&mut self, target: &ast::Annotation<ast::Scheme>) -> Result<Self::Result> {
        self.infer(&target.body).map_err(|e| e.on(target.id))
    }
}

impl<'a, E: External> Infer<ast::Scheme> for Context<'a, E> {
    type Result = ();

    fn infer(&mut self, target: &ast::Scheme) -> Result<Self::Result> {
        self.infer(&target.s_params)?;
        let kind = self.infer(&target.body)?;
        self.unify(kind, u::Kind::Type)?;
        Ok(())
    }
}
