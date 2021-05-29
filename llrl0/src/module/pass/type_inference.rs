use super::{
    AvailableInstances, Error, External, InferredKinds, Module, ModuleId, Result,
    TextualInformation,
};
use crate::ast::{self, Dfs as _};
use crate::topological_sort;
use derive_new::new;
use either::*;
use if_chain::if_chain;
use itertools::Itertools;
use std::collections::{BTreeSet, HashMap, HashSet};
use typed_arena::Arena;

mod kind_env;
mod type_env;
mod unification;

use kind_env::KindEnv;
use type_env::TypeEnv;
use unification::{self as u, Export as _, GenericTypes as _, Import as _, Resolve as _};

const SATISFACTION_RECURSION_LIMIT: i32 = 20;

pub fn run(module: &mut Module, external: &impl External) -> Result<()> {
    let mut ctx = Context {
        mid: module.id(),
        u_ctx: u::Context::new(KindEnv::new(
            module.id(),
            &mut module.inferred_kinds,
            external,
        )),
        u_types: UnifyingTypes::new(),
        type_env: TypeEnv::new(
            &module.available_instances,
            &mut module.ast_id_generator,
            &module.ast_root,
            &mut module.textual_information,
            external,
        ),
        overlap_checked_available_instances: HashSet::new(),
    };

    ctx.run(&module.ast_root)?;

    for (construct, scheme) in ctx.u_types.schemes {
        if construct.module() == ctx.mid {
            module
                .inferred_types
                .set_scheme(construct, ctx.u_ctx.export(&scheme));
        }
    }

    for (construct, ty) in ctx.u_types.types {
        if construct.module() == ctx.mid {
            module
                .inferred_types
                .set_type(construct, ctx.u_ctx.export(&ty));
        }
    }

    for (construct, instantiation) in ctx.u_types.instantiations {
        if construct.module() == ctx.mid {
            module
                .inferred_types
                .set_instantiation(construct, ctx.u_ctx.export(&instantiation));
        }
    }

    Ok(())
}

#[derive(Debug)]
struct Context<'a, E> {
    mid: ModuleId,
    u_ctx: u::Context<KindEnv<'a, E>>,
    u_types: UnifyingTypes,
    type_env: TypeEnv<'a, E>,
    overlap_checked_available_instances: HashSet<ast::NodeId<ast::ClassCon>>,
}

impl<'a, E: External> Context<'a, E> {
    fn unify<A: Copy, B: Copy>(&mut self, a: A, b: B) -> Result<()>
    where
        u::Context<KindEnv<'a, E>>: u::Resolve<A> + u::Resolve<B>,
    {
        match self.u_ctx.unify(a, b) {
            Ok(()) => Ok(()),
            Err(e) => {
                let a = self.u_ctx.resolve(a);
                let b = self.u_ctx.resolve(b);
                let a = self.u_ctx.export(&a);
                let b = self.u_ctx.export(&b);
                Err(Error::CannotUnifyType(a, b, e))
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
        u::Context<KindEnv<'a, E>>: u::Resolve<A> + u::Resolve<B>,
    {
        self.unify(a, b).map_err(|e| e.on(construct))
    }

    fn unify_with_params(
        &mut self,
        construct: impl Into<ast::Construct>,
        params: Option<&Vec<ast::Parameter>>,
        fun_ty: u::Type,
    ) -> Result<u::Type> {
        match params {
            Some(params) => {
                let param_tys = params
                    .iter()
                    .map(|param| {
                        let ty = self.u_ctx.new_type_var(u::Level::bottom());
                        self.u_types.types.insert(param.id.into(), ty);
                        ty
                    })
                    .collect();
                let ret_ty = self.u_ctx.new_type_var(u::Level::bottom());
                let ty = build_type!(self.u_ctx, (-> ...{param_tys} {ret_ty}));
                self.unify_on(construct, ty, fun_ty)?;
                Ok(ret_ty)
            }
            None => Ok(fun_ty),
        }
    }

    fn install_premise_constraints(
        &mut self,
        premise: &mut u::Premise,
        constraints: Vec<u::Constraint>,
    ) {
        for constraint in constraints {
            self.install_premise_constraint(premise, constraint.id, Vec::new(), constraint.rep);
        }
    }

    fn install_premise_constraint(
        &mut self,
        premise: &mut u::Premise,
        id: u::ConstraintId,
        path: Vec<u::ConstraintId>,
        rep: u::ConstraintRep,
    ) {
        let u::ConstraintRep::Class(class, class_args) = rep;

        let class_con = {
            let mut con = self.type_of(class).clone();
            con.apply_types(class_args.as_ref(), &mut self.u_ctx);
            con
        };
        for constraint in class_con.superclasses {
            let mut path = path.clone();
            path.push(constraint.id);
            self.install_premise_constraint(premise, id, path, constraint.rep);
        }
        premise.add_class_premise(id, path, class, class_args);
    }

    fn build_satisfaction<Error: BuildSatisfactionError>(
        &mut self,
        premise: &u::Premise,
        constraint: &u::Constraint,
        recursion_count: i32,
    ) -> Result<u::Satisfaction> {
        if recursion_count > SATISFACTION_RECURSION_LIMIT {
            Err(Error::recursion_limit_exceeded(constraint, &mut self.u_ctx))?;
        }

        let u::ConstraintRep::Class(class, ref class_args) = constraint.rep;

        for cp in premise.class_premises(class) {
            if cp.has_equivalent_class_args(class_args.as_ref(), &mut self.u_ctx) {
                return Ok(cp.to_satisfaction());
            }
        }

        self.ensure_overlap_checked(class)?;

        // NOTE: There is room for optimization in instance search
        for inst_id in self.type_env.available_instances().get(class) {
            let (inst, ty_args) = {
                let mut inst = self.type_of(*inst_id).clone();
                let ty_args = inst.instantiate_types(u::Level::bottom(), &mut self.u_ctx);
                (inst, ty_args)
            };
            if inst
                .target
                .match_types_to_right(constraint, &mut self.u_ctx)
                .is_ok()
            {
                let s_args = inst
                    .s_params
                    .iter()
                    .map(|c| self.build_satisfaction::<Error>(premise, c, recursion_count + 1))
                    .collect::<Result<Vec<_>>>()
                    .map_err(|e| Error::required_for(e, &inst.target, &mut self.u_ctx))?;

                return Ok(u::Satisfaction::by_instance(
                    *inst_id,
                    u::Instantiation::new(ty_args, s_args),
                ));
            }
        }

        Err(Error::no_matching_instances(
            constraint,
            premise,
            &mut self.u_ctx,
        ))
    }

    fn ensure_overlap_checked(&mut self, id: ast::NodeId<ast::ClassCon>) -> Result<()> {
        if !self.overlap_checked_available_instances.insert(id) {
            return Ok(());
        }

        let insts = self.type_env.available_instances().get(id);

        for (a_id, b_id) in insts
            .iter()
            .enumerate()
            .flat_map(|(i, a)| insts.iter().skip(i + 1).map(move |b| (*a, *b)))
        {
            // These instances are defined together in another module.
            if ast::NodeId::module(a_id) == ast::NodeId::module(b_id) && a_id.module() != self.mid {
                // TODO: Reduce more repeated check
                continue;
            }

            let mut a = self.type_of(a_id).clone();
            let mut b = self.type_of(b_id).clone();

            // unnecessary for overlap check
            a.s_params.clear();
            b.s_params.clear();

            a.instantiate_types(u::Level::top(), &mut self.u_ctx);
            b.instantiate_types(u::Level::top(), &mut self.u_ctx);

            if a.target.unify_types(&b.target, &mut self.u_ctx).is_ok() {
                Err(Error::OverlappingInstances(
                    vec![a_id, b_id].into_iter().collect(),
                ))?;
            }
        }

        Ok(())
    }

    fn finalize_scope(
        &mut self,
        construct: ast::Construct,
        mut scope: u::Scope,
        quantify_vars: BTreeSet<u::Var>,
        mut outer: Option<&mut u::Scope>,
    ) -> Result<(Vec<u::Gen>, Vec<u::Constraint>)> {
        debug_assert!(match outer {
            Some(ref outer_scope) => outer_scope.level() < scope.level(),
            None => scope.level() == u::Level::top(),
        });

        let current_level = scope.level();
        let current_constraints = Arena::new();
        let mut outer_constraints = Vec::new();
        let mut s_params = Vec::new();
        let mut defaulting_vars = HashMap::<u::Var, Vec<_>>::new();

        // rust-lang/rust#43244
        for constraint in scope.consume_context_constraints() {
            // If the current level is not a top-level and the constraint does not contain
            // the types on the current level, it can be deferred to the outer scope.
            if outer.is_some()
                && constraint.body().compute_shallowest_level(&mut self.u_ctx) < current_level
            {
                outer_constraints.push(constraint);
                continue;
            }

            let current_level_vars = constraint
                .body()
                .enumerate_vars(current_level, &mut self.u_ctx);
            let defaulting_required_vars = current_level_vars
                .difference(&quantify_vars)
                .copied()
                .collect::<BTreeSet<_>>();

            if !current_level_vars.is_empty() && defaulting_required_vars.is_empty() {
                // Constraints like (From <a> <b>) where every type variable is in `quantify_vars`
                let constraint = constraint.into_premise(self.type_env.new_constraint_id());
                s_params.push(constraint);
            } else {
                // Constraints like (Number <a>) where some type variable is not in `quantify_vars`
                // or constant constraints like (Number I32)
                let constraint = current_constraints.alloc(constraint);
                for var in defaulting_required_vars {
                    defaulting_vars
                        .entry(var)
                        .or_default()
                        .push(constraint.body());
                }
            }
        }

        let ty_params = quantify_vars
            .into_iter()
            .enumerate()
            .map(|(i, var)| {
                let kind = self.u_ctx.get_kind(var).clone();
                let gen_name = if current_level == u::Level::top() {
                    format!("_{}", i)
                } else {
                    format!("_{}.{}", current_level.depth(), i)
                };
                let gen = self.type_env.new_gen(construct, gen_name);
                self.u_ctx.kind_env.put(gen, kind);
                self.unify(var, u::Type::Gen(gen, u::Level::top()))?;
                Ok(gen)
            })
            .collect::<Result<Vec<_>>>()?;

        for (var, related_constraints) in defaulting_vars {
            self.resolve_ambiguity(var, &related_constraints)?;
        }

        for constraint in outer_constraints {
            match self.build_satisfaction::<()>(scope.context_premise(), constraint.body(), 0) {
                Ok(satisfaction) => constraint.resolve_by(satisfaction),
                Err(Error::BuildSatisfactionFailedAtThisScope) => {
                    outer
                        .as_mut()
                        .unwrap()
                        .inherit_deferred_context_constraint(constraint);
                }
                Err(e) => Err(e)?,
            }
        }

        for constraint in current_constraints.into_vec() {
            let satisfaction =
                self.build_satisfaction::<Error>(scope.context_premise(), constraint.body(), 0)?;
            constraint.resolve_by(satisfaction);
        }

        Ok((ty_params, s_params))
    }

    fn finalize_scope_on(
        &mut self,
        construct: impl Into<ast::Construct>,
        scope: u::Scope,
        quantify_vars: BTreeSet<u::Var>,
        outer: Option<&mut u::Scope>,
    ) -> Result<(Vec<u::Gen>, Vec<u::Constraint>)> {
        let construct = construct.into();
        self.finalize_scope(construct, scope, quantify_vars, outer)
            .map_err(|e| e.on(construct))
    }

    fn finalized_scope_instantiation(
        ty_params: &[u::Gen],
        s_params: &[u::Constraint],
    ) -> u::Instantiation {
        let ty_args = ty_params
            .iter()
            .map(|tp| u::Type::Gen(*tp, u::Level::top()))
            .collect();

        let s_args = s_params
            .iter()
            .map(|constraint| u::Satisfaction::by_premise(constraint.id, Vec::new()))
            .collect();

        u::Instantiation::new(ty_args, s_args)
    }

    fn resolve_ambiguity(
        &mut self,
        var: u::Var,
        related_constraints: &[&u::Constraint],
    ) -> Result<()> {
        if related_constraints.is_empty() {
            return self.unify(var, u::Type::Con(u::Con::unit()));
        }

        if related_constraints
            .iter()
            .any(|c| matches!(c.matches_fpnumber(), Some(ty) if self.u_ctx.equal(var, ty)))
        {
            return self.unify(var, u::Type::Con(u::Con::F64));
        }

        if related_constraints
            .iter()
            .any(|c| matches!(c.matches_number(), Some(ty) if self.u_ctx.equal(var, ty)))
        {
            return self.unify(var, u::Type::Con(u::Con::I32));
        }

        // Defaults to unit: is it safe? we can produce an error as below:
        // let ty = self.u_ctx.export(&var);
        // let constraints = self.u_ctx.export(related_constraints);
        // Err(Error::CannotResolveAmbiguity(ty, constraints))
        self.unify(var, u::Type::Con(u::Con::unit()))
    }

    fn run(&mut self, root: &ast::Root) -> Result<()> {
        self.verify_classes_hierarchy(&root.class_cons)?;

        for inst in root.instance_cons.values() {
            let ast::ConstraintRep::Class(ref class, _) = inst.target.rep;
            self.ensure_overlap_checked(*class.get_resolved())?;
        }

        for inst in root.instance_cons.values() {
            self.infer_instance_satisfaction(inst)?;
        }

        self.infer_functions(&root.functions)?;

        for class in root.classes() {
            self.infer_class(class)?;
        }

        for inst in root.instances() {
            self.infer_instance(inst)?;
        }

        for macro_ in root.macros.values() {
            self.infer_macro(macro_)?;
        }

        for expr in root.init_expressions.iter() {
            if let ast::InitExpr::Eval(expr) = expr {
                let mut scope = u::Scope::new();
                let ty = self.infer_expr(&mut ExprScope::new(&mut scope, None), expr)?;
                self.finalize_scope_on(expr.id, scope, BTreeSet::new(), None)?;
                self.u_types.types.insert(expr.id.into(), ty);
            }
        }

        // Ensure that the Scheme is stored in self.u_types
        for id in root.builtin_ops.keys() {
            self.type_of(*id);
        }
        for id in root.c_functions.keys() {
            self.type_of(*id);
        }
        for id in root.class_methods.keys() {
            self.type_of(*id);
        }
        for id in root.data_value_cons.keys() {
            self.type_of(*id);
        }

        for id in root.builtin_value_cons.keys() {
            self.type_of(*id);
        }

        for (construct, ty) in self
            .u_types
            .schemes
            .iter()
            .flat_map(|(c, scheme)| scheme.types().map(move |ty| (*c, ty)))
            .chain(self.u_types.types.iter().map(|(c, ty)| (*c, *ty)))
            .chain(
                self.u_types
                    .instantiations
                    .iter()
                    .flat_map(|(c, instantiation)| instantiation.types().map(move |ty| (*c, ty))),
            )
        {
            if let Err(_) = self.u_ctx.default_vars(ty, u::Type::Con(u::Con::unit())) {
                let ty = self.u_ctx.export(&ty);
                Err(Error::CannotResolveAmbiguity(ty, Vec::new()).on(construct))?;
            }
        }

        Ok(())
    }

    fn verify_classes_hierarchy(
        &mut self,
        classes: &HashMap<ast::NodeId<ast::ClassCon>, ast::ClassCon>,
    ) -> Result<()> {
        for group in topological_sort::run(classes.values().map(|con| (con.id, con))) {
            if group.len() >= 2 {
                let ids = group.iter().map(|cls| cls.id).collect();
                Err(Error::CyclicClasses(ids))?
            }
        }
        Ok(())
    }

    fn infer_instance_satisfaction(&mut self, inst: &ast::InstanceCon) -> Result<()> {
        let inst_id = inst.id;
        let inst = self.type_of(inst_id).clone();
        let premise = {
            let mut premise = u::Premise::new();
            self.install_premise_constraints(&mut premise, inst.s_params);
            premise
        };

        let u::ConstraintRep::Class(class, class_args) = inst.target.rep;

        let class_con = {
            let mut con = self.type_of(class).clone();
            con.apply_types(class_args.as_ref(), &mut self.u_ctx);
            con
        };

        let superclass_satisfactions = class_con
            .superclasses
            .iter()
            .map(|c| self.build_satisfaction::<Error>(&premise, c, 0))
            .collect::<Result<Vec<_>>>()
            .map_err(|e| e.on(inst_id))?;

        self.u_types.instantiations.insert(
            inst_id.into(),
            u::Instantiation::new(Vec::new(), superclass_satisfactions),
        );

        Ok(())
    }

    fn infer_functions(
        &mut self,
        functions: &HashMap<ast::NodeId<ast::Function>, ast::Function>,
    ) -> Result<()> {
        for group in topological_sort::run(
            functions
                .values()
                .filter(|f| f.ann.is_none())
                .map(|f| (f.id, f)),
        ) {
            self.infer_impl_functions(group.as_slice())?;
        }

        for f in functions.values().filter(|f| f.ann.is_some()) {
            self.infer_expl_function(&f)?;
        }

        Ok(())
    }

    fn infer_impl_functions(&mut self, functions: &[&ast::Function]) -> Result<()> {
        if functions.is_empty() {
            return Ok(());
        }

        debug_assert!(functions.iter().all(|f| f.ann.is_none()));
        let mut scope = u::Scope::new();

        let ret_tys = functions
            .iter()
            .map(|f| self.prepare_tentative_scheme_with_params(&mut scope, f.id, f.params.as_ref()))
            .collect::<Vec<_>>();

        for (f, a) in functions.iter().zip(ret_tys) {
            let b = self.infer_expr(&mut ExprScope::new(&mut scope, Some(a)), &f.body)?;
            self.unify_on(f.id, a, b)?;
        }

        // We only quantify type variables that appear in every function type.
        let quantify_vars = self.compute_common_type_vars(&scope, functions.iter().map(|f| f.id));
        let (ty_params, s_params) =
            self.finalize_scope_on(functions[0].id, scope, quantify_vars, None)?;
        let instantiation = Self::finalized_scope_instantiation(&ty_params, &s_params);

        for f in functions.iter() {
            let scheme =
                u::Scheme::new(ty_params.clone(), s_params.clone(), self.type_of(f.id).body);
            self.u_types.schemes.insert(f.id.into(), scheme);

            f.body.dfs_do(|expr| {
                if_chain! {
                    if let ast::ExprRep::Use(ref use_) = expr.rep;
                    if let ast::Value::Function(id) = *use_.get_resolved();
                    if functions.iter().any(|f| f.id == id);
                    then {
                        self.u_types
                            .instantiations
                            .insert(expr.id.into(), instantiation.clone());
                    }
                }
            });
        }

        Ok(())
    }

    fn prepare_tentative_scheme_with_params(
        &mut self,
        scope: &mut u::Scope,
        construct: impl Into<ast::Construct>,
        params: Option<&Vec<ast::Parameter>>,
    ) -> u::Type {
        let construct = construct.into();
        let fun_ty = self.u_ctx.new_type_var(scope.level());
        self.u_types.schemes.insert(construct, fun_ty.into());
        self.unify_with_params(construct, params, fun_ty).unwrap()
    }

    fn compute_common_type_vars<It>(&mut self, scope: &u::Scope, constructs: It) -> BTreeSet<u::Var>
    where
        It: Iterator,
        It::Item: Into<ast::Construct>,
    {
        constructs
            .map(|construct| {
                let ty = self.u_types.schemes[&construct.into()].body;
                self.u_ctx.enumerate_vars(ty, scope.level())
            })
            .fold1(|a, b| a.intersection(&b).copied().collect())
            .unwrap_or_default()
    }

    fn infer_expl_function(&mut self, f: &ast::Function) -> Result<()> {
        debug_assert!(f.ann.is_some());
        let mut scope = u::Scope::new();

        let scheme = self.type_of(f.id).clone();
        // Since functions are on top-level, we don't need to register and subst scoped gen types.
        self.install_premise_constraints(scope.context_premise_mut(), scheme.s_params);

        {
            let a = self.unify_with_params(f.id, f.params.as_ref(), scheme.body)?;
            let b = self.infer_expr(&mut ExprScope::new(&mut scope, Some(a)), &f.body)?;
            self.unify_on(f.id, a, b)?;
        }

        self.finalize_scope_on(f.id, scope, BTreeSet::new(), None)?;
        Ok(())
    }

    fn infer_class(&mut self, class: ast::Class) -> Result<()> {
        let mut scope = u::Scope::new();

        let class_constraint = self.u_ctx.import(&class.con.constraint());
        // Since classes are on top-level, we don't need to register and subst scoped gen types.
        self.install_premise_constraints(scope.context_premise_mut(), vec![class_constraint]);

        for method in class.methods() {
            self.infer_class_method(&mut scope, method)?;
        }

        // NOTE: Should we reject definitions where the type parameters of the class does not
        // appear in the type scheme of the methods?

        self.finalize_scope_on(class.con.id, scope, BTreeSet::new(), None)?;
        Ok(())
    }

    fn infer_class_method(
        &mut self,
        outer_scope: &mut u::Scope,
        method: &ast::ClassMethod,
    ) -> Result<()> {
        let mut scope = u::Scope::new_inner(outer_scope);

        let mut scheme = self.u_ctx.import(&method.ann);
        scope.register_scoped_gen_types(scheme.generic_types());
        scheme.subst_types(scope.scoped_gen_types(), &mut self.u_ctx);
        self.install_premise_constraints(scope.context_premise_mut(), scheme.s_params);

        {
            let a = self.unify_with_params(method.id, method.params.as_ref(), scheme.body)?;
            if let Some(ref body) = method.default_body {
                let b = self.infer_expr(&mut ExprScope::new(&mut scope, Some(a)), body)?;
                self.unify_on(method.id, a, b)?;
            }
        }

        self.finalize_scope_on(method.id, scope, BTreeSet::new(), Some(outer_scope))?;
        Ok(())
    }

    fn infer_instance(&mut self, inst: ast::Instance) -> Result<()> {
        let mut scope = u::Scope::new();

        let inst_con = self.type_of(inst.con.id).clone();
        // Since instances are on top-level, we don't need to register and subst scoped gen types.
        self.install_premise_constraints(scope.context_premise_mut(), inst_con.s_params);

        let u::ConstraintRep::Class(class, class_args) = inst_con.target.rep;
        let class_params = self.type_of(class).ty_params.clone();
        let class_inst_map = u::gen_type_mapping(class_params.as_ref(), class_args.as_ref());

        for method in inst.methods() {
            self.infer_instance_method(&mut scope, &class_inst_map, method)?;
        }

        self.finalize_scope_on(inst.con.id, scope, BTreeSet::new(), None)?;
        Ok(())
    }

    fn infer_instance_method(
        &mut self,
        outer_scope: &mut u::Scope,
        class_inst_map: &HashMap<u::Gen, u::Type>,
        method: &ast::InstanceMethod,
    ) -> Result<()> {
        let mut scope = u::Scope::new_inner(outer_scope);

        let class_method_ann = &self
            .type_env
            .class_method(*method.class_method.get_resolved())
            .ann;

        let mut orig_scheme = self.u_ctx.import(&class_method_ann.body);
        orig_scheme.subst_types(class_inst_map, &mut self.u_ctx);
        let mut scheme = match method.ann {
            Some(ref inst_method_ann) => {
                let inst_scheme = self.u_ctx.import(inst_method_ann);
                if !inst_scheme.alpha_equal(&orig_scheme, &mut self.u_ctx) {
                    Err(Error::MethodTypeSchemeMismatch(
                        class_method_ann.body.clone(),
                        inst_method_ann.body.clone(),
                    ))?;
                }
                inst_scheme
            }
            None => orig_scheme,
        };
        scope.register_scoped_gen_types(scheme.generic_types());
        scheme.subst_types(scope.scoped_gen_types(), &mut self.u_ctx);
        self.install_premise_constraints(scope.context_premise_mut(), scheme.s_params.clone());

        {
            let a = self.unify_with_params(method.id, method.params.as_ref(), scheme.body)?;
            let b = self.infer_expr(&mut ExprScope::new(&mut scope, Some(a)), &method.body)?;
            self.unify_on(method.id, a, b)?;
        }

        self.finalize_scope_on(method.id, scope, BTreeSet::new(), Some(outer_scope))?;

        // Since instance methods are instance-local, the Scheme to be stored does not include
        // instance type parameters and instance constraints.
        self.u_types.schemes.insert(method.id.into(), scheme);

        Ok(())
    }

    fn infer_macro(&mut self, macro_: &ast::Macro) -> Result<()> {
        let mut scope = u::Scope::new();

        let src = self.u_ctx.import(&ast::Macro::src_ty());
        let dest = self.u_ctx.import(&ast::Macro::dest_ty());

        self.u_types.types.insert(macro_.param.id.into(), src);

        let body = self.infer_expr(&mut ExprScope::new(&mut scope, Some(dest)), &macro_.body)?;
        self.unify_on(macro_.id, body, dest)?;

        self.finalize_scope_on(macro_.id, scope, BTreeSet::new(), None)?;
        Ok(())
    }

    fn infer_expr(&mut self, es: &mut ExprScope, expr: &ast::Expr) -> Result<u::Type> {
        match expr.rep {
            ast::ExprRep::Use(ref use_) => {
                let scheme = match self.type_of(*use_.get_resolved()) {
                    Left(scheme) => scheme.clone(),
                    Right(ty) => ty.into(),
                };
                self.infer_use(es.scope, expr.id, scheme)
            }
            ast::ExprRep::Con(con) => {
                let scheme = self.type_of(con).clone();
                self.infer_use(es.scope, expr.id, scheme)
            }
            ast::ExprRep::Const(ref lit) => self.infer_const(es.scope, expr.id, lit),
            ast::ExprRep::App(ref apply) => self.infer_expr_apply(es, expr.id, apply),
            ast::ExprRep::Capture(_) => {
                Ok(build_type!(self.u_ctx, ((con {u::Con::SYNTAX}) (con {u::Con::SEXP}))))
            }
            ast::ExprRep::Annotate(ref annotate) => self.infer_expr_annotate(es, annotate),
            ast::ExprRep::Let(ref let_) => self.infer_expr_let(es, let_),
            ast::ExprRep::Seq(ref seq) => self.infer_expr_seq(es, seq),
            ast::ExprRep::If(ref if_) => self.infer_expr_if(es, if_),
            ast::ExprRep::While(ref while_) => self.infer_expr_while(es, while_),
            ast::ExprRep::Match(ref match_) => self.infer_expr_match(es, match_),
            ast::ExprRep::Return(ref ret) => {
                if let Some(return_ty) = es.return_ty {
                    if let Some(ref ret) = **ret {
                        let ret_ty = self.infer_expr(es, ret)?;
                        self.unify_on(expr.id, return_ty, ret_ty)?;
                    } else {
                        self.unify_on(expr.id, return_ty, u::Type::Con(u::Con::unit()))?;
                    }
                } else {
                    Err(Error::CannotUseReturnInThisContext.on(expr.id))?;
                }
                Ok(self.u_ctx.new_type_var(es.scope.level()))
            }
        }
    }

    fn infer_expr_apply(
        &mut self,
        es: &mut ExprScope,
        id: ast::NodeId<ast::Expr>,
        apply: &ast::ExprApply,
    ) -> Result<u::Type> {
        let callee = self.infer_expr(es, &apply.callee)?;
        let args = apply
            .args
            .iter()
            .map(|arg| self.infer_expr(es, arg))
            .collect::<Result<Vec<_>>>()?;
        let ret = self.u_ctx.new_type_var(es.scope.level());
        let callee_ty = build_type!(self.u_ctx, (-> ...{args} {ret}));
        self.unify_on(id, callee, callee_ty)?;
        Ok(ret)
    }

    fn infer_expr_let(&mut self, es: &mut ExprScope, let_: &ast::ExprLet) -> Result<u::Type> {
        // 0. insert explicit function schemes
        for f in let_.local_functions().filter(|f| f.ann.is_some()) {
            let mut scheme = self.u_ctx.import(f.ann.as_ref().unwrap());
            scheme.subst_types(es.scope.scoped_gen_types(), &mut self.u_ctx);
            self.u_types.schemes.insert(f.id.into(), scheme);
        }

        // 1. infer implicit local functions
        for group in topological_sort::run(
            let_.local_functions()
                .filter(|f| f.ann.is_none())
                .map(|f| (f.id, f)),
        ) {
            self.infer_impl_local_functions(es.scope, group.as_slice())?;
        }

        // 2. infer explicit local functions
        for f in let_.local_functions().filter(|f| f.ann.is_some()) {
            self.infer_expl_local_function(es.scope, f)?;
        }

        // 3. infer local variables
        for var in let_.local_vars() {
            let a = self.infer_expr(es, &var.init)?;
            if let Some(ref b) = var.ann {
                let b = self.u_ctx.import(b);
                let b = self.u_ctx.subst(es.scope.scoped_gen_types(), b);
                self.unify_on(var.id, a, b)?;
            }
            self.u_types.types.insert(var.id.into(), a);
        }

        self.infer_expr(es, &let_.body)
    }

    fn infer_expr_seq(&mut self, es: &mut ExprScope, seq: &ast::ExprSeq) -> Result<u::Type> {
        for stmt in seq.stmts.iter() {
            self.infer_expr(es, stmt)?;
        }
        self.infer_expr(es, &seq.ret)
    }

    fn infer_expr_annotate(
        &mut self,
        es: &mut ExprScope,
        annotate: &ast::ExprAnnotate,
    ) -> Result<u::Type> {
        let a = self.infer_expr(es, &annotate.body)?;
        let b = self.u_ctx.import(&annotate.ann);
        let b = self.u_ctx.subst(es.scope.scoped_gen_types(), b);
        self.unify_on(annotate.body.id, a, b)?;
        Ok(a)
    }

    fn infer_expr_if(&mut self, es: &mut ExprScope, if_: &ast::ExprIf) -> Result<u::Type> {
        let cond = self.infer_expr(es, &if_.cond)?;
        let cond_ty = u::Type::Con(u::Con::BOOL);
        self.unify_on(if_.cond.id, cond, cond_ty)?;
        let then = self.infer_expr(es, &if_.then)?;
        let else_ = self.infer_expr(es, &if_.else_)?;
        self.unify_on(if_.else_.id, then, else_)?;
        Ok(then)
    }

    fn infer_expr_while(&mut self, es: &mut ExprScope, while_: &ast::ExprWhile) -> Result<u::Type> {
        let cond = self.infer_expr(es, &while_.cond)?;
        let cond_ty = u::Type::Con(u::Con::BOOL);
        self.unify_on(while_.cond.id, cond, cond_ty)?;
        self.infer_expr(es, &while_.body)?;
        Ok(u::Type::Con(u::Con::unit()))
    }

    fn infer_impl_local_functions(
        &mut self,
        outer_scope: &mut u::Scope,
        functions: &[&ast::LocalFun],
    ) -> Result<()> {
        if functions.is_empty() {
            return Ok(());
        }

        debug_assert!(functions.iter().all(|f| f.ann.is_none()));
        let mut scope = u::Scope::new_inner(outer_scope);

        let ret_tys = functions
            .iter()
            .map(|f| self.prepare_tentative_scheme_with_params(&mut scope, f.id, Some(&f.params)))
            .collect::<Vec<_>>();

        for (f, a) in functions.iter().zip(ret_tys) {
            let b = self.infer_expr(&mut ExprScope::new(&mut scope, Some(a)), &f.body)?;
            self.unify_on(f.id, a, b)?;
        }

        let quantify_vars = self.compute_common_type_vars(&scope, functions.iter().map(|f| f.id));
        let (ty_params, s_params) =
            self.finalize_scope_on(functions[0].id, scope, quantify_vars, Some(outer_scope))?;
        let instantiation = Self::finalized_scope_instantiation(&ty_params, &s_params);

        for f in functions.iter() {
            let scheme =
                u::Scheme::new(ty_params.clone(), s_params.clone(), self.type_of(f.id).body);
            self.u_types.schemes.insert(f.id.into(), scheme);

            f.body.dfs_do(|expr| {
                if_chain! {
                    if let ast::ExprRep::Use(ref use_) = expr.rep;
                    if let ast::Value::LocalFun(id) = *use_.get_resolved();
                    if functions.iter().any(|f| f.id == id);
                    then {
                        self.u_types
                            .instantiations
                            .insert(expr.id.into(), instantiation.clone());
                    }
                }
            });
        }

        Ok(())
    }

    fn infer_expl_local_function(
        &mut self,
        outer_scope: &mut u::Scope,
        f: &ast::LocalFun,
    ) -> Result<()> {
        debug_assert!(f.ann.is_some());
        let mut scope = u::Scope::new_inner(outer_scope);

        let mut scheme = self.u_ctx.import(f.ann.as_ref().unwrap());
        scope.register_scoped_gen_types(scheme.generic_types());
        scheme.subst_types(scope.scoped_gen_types(), &mut self.u_ctx);
        self.install_premise_constraints(scope.context_premise_mut(), scheme.s_params);

        {
            let a = self.unify_with_params(f.id, Some(&f.params), scheme.body)?;
            let b = self.infer_expr(&mut ExprScope::new(&mut scope, Some(a)), &f.body)?;
            self.unify_on(f.id, a, b)?;
        }

        self.finalize_scope_on(f.id, scope, BTreeSet::new(), Some(outer_scope))?;
        Ok(())
    }

    fn infer_expr_match(&mut self, es: &mut ExprScope, match_: &ast::ExprMatch) -> Result<u::Type> {
        let target = self.infer_expr(es, &match_.target)?;
        let ret = self.u_ctx.new_type_var(es.scope.level());

        for (pattern, body) in match_.clauses.iter() {
            let pattern_ty = self.infer_pattern(es.scope, pattern)?;
            self.unify_on(pattern.id, target, pattern_ty)?;
            let body_ty = self.infer_expr(es, body)?;
            self.unify_on(body.id, ret, body_ty)?;
        }

        Ok(ret)
    }

    fn infer_pattern(&mut self, scope: &mut u::Scope, pattern: &ast::Pattern) -> Result<u::Type> {
        match pattern.rep {
            ast::PatternRep::Var(ref var) => self.infer_pattern_var(scope, var),
            ast::PatternRep::Wildcard => Ok(self.u_ctx.new_type_var(scope.level())),
            ast::PatternRep::Decon(ref decon) => self.infer_pattern_decon(scope, pattern.id, decon),
            ast::PatternRep::Const(ref c) => self.infer_const(scope, pattern.id, c),
        }
    }

    fn infer_pattern_var(
        &mut self,
        scope: &mut u::Scope,
        var: &ast::PatternVar,
    ) -> Result<u::Type> {
        let ty = self.u_ctx.new_type_var(scope.level());
        self.u_types.types.insert(var.id.into(), ty);
        if let Some(ref as_pat) = var.as_pat {
            let as_pat_ty = self.infer_pattern(scope, as_pat)?;
            self.unify_on(var.id, ty, as_pat_ty)?;
        }
        Ok(ty)
    }

    fn infer_pattern_decon(
        &mut self,
        scope: &mut u::Scope,
        id: ast::NodeId<ast::Pattern>,
        decon: &ast::PatternDecon,
    ) -> Result<u::Type> {
        let value_con = *decon.use_.get_resolved();
        {
            let a = decon.arity();
            let b = self.type_env.value_con_arity(value_con);
            if a != b {
                Err(Error::ArityMismatch(a, b).on(id))?
            }
        }
        let scheme = self.type_of(*decon.use_.get_resolved()).clone();
        let ty = self.infer_use(scope, id, scheme)?;
        match decon.fields {
            Some(ref fields) => {
                let arg_tys = fields
                    .iter()
                    .map(|field| self.infer_pattern(scope, field))
                    .collect::<Result<Vec<_>>>()?;
                let ret_ty = self.u_ctx.new_type_var(scope.level());
                let con_ty = build_type!(self.u_ctx, (-> ...{arg_tys} {ret_ty}));
                self.unify_on(id, ty, con_ty)?;
                Ok(ret_ty)
            }
            None => Ok(ty),
        }
    }

    fn infer_const(
        &mut self,
        scope: &mut u::Scope,
        construct: impl Into<ast::Construct>,
        const_: &ast::Const,
    ) -> Result<u::Type> {
        let construct = construct.into();
        let ty = match const_ {
            ast::Const::Integer(_, _) => {
                let ty = self.u_ctx.new_type_var(scope.level());
                let s = scope.put_context_constraint(u::Constraint::number(ty));
                self.u_types
                    .instantiations
                    .insert(construct, u::Instantiation::new(Vec::new(), vec![s]));
                ty
            }
            ast::Const::FPNumber(_) => {
                let ty = self.u_ctx.new_type_var(scope.level());
                let s = scope.put_context_constraint(u::Constraint::fpnumber(ty));
                self.u_types
                    .instantiations
                    .insert(construct, u::Instantiation::new(Vec::new(), vec![s]));
                ty
            }
            ast::Const::String(_) => u::Type::Con(u::Con::STRING),
            ast::Const::Char(_) => u::Type::Con(u::Con::CHAR),
            ast::Const::SyntaxSexp(_) => {
                build_type!(self.u_ctx, ((con {u::Con::SYNTAX}) (con {u::Con::SEXP})))
            }
        };
        self.u_types.types.insert(construct, ty);
        Ok(ty)
    }

    fn infer_use(
        &mut self,
        scope: &mut u::Scope,
        construct: impl Into<ast::Construct>,
        mut scheme: u::Scheme,
    ) -> Result<u::Type> {
        let ty_args = scheme.instantiate_types(scope.level(), &mut self.u_ctx);
        let s_args = scheme
            .s_params
            .into_iter()
            .map(|c| scope.put_context_constraint(c))
            .collect();
        self.u_types
            .instantiations
            .insert(construct.into(), u::Instantiation::new(ty_args, s_args));
        Ok(scheme.body)
    }
}

trait TypeOf<'d, T> {
    type Dest;
    fn type_of(&'d mut self, src: T) -> Self::Dest;
}

impl<'a, 'd, E: External> TypeOf<'d, ast::NodeId<ast::Function>> for Context<'a, E> {
    type Dest = &'d u::Scheme;

    fn type_of(&'d mut self, id: ast::NodeId<ast::Function>) -> Self::Dest {
        let u_ctx = &mut self.u_ctx;
        let type_env = &self.type_env;

        self.u_types.schemes.entry(id.into()).or_insert_with(|| {
            let scheme = type_env
                .function_scheme(id)
                .unwrap_or_else(|| panic!("The Scheme is not pre-inserted: {}", id));
            u_ctx.import(scheme)
        })
    }
}

impl<'a, 'd, E: External> TypeOf<'d, ast::NodeId<ast::CFunction>> for Context<'a, E> {
    type Dest = u::Type;

    fn type_of(&'d mut self, id: ast::NodeId<ast::CFunction>) -> Self::Dest {
        let u_ctx = &mut self.u_ctx;
        let type_env = &self.type_env;

        *self.u_types.types.entry(id.into()).or_insert_with(|| {
            let ty = type_env.c_function_type(id);
            u_ctx.import(ty)
        })
    }
}

impl<'a, 'd, E: External> TypeOf<'d, ast::NodeId<ast::BuiltinOp>> for Context<'a, E> {
    type Dest = &'d u::Scheme;

    fn type_of(&'d mut self, id: ast::NodeId<ast::BuiltinOp>) -> Self::Dest {
        let u_ctx = &mut self.u_ctx;
        let type_env = &self.type_env;

        self.u_types.schemes.entry(id.into()).or_insert_with(|| {
            let scheme = type_env.builtin_op_scheme(id);
            u_ctx.import(scheme)
        })
    }
}

impl<'a, 'd, E: External> TypeOf<'d, ast::NodeId<ast::DataValueCon>> for Context<'a, E> {
    type Dest = &'d u::Scheme;

    fn type_of(&'d mut self, id: ast::NodeId<ast::DataValueCon>) -> Self::Dest {
        let u_ctx = &mut self.u_ctx;
        let type_env = &mut self.type_env;

        self.u_types
            .schemes
            .entry(id.into())
            .or_insert_with(|| u_ctx.import(&*type_env.get_or_create_data_value_con_scheme(id)))
    }
}

impl<'a, 'd, E: External> TypeOf<'d, ast::NodeId<ast::BuiltinValueCon>> for Context<'a, E> {
    type Dest = &'d u::Scheme;

    fn type_of(&'d mut self, id: ast::NodeId<ast::BuiltinValueCon>) -> Self::Dest {
        let u_ctx = &mut self.u_ctx;
        let type_env = &mut self.type_env;

        self.u_types
            .schemes
            .entry(id.into())
            .or_insert_with(|| u_ctx.import(&*type_env.get_or_create_builtin_value_con_scheme(id)))
    }
}

impl<'a, 'd, E: External> TypeOf<'d, ast::NodeId<ast::ClassCon>> for Context<'a, E> {
    type Dest = &'d u::ClassCon;

    fn type_of(&'d mut self, id: ast::NodeId<ast::ClassCon>) -> Self::Dest {
        let u_ctx = &mut self.u_ctx;
        let type_env = &self.type_env;

        self.u_types
            .class_cons
            .entry(id)
            .or_insert_with(|| u_ctx.import(type_env.class_con(id)))
    }
}

impl<'a, 'd, E: External> TypeOf<'d, ast::NodeId<ast::ClassMethod>> for Context<'a, E> {
    type Dest = &'d u::Scheme;

    fn type_of(&'d mut self, id: ast::NodeId<ast::ClassMethod>) -> Self::Dest {
        let u_ctx = &mut self.u_ctx;
        let type_env = &mut self.type_env;

        self.u_types.schemes.entry(id.into()).or_insert_with(|| {
            u_ctx.import(&*type_env.get_or_create_class_method_external_scheme(id))
        })
    }
}

impl<'a, 'd, E: External> TypeOf<'d, ast::NodeId<ast::InstanceCon>> for Context<'a, E> {
    type Dest = &'d u::InstanceCon;

    fn type_of(&'d mut self, id: ast::NodeId<ast::InstanceCon>) -> Self::Dest {
        let u_ctx = &mut self.u_ctx;
        let type_env = &self.type_env;

        self.u_types
            .instance_cons
            .entry(id)
            .or_insert_with(|| u_ctx.import(type_env.instance_con(id)))
    }
}

impl<'a, 'd, E: External> TypeOf<'d, ast::Value> for Context<'a, E> {
    type Dest = Either<&'d u::Scheme, u::Type>;

    fn type_of(&'d mut self, value: ast::Value) -> Self::Dest {
        match value {
            ast::Value::Function(id) => Left(self.type_of(id)),
            ast::Value::CFunction(id) => Right(self.type_of(id)),
            ast::Value::BuiltinOp(id) => Left(self.type_of(id)),
            ast::Value::ClassMethod(id) => Left(self.type_of(id)),
            ast::Value::Parameter(id) => Right(self.type_of(id)),
            ast::Value::LocalVar(id) => Right(self.type_of(id)),
            ast::Value::LocalFun(id) => Left(self.type_of(id)),
            ast::Value::PatternVar(id) => Right(self.type_of(id)),
        }
    }
}

impl<'a, 'd, E: External> TypeOf<'d, ast::ValueCon> for Context<'a, E> {
    type Dest = &'d u::Scheme;

    fn type_of(&'d mut self, con: ast::ValueCon) -> Self::Dest {
        match con {
            ast::ValueCon::Data(id) => self.type_of(id),
            ast::ValueCon::Builtin(id) => self.type_of(id),
        }
    }
}

macro_rules! impl_pre_inserted_scheme_construct {
    ($ty:ty) => {
        impl<'a, 'd, E: External> TypeOf<'d, ast::NodeId<$ty>> for Context<'a, E> {
            type Dest = &'d u::Scheme;

            fn type_of(&'d mut self, id: ast::NodeId<$ty>) -> Self::Dest {
                self.u_types
                    .schemes
                    .get(&id.into())
                    .unwrap_or_else(|| panic!("The Scheme is not pre-inserted: {}", id))
            }
        }
    };
}

impl_pre_inserted_scheme_construct!(ast::LocalFun);

macro_rules! impl_pre_inserted_type_construct {
    ($ty:ty) => {
        impl<'a, 'd, E: External> TypeOf<'d, ast::NodeId<$ty>> for Context<'a, E> {
            type Dest = u::Type;

            fn type_of(&'d mut self, id: ast::NodeId<$ty>) -> Self::Dest {
                *self
                    .u_types
                    .types
                    .get(&id.into())
                    .unwrap_or_else(|| panic!("The Type is not pre-inserted: {}", id))
            }
        }
    };
}

impl_pre_inserted_type_construct!(ast::Parameter);
impl_pre_inserted_type_construct!(ast::PatternVar);
impl_pre_inserted_type_construct!(ast::LocalVar);

#[derive(Debug)]
struct UnifyingTypes {
    schemes: HashMap<ast::Construct, u::Scheme>,
    types: HashMap<ast::Construct, u::Type>,
    class_cons: HashMap<ast::NodeId<ast::ClassCon>, u::ClassCon>,
    instance_cons: HashMap<ast::NodeId<ast::InstanceCon>, u::InstanceCon>,
    instantiations: HashMap<ast::Construct, u::Instantiation>,
    tuple_con_schemes: HashMap<u32, u::Scheme>,
}

impl UnifyingTypes {
    fn new() -> Self {
        Self {
            schemes: HashMap::new(),
            types: HashMap::new(),
            class_cons: HashMap::new(),
            instance_cons: HashMap::new(),
            instantiations: HashMap::new(),
            tuple_con_schemes: HashMap::new(),
        }
    }
}

#[derive(Debug, new)]
struct ExprScope<'a> {
    scope: &'a mut u::Scope,
    return_ty: Option<u::Type>,
}

pub trait BuildSatisfactionError {
    fn recursion_limit_exceeded<E: u::KindEnvironment>(
        constraint: &u::Constraint,
        u_ctx: &mut u::Context<E>,
    ) -> Error;

    fn no_matching_instances<E: u::KindEnvironment>(
        constraint: &u::Constraint,
        premise: &u::Premise,
        u_ctx: &mut u::Context<E>,
    ) -> Error;

    fn required_for<E: u::KindEnvironment>(
        error: Error,
        constraint: &u::Constraint,
        u_ctx: &mut u::Context<E>,
    ) -> Error;
}

impl BuildSatisfactionError for Error {
    fn recursion_limit_exceeded<E: u::KindEnvironment>(
        constraint: &u::Constraint,
        u_ctx: &mut u::Context<E>,
    ) -> Error {
        Error::RecursionLimitExceeded(u_ctx.export(constraint))
    }

    fn no_matching_instances<E: u::KindEnvironment>(
        constraint: &u::Constraint,
        premise: &u::Premise,
        u_ctx: &mut u::Context<E>,
    ) -> Error {
        let constraint = u_ctx.export(constraint);
        let premise = u_ctx.export(premise);
        Error::NoMatchingInstances(constraint, premise)
    }

    fn required_for<E: u::KindEnvironment>(
        error: Error,
        constraint: &u::Constraint,
        u_ctx: &mut u::Context<E>,
    ) -> Error {
        error.required_for(u_ctx.export(constraint))
    }
}

impl BuildSatisfactionError for () {
    fn recursion_limit_exceeded<E: unification::KindEnvironment>(
        _: &unification::Constraint,
        _: &mut unification::Context<E>,
    ) -> Error {
        Error::BuildSatisfactionFailedAtThisScope
    }

    fn no_matching_instances<E: unification::KindEnvironment>(
        _: &unification::Constraint,
        _: &unification::Premise,
        _: &mut unification::Context<E>,
    ) -> Error {
        Error::BuildSatisfactionFailedAtThisScope
    }

    fn required_for<E: unification::KindEnvironment>(
        error: Error,
        _: &unification::Constraint,
        _: &mut unification::Context<E>,
    ) -> Error {
        error
    }
}
