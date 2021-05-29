use super::{types::*, Error, Level};
use crate::ast::{self, builtin};
use derive_new::new;
use if_chain::if_chain;
use std::cell::RefCell;
use std::collections::{BTreeSet, HashMap};
use std::rc::Rc;

pub type ConstraintId = ast::NodeId<ast::Constraint>;

#[derive(Debug, Clone, new)]
pub struct Constraint {
    pub id: ConstraintId,
    pub rep: ConstraintRep,
}

impl Constraint {
    pub fn class(
        id: ConstraintId,
        class: ast::NodeId<ast::ClassCon>,
        class_args: Vec<Type>,
    ) -> Self {
        Self::new(id, ConstraintRep::Class(class, class_args))
    }

    pub fn number(ty: Type) -> Self {
        Self::class(builtin::NUMBER_CONSTRAINT, builtin::NUMBER, vec![ty])
    }

    pub fn fpnumber(ty: Type) -> Self {
        Self::class(builtin::FPNUMBER_CONSTRAINT, builtin::FPNUMBER, vec![ty])
    }

    pub fn matches_number(&self) -> Option<Type> {
        if_chain! {
            let ConstraintRep::Class(class, ref args) = self.rep;
            if class == builtin::NUMBER;
            if let [ty] = args.as_slice();
            then { Some(*ty) }
            else { None }
        }
    }

    pub fn matches_fpnumber(&self) -> Option<Type> {
        if_chain! {
            let ConstraintRep::Class(class, ref args) = self.rep;
            if class == builtin::FPNUMBER;
            if let [ty] = args.as_slice();
            then { Some(*ty) }
            else { None }
        }
    }

    pub fn types<'a>(&'a self) -> impl Iterator<Item = Type> + 'a {
        let ConstraintRep::Class(_, ref class_args) = self.rep;
        class_args.iter().copied()
    }

    pub fn types_mut(&mut self) -> impl Iterator<Item = &mut Type> {
        let ConstraintRep::Class(_, ref mut class_args) = self.rep;
        class_args.iter_mut()
    }

    pub fn unify_types<E: KindEnvironment>(
        &self,
        other: &Self,
        ctx: &mut Context<E>,
    ) -> Result<(), Error> {
        debug_assert!(matches!(
            (&self.rep, &other.rep),
            (ConstraintRep::Class(a, _), ConstraintRep::Class(b, _)) if a == b
        ));
        for (a, b) in self.types().zip(other.types()) {
            ctx.unify(a, b)?;
        }

        Ok(())
    }

    pub fn match_types_to_right<E: KindEnvironment>(
        &self,
        other: &Self,
        ctx: &mut Context<E>,
    ) -> Result<(), Error> {
        debug_assert!(matches!(
            (&self.rep, &other.rep),
            (ConstraintRep::Class(a, _), ConstraintRep::Class(b, _)) if a == b
        ));
        for (a, b) in self.types().zip(other.types()) {
            ctx.match_to_right(a, b)?;
        }
        Ok(())
    }

    pub fn equal<E: KindEnvironment>(&self, other: &Self, ctx: &mut Context<E>) -> bool {
        let ConstraintRep::Class(a, _) = self.rep;
        let ConstraintRep::Class(b, _) = other.rep;

        a == b
            && self
                .types()
                .zip(other.types())
                .all(|(a, b)| ctx.equal(a, b))
    }

    pub fn equal_on_subst<E: KindEnvironment>(
        &self,
        other: &Self,
        map: &HashMap<Gen, Type>,
        ctx: &mut Context<E>,
    ) -> bool {
        let ConstraintRep::Class(a, _) = self.rep;
        let ConstraintRep::Class(b, _) = other.rep;

        a == b
            && self
                .types()
                .zip(other.types())
                .all(|(a, b)| ctx.equal_on_subst(map, a, b))
    }

    pub fn compute_shallowest_level<E: KindEnvironment>(&self, ctx: &mut Context<E>) -> Level {
        self.types()
            .map(|ty| ctx.compute_shallowest_level(ty))
            .fold(Level::top(), std::cmp::max)
    }

    pub fn enumerate_vars<E: KindEnvironment>(
        &self,
        min_level: Level,
        ctx: &mut Context<E>,
    ) -> BTreeSet<Var> {
        let mut result = BTreeSet::new();
        for ty in self.types() {
            ctx.collect_vars(ty, min_level, &mut result);
        }
        result
    }
}

impl<E: KindEnvironment> Import<ast::Constraint> for Context<E> {
    type Dest = Constraint;

    fn import(&mut self, src: &ast::Constraint) -> Self::Dest {
        Constraint::new(src.id, self.import(&src.rep))
    }
}

impl<E: KindEnvironment> Export<Constraint> for Context<E> {
    type Dest = ast::Constraint;

    fn export(&mut self, src: &Constraint) -> Self::Dest {
        ast::Constraint::new(src.id, self.export(&src.rep))
    }
}

#[derive(Debug, Clone)]
pub enum ConstraintRep {
    Class(ast::NodeId<ast::ClassCon>, Vec<Type>),
}

impl<E: KindEnvironment> Import<ast::ConstraintRep> for Context<E> {
    type Dest = ConstraintRep;

    fn import(&mut self, src: &ast::ConstraintRep) -> Self::Dest {
        let ast::ConstraintRep::Class(class, class_args) = src;
        ConstraintRep::Class(*class.get_resolved(), self.import(class_args))
    }
}

impl<E: KindEnvironment> Export<ConstraintRep> for Context<E> {
    type Dest = ast::ConstraintRep;

    fn export(&mut self, src: &ConstraintRep) -> Self::Dest {
        let ConstraintRep::Class(class, class_args) = src;
        ast::ConstraintRep::class(*class, self.export(class_args))
    }
}

#[derive(Debug)]
pub struct Scope {
    level: Level,
    context_constraints: Vec<ContextConstraint>,
    context_premise: Premise,
    scoped_gen_types: HashMap<Gen, Type>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            level: Level::top(),
            context_constraints: Vec::new(),
            context_premise: Premise::new(),
            scoped_gen_types: HashMap::new(),
        }
    }

    pub fn new_inner(outer_scope: &Scope) -> Self {
        Self {
            level: outer_scope.level.down(),
            context_constraints: Vec::new(),
            context_premise: outer_scope.context_premise.clone(),
            scoped_gen_types: outer_scope.scoped_gen_types.clone(),
        }
    }

    pub fn level(&self) -> Level {
        self.level
    }

    pub fn put_context_constraint(&mut self, constraint: Constraint) -> Satisfaction {
        let (constraint, satisfaction) = ContextConstraint::new(constraint);
        self.context_constraints.push(constraint);
        satisfaction
    }

    pub fn inherit_deferred_context_constraint(&mut self, constraint: ContextConstraint) {
        self.context_constraints.push(constraint);
    }

    pub fn consume_context_constraints(&mut self) -> Vec<ContextConstraint> {
        std::mem::take(&mut self.context_constraints)
    }

    pub fn context_premise(&self) -> &Premise {
        &self.context_premise
    }

    pub fn context_premise_mut(&mut self) -> &mut Premise {
        &mut self.context_premise
    }

    pub fn scoped_gen_types(&self) -> &HashMap<Gen, Type> {
        &self.scoped_gen_types
    }

    pub fn register_scoped_gen_types(&mut self, gens: &[Gen]) {
        for gen in gens.iter() {
            self.scoped_gen_types
                .insert(*gen, Type::Gen(*gen, self.level));
        }
    }
}

#[derive(Debug)]
pub struct ContextConstraint {
    constraint: Constraint,
    var: Rc<RefCell<Option<Satisfaction>>>,
}

impl ContextConstraint {
    pub fn new(constraint: Constraint) -> (Self, Satisfaction) {
        let var = Rc::new(RefCell::new(None));
        let satisfaction = Satisfaction::Var(Rc::clone(&var));
        let constraint = Self { constraint, var };
        (constraint, satisfaction)
    }

    pub fn body(&self) -> &Constraint {
        &self.constraint
    }

    pub fn resolve_by(self, satisfaction: Satisfaction) {
        *self.var.borrow_mut() = Some(satisfaction);
    }

    pub fn into_premise(self, constraint_id: ConstraintId) -> Constraint {
        let constraint = Constraint::new(constraint_id, self.constraint.rep);
        *self.var.borrow_mut() = Some(Satisfaction::by_premise(constraint.id, Vec::new()));
        constraint
    }
}

#[derive(Debug, Clone)]
pub struct Premise {
    class_premises: HashMap<ast::NodeId<ast::ClassCon>, Vec<ClassPremise>>,
}

impl Premise {
    pub fn new() -> Self {
        Self {
            class_premises: HashMap::new(),
        }
    }

    pub fn add_class_premise(
        &mut self,
        id: ConstraintId,
        path: Vec<ConstraintId>,
        class: ast::NodeId<ast::ClassCon>,
        class_args: Vec<Type>,
    ) {
        self.class_premises
            .entry(class)
            .or_default()
            .push(ClassPremise {
                id,
                path,
                class_args,
            });
    }

    pub fn class_premises(&self, class: ast::NodeId<ast::ClassCon>) -> &[ClassPremise] {
        self.class_premises
            .get(&class)
            .map_or(&[], |cs| cs.as_slice())
    }
}

impl<E: KindEnvironment> Export<Premise> for Context<E> {
    type Dest = Vec<ast::Constraint>;

    fn export(&mut self, src: &Premise) -> Self::Dest {
        let mut result = Vec::new();
        for (class, premises) in src.class_premises.iter() {
            for premise in premises {
                if premise.path.is_empty() {
                    result.push(ast::Constraint::class(
                        premise.id,
                        *class,
                        self.export(&premise.class_args),
                    ));
                }
            }
        }
        result
    }
}

#[derive(Debug, Clone)]
pub struct ClassPremise {
    id: ConstraintId,
    path: Vec<ConstraintId>,
    class_args: Vec<Type>,
}

impl ClassPremise {
    pub fn class_args(&self) -> &[Type] {
        self.class_args.as_slice()
    }

    pub fn to_satisfaction(&self) -> Satisfaction {
        Satisfaction::by_premise(self.id, self.path.clone())
    }

    pub fn has_equivalent_class_args<E: KindEnvironment>(
        &self,
        class_args: &[Type],
        ctx: &mut Context<E>,
    ) -> bool {
        self.class_args()
            .iter()
            .zip(class_args)
            .all(|(a, b)| ctx.equal(*a, *b))
    }
}

#[derive(Debug, Clone)]
pub enum Satisfaction {
    Var(Rc<RefCell<Option<Satisfaction>>>),
    ByPremise(SatisfactionByPremise),
    ByInstance(SatisfactionByInstance),
}

impl Satisfaction {
    pub fn by_premise(id: ConstraintId, path: Vec<ConstraintId>) -> Self {
        Self::ByPremise(SatisfactionByPremise { id, path })
    }

    pub fn by_instance(use_: ast::NodeId<ast::InstanceCon>, instantiation: Instantiation) -> Self {
        Self::ByInstance(SatisfactionByInstance {
            use_,
            instantiation,
        })
    }
}

impl<E: KindEnvironment> Export<Satisfaction> for Context<E> {
    type Dest = ast::Satisfaction;

    fn export(&mut self, src: &Satisfaction) -> Self::Dest {
        match src {
            Satisfaction::Var(var) => match *var.borrow() {
                Some(ref src) => self.export(src),
                None => ast::Satisfaction::Error("???".to_string()),
            },
            Satisfaction::ByPremise(by_premise) => self.export(by_premise).into(),
            Satisfaction::ByInstance(by_instance) => self.export(by_instance).into(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SatisfactionByPremise {
    pub id: ConstraintId,
    pub path: Vec<ConstraintId>,
}

impl<E: KindEnvironment> Export<SatisfactionByPremise> for Context<E> {
    type Dest = ast::SatisfactionByPremise;

    fn export(&mut self, src: &SatisfactionByPremise) -> Self::Dest {
        ast::SatisfactionByPremise {
            id: src.id,
            path: src.path.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SatisfactionByInstance {
    pub use_: ast::NodeId<ast::InstanceCon>,
    pub instantiation: Instantiation,
}

impl<E: KindEnvironment> Export<SatisfactionByInstance> for Context<E> {
    type Dest = ast::SatisfactionByInstance;

    fn export(&mut self, src: &SatisfactionByInstance) -> Self::Dest {
        let instantiation = self.export(&src.instantiation);
        ast::SatisfactionByInstance {
            use_: src.use_.into(),
            instantiation,
        }
    }
}

#[derive(Debug, Clone, new)]
pub struct Instantiation {
    pub ty_args: Vec<Type>,
    pub s_args: Vec<Satisfaction>,
}

impl Instantiation {
    pub fn types<'a>(&'a self) -> impl Iterator<Item = Type> + 'a {
        self.ty_args.iter().copied()
    }
}

impl<E: KindEnvironment> Export<Instantiation> for Context<E> {
    type Dest = ast::Instantiation;

    fn export(&mut self, src: &Instantiation) -> Self::Dest {
        let ty_args = self.export(&src.ty_args);
        let s_args = self.export(&src.s_args);
        ast::Instantiation::new(ty_args, s_args)
    }
}
