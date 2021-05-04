use crate::ast;
use derive_new::new;
use std::collections::HashMap;

pub use crate::unification::constraint::*;
pub use crate::unification::types::*;
pub use crate::unification::{Error, Level};

pub fn gen_type_mapping(gens: &[Gen], tys: &[Type]) -> HashMap<Gen, Type> {
    gens.iter()
        .zip(tys)
        .map(|(gen, ty)| (*gen, *ty))
        .collect::<HashMap<_, _>>()
}

pub trait GenericTypes {
    fn generic_types(&self) -> &[Gen];

    fn apply_types<E: KindEnvironment>(&mut self, tys: &[Type], ctx: &mut Context<E>) {
        let map = gen_type_mapping(self.generic_types(), tys);
        self.subst_types(&map, ctx);
    }

    fn instantiate_types<E: KindEnvironment>(
        &mut self,
        level: Level,
        ctx: &mut Context<E>,
    ) -> Vec<Type> {
        let tys = self
            .generic_types()
            .iter()
            .map(|gen| ctx.instantiate_gen(*gen, level))
            .collect::<Vec<_>>();
        self.apply_types(tys.as_slice(), ctx);
        tys
    }

    fn subst_types<E: KindEnvironment>(&mut self, map: &HashMap<Gen, Type>, ctx: &mut Context<E>);
}

#[derive(Debug, Clone, new)]
pub struct Scheme {
    pub ty_params: Vec<Gen>,
    pub s_params: Vec<Constraint>,
    pub body: Type,
}

impl Scheme {
    pub fn types<'a>(&'a self) -> impl Iterator<Item = Type> + 'a {
        std::iter::once(self.body).chain(self.s_params.iter().flat_map(|c| c.types()))
    }

    pub fn types_mut(&mut self) -> impl Iterator<Item = &mut Type> {
        std::iter::once(&mut self.body).chain(self.s_params.iter_mut().flat_map(|c| c.types_mut()))
    }

    pub fn alpha_equal<E: KindEnvironment>(&self, other: &Scheme, ctx: &mut Context<E>) -> bool {
        if self.ty_params.len() != other.ty_params.len()
            || self.s_params.len() != other.s_params.len()
        {
            return false;
        }
        let map = self
            .ty_params
            .iter()
            .zip(other.ty_params.iter())
            .map(|(a, b)| (*a, Type::Gen(*b, Level::top())))
            .collect();

        ctx.equal_on_subst(&map, self.body, other.body)
            && self
                .s_params
                .iter()
                .zip(other.s_params.iter())
                .all(|(a, b)| a.equal_on_subst(b, &map, ctx))
    }
}

impl From<Type> for Scheme {
    fn from(body: Type) -> Self {
        Self::new(Vec::new(), Vec::new(), body)
    }
}

impl GenericTypes for Scheme {
    fn generic_types(&self) -> &[Gen] {
        self.ty_params.as_slice()
    }

    fn subst_types<E: KindEnvironment>(&mut self, map: &HashMap<Gen, Type>, ctx: &mut Context<E>) {
        if !map.is_empty() {
            for ty in self.types_mut() {
                *ty = ctx.subst(map, *ty);
            }
        }
    }
}

impl<E: KindEnvironment> Import<ast::Scheme> for Context<E> {
    type Dest = Scheme;

    fn import(&mut self, src: &ast::Scheme) -> Self::Dest {
        let ty_params = self.import(&src.ty_params);
        let s_params = self.import(&src.s_params);
        let body = self.import(&src.body);
        Scheme::new(ty_params, s_params, body)
    }
}

impl<E: KindEnvironment> Export<Scheme> for Context<E> {
    type Dest = ast::Scheme;

    fn export(&mut self, src: &Scheme) -> Self::Dest {
        let ty_params = self.export(&src.ty_params);
        let s_params = self.export(&src.s_params);
        let body = self.export(&src.body);
        ast::Scheme::new(ty_params, s_params, body)
    }
}

#[derive(Debug, Clone)]
pub struct ClassCon {
    pub ty_params: Vec<Gen>,
    pub superclasses: Vec<Constraint>,
}

impl ClassCon {
    pub fn types_mut(&mut self) -> impl Iterator<Item = &mut Type> {
        self.superclasses.iter_mut().flat_map(|c| c.types_mut())
    }
}

impl GenericTypes for ClassCon {
    fn generic_types(&self) -> &[Gen] {
        self.ty_params.as_slice()
    }

    fn subst_types<E: KindEnvironment>(&mut self, map: &HashMap<Gen, Type>, ctx: &mut Context<E>) {
        if !map.is_empty() {
            for ty in self.types_mut() {
                *ty = ctx.subst(map, *ty);
            }
        }
    }
}

impl<E: KindEnvironment> Import<ast::ClassCon> for Context<E> {
    type Dest = ClassCon;

    fn import(&mut self, src: &ast::ClassCon) -> Self::Dest {
        let ty_params = self.import(&src.ty_params);
        let superclasses = self.import(&src.superclasses);
        ClassCon {
            ty_params,
            superclasses,
        }
    }
}

#[derive(Debug, Clone)]
pub struct InstanceCon {
    pub ty_params: Vec<Gen>,
    pub s_params: Vec<Constraint>,
    pub target: Constraint,
}

impl InstanceCon {
    pub fn types_mut(&mut self) -> impl Iterator<Item = &mut Type> {
        self.target
            .types_mut()
            .chain(self.s_params.iter_mut().flat_map(|c| c.types_mut()))
    }
}

impl GenericTypes for InstanceCon {
    fn generic_types(&self) -> &[Gen] {
        self.ty_params.as_slice()
    }

    fn subst_types<E: KindEnvironment>(&mut self, map: &HashMap<Gen, Type>, ctx: &mut Context<E>) {
        if !map.is_empty() {
            for ty in self.types_mut() {
                *ty = ctx.subst(map, *ty);
            }
        }
    }
}

impl<E: KindEnvironment> Import<ast::InstanceCon> for Context<E> {
    type Dest = InstanceCon;

    fn import(&mut self, src: &ast::InstanceCon) -> Self::Dest {
        let ty_params = self.import(&src.ty_params);
        let s_params = self.import(&src.s_params);
        let target = self.import(&src.target);
        InstanceCon {
            ty_params,
            s_params,
            target,
        }
    }
}
