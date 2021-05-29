use super::{Error, Level};
use crate::ast::{self, Ast};
use crate::interning::{InternTable, Interned};
use std::borrow::Cow;
use std::collections::{BTreeSet, HashMap};

pub type Kind = ast::Kind;

pub type KindId = Interned<Kind>;

pub type Var = super::Var<Type>;

pub type Vars = super::Vars<Type>;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
pub enum Type {
    Var(Var, KindId, Level),
    Con(Con),
    App(Var, Vars),
    Gen(Gen, Level),
}

pub type Con = ast::TypeCon;

pub type Gen = ast::NodeId<ast::TypeParameter>;

pub trait KindEnvironment {
    fn resolve_kind(&self, construct: impl Into<ast::Construct>) -> Cow<Kind>;
}

#[derive(Debug, Clone)]
pub struct Context<E> {
    pool: Vec<Type>,
    pub kind_table: InternTable<Kind>,
    pub kind_env: E,
    kind_type_cache: KindId,
    con_kind_cache: HashMap<Con, KindId>,
    gen_kind_cache: HashMap<Gen, KindId>,
}

impl<E: KindEnvironment> Context<E> {
    pub fn new(kind_env: E) -> Self {
        let mut kind_table = InternTable::new();
        let kind_type_cache = kind_table.intern(&build_kind!(Ast, type));
        Self {
            pool: Vec::new(),
            kind_table,
            kind_env,
            kind_type_cache,
            con_kind_cache: HashMap::new(),
            gen_kind_cache: HashMap::new(),
        }
    }

    fn alloc_var(&mut self, kind: KindId, level: Level) -> Var {
        let var = Var::from_index(self.pool.len());
        self.pool.push(Type::Var(var, kind, level));
        var
    }

    fn alloc_bind(&mut self, t: Type) -> Var {
        match t {
            Type::Var(var, _, _) => var,
            _ => {
                let var = Var::from_index(self.pool.len());
                self.pool.push(t);
                var
            }
        }
    }

    fn alloc_binds(&mut self, mut ts: Vec<Type>) -> Vars {
        if ts.len() == 1 {
            self.alloc_bind(ts[0]).into()
        } else {
            let vars = Vars::from_index_count(self.pool.len(), ts.len());
            self.pool.append(&mut ts);
            vars
        }
    }

    pub fn new_var(&mut self, kind: KindId, level: Level) -> Type {
        let var = self.alloc_var(kind, level);
        Type::Var(var, kind, level)
    }

    pub fn new_type_var(&mut self, level: Level) -> Type {
        self.new_var(self.kind_type_cache, level)
    }

    pub fn instantiate_gen(&mut self, gen: Gen, level: Level) -> Type {
        let kind = self.get_kind_id(Type::Gen(gen, Level::top()));
        self.new_var(kind, level)
    }

    pub fn get_kind<T>(&mut self, t: T) -> &Kind
    where
        Self: Resolve<T>,
    {
        let id = self.get_kind_id(t);
        self.kind_table.review(id)
    }

    pub fn get_kind_id<T>(&mut self, t: T) -> KindId
    where
        Self: Resolve<T>,
    {
        match self.resolve(t) {
            Type::Var(_, kind, _) => kind,
            Type::Con(con) => {
                let kind_env = &self.kind_env;
                let kind_table = &mut self.kind_table;
                *self.con_kind_cache.entry(con).or_insert_with(|| {
                    let kind = match con {
                        Con::Data(id) => kind_env.resolve_kind(id),
                        Con::Builtin(id) => kind_env.resolve_kind(id),
                    };
                    kind_table.intern(kind.as_ref())
                })
            }
            Type::App(callee, _) => {
                let ret_kind = match self.get_kind::<Var>(callee) {
                    ast::Kind::Fun(_, ret) => ret.clone(),
                    _ => panic!("Kind mismatch"),
                };
                self.kind_table.intern(&ret_kind)
            }
            Type::Gen(gen, _) => {
                let kind_env = &self.kind_env;
                let kind_table = &mut self.kind_table;
                *self.gen_kind_cache.entry(gen).or_insert_with(|| {
                    let kind = kind_env.resolve_kind(gen);
                    kind_table.intern(kind.as_ref())
                })
            }
        }
    }

    pub fn unify<A, B>(&mut self, a: A, b: B) -> Result<(), Error>
    where
        Self: Resolve<A> + Resolve<B>,
    {
        let a = self.resolve(a);
        let b = self.resolve(b);
        match (a, b) {
            (a, b) if a == b => Ok(()),
            (Type::Var(var, k, lv), t) | (t, Type::Var(var, k, lv)) => {
                if self.occurs_check(t, var).is_err() {
                    Err(Error::OccursCheckFailed)
                } else if self.get_kind_id(t) != k {
                    Err(Error::Mismatch)
                } else {
                    self.pool[var.index()] = t;
                    self.adjust_level(t, lv)?;
                    Ok(())
                }
            }
            (Type::App(a_callee, a_args), Type::App(b_callee, b_args)) => {
                if a_args.len() != b_args.len() {
                    return Err(Error::Mismatch);
                }
                self.unify(a_callee, b_callee)?;
                for (a, b) in a_args.iter().zip(b_args.iter()) {
                    self.unify(a, b)?;
                }
                Ok(())
            }
            _ => Err(Error::Mismatch),
        }
    }

    pub fn match_to_right<A, B>(&mut self, a: A, b: B) -> Result<(), Error>
    where
        Self: Resolve<A> + Resolve<B>,
    {
        let a = self.resolve(a);
        let b = self.resolve(b);
        match (a, b) {
            (a, b) if a == b => Ok(()),
            (Type::Var(var, k, lv), t) => {
                // In fact, occurs check is not required for all use cases of `match_to_right`.
                // Add an option to skip occurs check for optimization?
                if self.occurs_check(t, var).is_err() {
                    Err(Error::OccursCheckFailed)
                } else if self.get_kind_id(t) != k {
                    Err(Error::Mismatch)
                } else {
                    self.pool[var.index()] = t;
                    self.deny_adjust_required_level(t, lv)?;
                    Ok(())
                }
            }
            (Type::App(a_callee, a_args), Type::App(b_callee, b_args)) => {
                if a_args.len() != b_args.len() {
                    return Err(Error::Mismatch);
                }
                self.match_to_right(a_callee, b_callee)?;
                for (a, b) in a_args.iter().zip(b_args.iter()) {
                    self.match_to_right(a, b)?;
                }
                Ok(())
            }
            _ => Err(Error::Mismatch),
        }
    }

    pub fn subst<T>(&mut self, map: &HashMap<Gen, Type>, ty: T) -> Type
    where
        Self: Resolve<T>,
    {
        match self.resolve(ty) {
            t @ Type::Var(_, _, _) => t,
            t @ Type::Con(_) => t,
            Type::App(callee, args) => {
                let callee = self.subst::<Var>(map, callee);
                let args = args
                    .iter()
                    .map(|arg| self.subst::<Var>(map, arg))
                    .collect::<Vec<_>>();
                build_type!(*self, ({callee} ...{args}))
            }
            Type::Gen(gen, level) => match map.get(&gen) {
                Some(ty) => *ty,
                None => Type::Gen(gen, level),
            },
        }
    }

    fn equal_on(
        &mut self,
        a: Type,
        b: Type,
        mut rec: impl FnMut(&mut Self, Var, Var) -> bool,
    ) -> bool {
        match (a, b) {
            (Type::Var(a, _, _), Type::Var(b, _, _)) => a == b,
            (Type::Con(a), Type::Con(b)) => a == b,
            (Type::App(a_callee, a_args), Type::App(b_callee, b_args)) => {
                a_args.len() == b_args.len()
                    && rec(self, a_callee, b_callee)
                    && a_args
                        .iter()
                        .zip(b_args.iter())
                        .all(|(a, b)| rec(self, a, b))
            }
            (Type::Gen(a, _), Type::Gen(b, _)) => a == b,
            _ => false,
        }
    }

    pub fn equal<A, B>(&mut self, a: A, b: B) -> bool
    where
        Self: Resolve<A> + Resolve<B>,
    {
        let a = self.resolve(a);
        let b = self.resolve(b);
        self.equal_on(a, b, |ctx, a, b| ctx.equal(a, b))
    }

    pub fn equal_on_subst<A, B>(&mut self, map: &HashMap<Gen, Type>, a: A, b: B) -> bool
    where
        Self: Resolve<A> + Resolve<B>,
    {
        let a = self.resolve(a);
        let b = self.resolve(b);
        if let Type::Gen(gen, _) = a {
            if let Some(a) = map.get(&gen) {
                return self.equal_on_subst(map, *a, b);
            }
        }
        self.equal_on(a, b, |ctx, a, b| ctx.equal_on_subst(map, a, b))
    }

    fn dfs<T, F, Err>(&mut self, ty: T, f: &mut F) -> Result<(), Err>
    where
        Self: Resolve<T>,
        F: FnMut(Type) -> Result<(), Err>,
    {
        let ty = self.resolve(ty);
        match ty {
            Type::Var(_, _, _) | Type::Con(_) | Type::Gen(_, _) => {}
            Type::App(callee, args) => {
                self.dfs::<Var, _, _>(callee, f)?;
                for arg in args.iter() {
                    self.dfs::<Var, _, _>(arg, f)?;
                }
            }
        }
        f(ty)
    }

    pub fn compute_shallowest_level<T>(&mut self, ty: T) -> Level
    where
        Self: Resolve<T>,
    {
        let mut level = Level::top();
        self.dfs(ty, &mut |ty| {
            match ty {
                Type::Var(_, _, lv) => level = level.max(lv),
                Type::Gen(_, lv) => level = level.max(lv),
                _ => {}
            }
            Ok::<(), ()>(())
        })
        .unwrap(); // .into_ok()
        level
    }

    pub fn get_vars<T>(&mut self, ty: T, min_level: Level) -> BTreeSet<Var>
    where
        Self: Resolve<T>,
    {
        let mut result = BTreeSet::new();
        self.collect_vars(ty, min_level, &mut result);
        result
    }

    pub fn collect_vars<T>(&mut self, ty: T, min_level: Level, result: &mut BTreeSet<Var>)
    where
        Self: Resolve<T>,
    {
        self.dfs(ty, &mut |ty| {
            if let Type::Var(var, _, level) = ty {
                if min_level <= level {
                    result.insert(var);
                }
            }
            Ok::<(), ()>(())
        })
        .unwrap(); // .into_ok()
    }

    pub fn default_vars<T>(&mut self, ty: T, default_type: Type) -> Result<(), Error>
    where
        Self: Resolve<T>,
    {
        match self.resolve(ty) {
            Type::Var(var, _, _) => self.unify::<Var, Type>(var, default_type),
            Type::Con(_) => Ok(()),
            Type::App(callee, args) => {
                self.default_vars::<Var>(callee, default_type)?;
                for arg in args.iter() {
                    self.default_vars::<Var>(arg, default_type)?;
                }
                Ok(())
            }
            Type::Gen(_, _) => Ok(()),
        }
    }

    fn occurs_check<T>(&mut self, ty: T, var: Var) -> Result<(), ()>
    where
        Self: Resolve<T>,
    {
        self.dfs(ty, &mut |ty| match ty {
            Type::Var(v, _, _) if v == var => Err(()),
            _ => Ok(()),
        })
    }

    fn adjust_level(&mut self, ty: Type, target_lv: Level) -> Result<(), Error> {
        let mut targets = Vec::new();
        self.dfs(ty, &mut |ty| match ty {
            Type::Var(var, k, lv) if target_lv < lv => {
                targets.push((var, k));
                Ok(())
            }
            Type::Gen(_, lv) if target_lv < lv => Err(Error::Mismatch),
            _ => Ok(()),
        })?;

        for (var, kind) in targets {
            self.pool[var.index()] = Type::Var(var, kind, target_lv);
        }
        Ok(())
    }

    fn deny_adjust_required_level(&mut self, ty: Type, target_lv: Level) -> Result<(), Error> {
        self.dfs(ty, &mut |ty| match ty {
            Type::Var(_, _, lv) if target_lv < lv => Err(Error::Mismatch),
            Type::Gen(_, lv) if target_lv < lv => Err(Error::Mismatch),
            _ => Ok(()),
        })
    }
}

impl<E: KindEnvironment> ast::TypeBuilder for Context<E> {
    type Result = Type;

    fn new_use(&mut self, id: impl Into<ast::Use<ast::TypeUse>>) -> Self::Result {
        panic!("Unsupported Type: Use({})", id.into())
    }

    fn new_con(&mut self, con: Con) -> Self::Result {
        Type::Con(con)
    }

    fn new_app(&mut self, callee: Self::Result, args: Vec<Self::Result>) -> Self::Result {
        if args.is_empty() {
            callee
        } else {
            let callee = self.alloc_bind(callee);
            let args = self.alloc_binds(args);
            Type::App(callee, args)
        }
    }

    fn new_gen(&mut self, gen: Gen) -> Self::Result {
        Type::Gen(gen, Level::top())
    }

    fn new_error(&mut self, e: impl Into<String>) -> Self::Result {
        panic!("Unsupported Type: Error({})", e.into())
    }
}

pub trait Resolve<T> {
    fn resolve(&mut self, t: T) -> Type;
}

impl<E: KindEnvironment> Resolve<Var> for Context<E> {
    fn resolve(&mut self, var: Var) -> Type {
        match self.pool[var.index()] {
            Type::Var(v, _, _) if v != var => {
                let t = self.resolve(v);
                self.pool[var.index()] = t;
                t
            }
            t => t,
        }
    }
}

impl<E: KindEnvironment> Resolve<Type> for Context<E> {
    fn resolve(&mut self, t: Type) -> Type {
        match t {
            Type::Var(var, _, _) => self.resolve(var),
            t => t,
        }
    }
}

pub trait Import<T: ?Sized> {
    type Dest;

    fn import(&mut self, src: &T) -> Self::Dest;
}

pub trait Export<T: ?Sized> {
    type Dest;

    fn export(&mut self, src: &T) -> Self::Dest;
}

impl<E, T> Export<&'_ T> for Context<E>
where
    Self: Export<T>,
{
    type Dest = <Self as Export<T>>::Dest;

    fn export(&mut self, src: &&'_ T) -> Self::Dest {
        self.export(*src)
    }
}

impl<E, T> Import<Box<T>> for Context<E>
where
    Self: Import<T>,
{
    type Dest = <Self as Import<T>>::Dest;

    fn import(&mut self, src: &Box<T>) -> Self::Dest {
        self.import(&**src)
    }
}

impl<E, T> Import<Option<T>> for Context<E>
where
    Self: Import<T>,
{
    type Dest = Option<<Self as Import<T>>::Dest>;

    fn import(&mut self, src: &Option<T>) -> Self::Dest {
        src.as_ref().map(|src| self.import(src))
    }
}

impl<E, T> Export<Option<T>> for Context<E>
where
    Self: Export<T>,
{
    type Dest = Option<<Self as Export<T>>::Dest>;

    fn export(&mut self, src: &Option<T>) -> Self::Dest {
        src.as_ref().map(|src| self.export(src))
    }
}

impl<E, T> Export<[T]> for Context<E>
where
    Self: Export<T>,
{
    type Dest = Vec<<Self as Export<T>>::Dest>;

    fn export(&mut self, src: &[T]) -> Self::Dest {
        src.iter().map(|src| self.export(src)).collect()
    }
}

impl<E, T> Import<Vec<T>> for Context<E>
where
    Self: Import<T>,
{
    type Dest = Vec<<Self as Import<T>>::Dest>;

    fn import(&mut self, src: &Vec<T>) -> Self::Dest {
        src.iter().map(|src| self.import(src)).collect()
    }
}

impl<E, T> Export<Vec<T>> for Context<E>
where
    Self: Export<T>,
{
    type Dest = Vec<<Self as Export<T>>::Dest>;

    fn export(&mut self, src: &Vec<T>) -> Self::Dest {
        src.iter().map(|src| self.export(src)).collect()
    }
}

impl<E, T> Import<ast::Annotation<T>> for Context<E>
where
    Self: Import<T>,
{
    type Dest = <Self as Import<T>>::Dest;

    fn import(&mut self, src: &ast::Annotation<T>) -> Self::Dest {
        self.import(&src.body)
    }
}

impl<E: KindEnvironment> Import<ast::TypeParameter> for Context<E> {
    type Dest = Gen;

    fn import(&mut self, src: &ast::TypeParameter) -> Self::Dest {
        debug_assert!({
            // Ensure that the kind of this type parameter is available
            self.get_kind_id(Type::Gen(src.id, Level::top()));
            true
        });
        src.id
    }
}

impl<E: KindEnvironment> Export<Gen> for Context<E> {
    type Dest = ast::TypeParameter;

    fn export(&mut self, src: &Gen) -> Self::Dest {
        ast::TypeParameter {
            id: *src,
            ann: None,
        }
    }
}

impl<E: KindEnvironment> Import<ast::Type> for Context<E> {
    type Dest = Type;

    fn import(&mut self, src: &ast::Type) -> Self::Dest {
        match src {
            ast::Type::Use(use_) => match *use_.get_resolved() {},
            ast::Type::Con(con) => build_type!(*self, (con {*con})),
            ast::Type::App(callee, ref args) => {
                let callee = self.import(callee);
                let args = self.import(args);
                build_type!(*self, ({callee} ...{args}))
            }
            ast::Type::Gen(gen) => build_type!(*self, (gen {*gen})),
            ast::Type::Error(e) => panic!("Found Type::Error at import: {}", e),
        }
    }
}

impl<E: KindEnvironment> Export<Type> for Context<E> {
    type Dest = ast::Type;

    fn export(&mut self, src: &Type) -> Self::Dest {
        match self.resolve(*src) {
            Type::Var(var, _, _) => build_type!(Ast, (error {var.to_string()})),
            Type::Con(con) => build_type!(Ast, (con { con })),
            Type::App(callee, args) => {
                let callee = self.export(&callee);
                let args = self.export(&args);
                build_type!(Ast, ({callee} ...{args}))
            }
            Type::Gen(gen, _) => build_type!(Ast, (gen { gen })),
        }
    }
}

impl<E: KindEnvironment> Export<Var> for Context<E> {
    type Dest = <Self as Export<Type>>::Dest;

    fn export(&mut self, src: &Var) -> Self::Dest {
        let src = self.resolve(*src);
        self.export(&src)
    }
}

impl<E: KindEnvironment> Export<Vars> for Context<E> {
    type Dest = Vec<<Self as Export<Var>>::Dest>;

    fn export(&mut self, src: &Vars) -> Self::Dest {
        src.iter().map(|src| self.export(&src)).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    struct TestEnv(HashMap<Gen, Kind>);

    impl KindEnvironment for TestEnv {
        fn resolve_kind(&self, construct: impl Into<ast::Construct>) -> Cow<Kind> {
            let construct = construct.into();
            match construct {
                ast::Construct::BuiltinTypeCon(id) => {
                    if let Some(n) = ast::builtin::matches_fun(id) {
                        Cow::Owned(build_kind!(Ast, (-> ...{vec![Kind::Type; n + 1]} type)))
                    } else {
                        panic!("Unknown construct: {}", construct)
                    }
                }
                ast::Construct::DataTypeCon(id) => {
                    if let Some(n) = ast::builtin::matches_tuple_type(id) {
                        if n == 0 {
                            Cow::Owned(Kind::Type)
                        } else {
                            Cow::Owned(build_kind!(Ast, (-> ...{vec![Kind::Type; n]} type)))
                        }
                    } else {
                        panic!("Unknown construct: {}", construct)
                    }
                }
                ast::Construct::TypeParameter(id) => Cow::Borrowed(&self.0[&id]),
                _ => panic!("Unknown construct: {}", construct),
            }
        }
    }

    fn new_context(gens: Vec<(Gen, Kind)>) -> Context<TestEnv> {
        Context::new(TestEnv(gens.into_iter().collect()))
    }

    #[test]
    fn test_level() {
        let a = Level::top();
        let b = a.down();
        let c = b.down();
        assert!(a <= a);
        assert!(a <= b);
        assert!(a <= c);
        assert!(!(b <= a));
        assert!(!(c <= b));
    }

    #[test]
    fn test_vars_uniqueness() {
        let mut ctx = new_context(vec![]);
        let k = ctx.kind_table.intern(&build_kind!(Ast, type));
        let a = ctx.alloc_var(k, Level::top());
        let b = ctx.alloc_var(k, Level::top());
        assert_ne!(a, b);
    }

    #[test]
    fn test_get_kind() {
        let mut id_gen = ast::NodeIdGenerator::in_module(ast::ModuleId::from_index(1));
        let gen_0 = id_gen.generate();

        let mut ctx = new_context(vec![(gen_0, build_kind!(Ast, (-> type type type)))]);
        let a = Type::Con(Con::unit());
        let b = Type::Gen(gen_0, Level::top());
        let c = ctx.new_type_var(Level::top());
        let d = build_type!(ctx, ({b} {a} {c}));

        assert_eq!(ctx.get_kind(a), &build_kind!(Ast, type));
        assert_eq!(ctx.get_kind(b), &build_kind!(Ast, (-> type type type)));
        assert_eq!(ctx.get_kind(c), &build_kind!(Ast, type));
        assert_eq!(ctx.get_kind(d), &build_kind!(Ast, type));
    }

    #[test]
    fn test_unify() {
        let mut id_gen = ast::NodeIdGenerator::in_module(ast::ModuleId::from_index(1));
        let gen_0 = id_gen.generate();

        let mut ctx = new_context(vec![(gen_0, build_kind!(Ast, type))]);
        {
            let a = ctx.new_type_var(Level::top());
            let b = ctx.new_type_var(Level::top());
            let f = build_type!(ctx, (: {a} {a}));
            let g = build_type!(ctx, (: (gen {gen_0}) {b}));
            assert_eq!(ctx.unify(f, g), Ok(()));
            assert_eq!(ctx.resolve(a), Type::Gen(gen_0, Level::top()));
            assert_eq!(ctx.resolve(b), Type::Gen(gen_0, Level::top()));
        }
        {
            let kind = ctx.kind_table.intern(&build_kind!(Ast, (-> type type)));
            let a = ctx.new_type_var(Level::top());
            let b = ctx.new_var(kind, Level::top());
            assert_eq!(ctx.unify(a, b), Err(Error::Mismatch));
        }
        {
            let x = ctx.new_type_var(Level::top());
            let y = ctx.new_type_var(Level::top());
            let f = build_type!(ctx, (-> {x} {y}));
            assert_eq!(ctx.unify(f, x), Err(Error::OccursCheckFailed));
            assert_eq!(ctx.unify(x, f), Err(Error::OccursCheckFailed));
        }
        {
            let a = ctx.new_type_var(Level::top());
            let b = ctx.new_type_var(Level::top().down());
            let c = ctx.new_type_var(Level::top().down().down());
            let d = ctx.new_type_var(Level::top().down());
            let f = build_type!(ctx, (: {a} {b} {c}));
            assert_eq!(ctx.unify(f, d), Ok(()));
            assert!(matches!(ctx.resolve(a), Type::Var(_, _, lv) if lv == Level::top()));
            assert!(matches!(ctx.resolve(b), Type::Var(_, _, lv) if lv == Level::top().down()));
            assert!(matches!(ctx.resolve(c), Type::Var(_, _, lv) if lv == Level::top().down()));
        }
        {
            let a = ctx.new_type_var(Level::top());
            let b = ctx.new_type_var(Level::top().down());
            assert_eq!(ctx.unify(a, b), Ok(()));
            assert!(matches!(ctx.resolve(a), Type::Var(_, _, lv) if lv == Level::top()));
            assert!(matches!(ctx.resolve(b), Type::Var(_, _, lv) if lv == Level::top()));
        }
        {
            let a = ctx.new_type_var(Level::top());
            let b = ctx.new_type_var(Level::top().down());
            assert_eq!(ctx.unify(b, a), Ok(()));
            assert!(matches!(ctx.resolve(a), Type::Var(_, _, lv) if lv == Level::top()));
            assert!(matches!(ctx.resolve(b), Type::Var(_, _, lv) if lv == Level::top()));
        }
        {
            let a = ctx.new_type_var(Level::top());
            let b = Type::Gen(gen_0, Level::top().down());
            assert_eq!(ctx.unify(a, b), Err(Error::Mismatch));
        }
        {
            let a = ctx.new_type_var(Level::top().down());
            let b = Type::Gen(gen_0, Level::top());
            assert_eq!(ctx.unify(a, b), Ok(()));
        }
    }

    #[test]
    fn test_match_to_right() {
        let mut id_gen = ast::NodeIdGenerator::in_module(ast::ModuleId::from_index(1));
        let gen_0 = id_gen.generate();

        let mut ctx = new_context(vec![(gen_0, build_kind!(Ast, type))]);

        {
            let a = Type::Gen(gen_0, Level::top());
            let b = Type::Gen(gen_0, Level::top());
            assert_eq!(ctx.match_to_right(a, b), Ok(()));
        }
        {
            let a = Type::Gen(gen_0, Level::top());
            let b = ctx.new_type_var(Level::top());
            assert_eq!(ctx.match_to_right(a, b), Err(Error::Mismatch));
        }
        {
            let a = ctx.new_type_var(Level::top());
            let b = Type::Gen(gen_0, Level::top());
            assert_eq!(ctx.match_to_right(a, b), Ok(()));
            assert_eq!(ctx.resolve(a), Type::Gen(gen_0, Level::top()));
        }
        {
            let a = ctx.new_type_var(Level::top());
            assert_eq!(ctx.match_to_right(a, a), Ok(()));
        }
        {
            let a = ctx.new_type_var(Level::top());
            let b = ctx.new_type_var(Level::top());
            assert_eq!(ctx.match_to_right(a, b), Ok(()));
            assert_eq!(ctx.resolve(a), b);
        }
        {
            let a = ctx.new_type_var(Level::top());
            let b = ctx.new_type_var(Level::top());
            let f = build_type!(ctx, (-> {a} {b}));
            let g = build_type!(ctx, (-> (gen {gen_0}) unit));
            assert_eq!(ctx.match_to_right(f, g), Ok(()));
            assert_eq!(ctx.resolve(a), Type::Gen(gen_0, Level::top()));
            assert_eq!(ctx.resolve(b), Type::Con(Con::unit()));
        }
        {
            let a = ctx.new_type_var(Level::top());
            let b = ctx.new_type_var(Level::top());
            let f = build_type!(ctx, (-> {a} unit));
            let g = build_type!(ctx, (-> (gen {gen_0}) {b}));
            assert_eq!(ctx.match_to_right(f, g), Err(Error::Mismatch));
        }
        {
            let a = ctx.new_type_var(Level::top());
            let b = ctx.new_type_var(Level::top().down());
            assert_eq!(ctx.match_to_right(a, b), Err(Error::Mismatch));
        }
        {
            let a = ctx.new_type_var(Level::top().down());
            let b = ctx.new_type_var(Level::top());
            assert_eq!(ctx.match_to_right(a, b), Ok(()));
            assert_eq!(ctx.resolve(a), b);
        }
        {
            let a = ctx.new_type_var(Level::top());
            let b = Type::Gen(gen_0, Level::top().down());
            assert_eq!(ctx.match_to_right(a, b), Err(Error::Mismatch));
        }
        {
            let a = ctx.new_type_var(Level::top().down());
            let b = Type::Gen(gen_0, Level::top());
            assert_eq!(ctx.match_to_right(a, b), Ok(()));
        }
    }

    #[test]
    fn test_equal() {
        let mut id_gen = ast::NodeIdGenerator::in_module(ast::ModuleId::from_index(1));
        let gen_0 = id_gen.generate();

        let mut ctx = new_context(vec![(gen_0, build_kind!(Ast, type))]);

        {
            let a = ctx.new_type_var(Level::top());
            let b = Type::Gen(gen_0, Level::top());
            assert!(!ctx.equal(a, b));
        }
        {
            let a = ctx.new_type_var(Level::top());
            let b = Type::Gen(gen_0, Level::top());
            assert!(ctx.unify(a, b).is_ok());
            assert!(ctx.equal(a, b));
        }
        {
            let a = ctx.new_type_var(Level::top());
            let b = ctx.new_type_var(Level::top());
            let c = ctx.new_type_var(Level::top());
            let d = Type::Gen(gen_0, Level::top());
            let f = build_type!(ctx, (: {a} {b}));
            let g = build_type!(ctx, (: {c} {c}));
            assert!(ctx.unify(a, b).is_ok());
            assert!(ctx.unify(b, c).is_ok());
            assert!(ctx.unify(c, d).is_ok());
            assert!(ctx.equal(f, g));
        }
    }

    #[test]
    fn test_subst() {
        let mut id_gen = ast::NodeIdGenerator::in_module(ast::ModuleId::from_index(1));
        let gen_0 = id_gen.generate();
        let gen_1 = id_gen.generate();

        let mut ctx = new_context(vec![
            (gen_0, build_kind!(Ast, type)),
            (gen_1, build_kind!(Ast, type)),
        ]);

        let a = ctx.new_type_var(Level::top());
        let b = ctx.new_type_var(Level::top());
        let f = build_type!(ctx, (-> (gen {gen_0}) (gen {gen_1}) (gen {gen_0})));
        let g = ctx.subst(&vec![(gen_0, a), (gen_1, b)].into_iter().collect(), f);
        let h = build_type!(ctx, (-> {a} {b} {a}));
        assert!(!ctx.equal(f, h));
        assert!(ctx.equal(g, h));
    }

    #[test]
    fn test_get_lowest_level() {
        let mut id_gen = ast::NodeIdGenerator::in_module(ast::ModuleId::from_index(1));
        let gen_0 = id_gen.generate();

        let mut ctx = new_context(vec![(gen_0, build_kind!(Ast, type))]);

        let a = ctx.new_type_var(Level::top());
        let b = ctx.new_type_var(Level::top().down());
        let c = ctx.new_type_var(Level::top().down().down());
        let f = build_type!(ctx, (-> {a} {b} {c}));
        let g = Type::Gen(gen_0, Level::top().down());
        assert_eq!(ctx.compute_shallowest_level(f), Level::top().down().down());
        assert_eq!(ctx.compute_shallowest_level(g), Level::top().down());
    }

    #[test]
    fn test_enumerate_vars() {
        let mut ctx = new_context(Vec::new());
        let a = ctx.new_type_var(Level::top());
        let b = ctx.new_type_var(Level::top().down());
        let c = ctx.new_type_var(Level::top().down().down());
        let f = build_type!(ctx, (-> {a} {b}));
        let g = build_type!(ctx, (-> {f} {c} unit));
        assert_eq!(
            ctx.get_vars(g, Level::top().down()),
            vec![b, c]
                .into_iter()
                .map(|ty| ctx.alloc_bind(ty))
                .collect()
        );
    }
}
