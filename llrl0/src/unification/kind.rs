use super::Error;
use crate::ast;

pub type Var = super::Var<Kind>;

pub type Vars = super::Vars<Kind>;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
pub enum Kind {
    Var(Var),
    Type,
    Constraint,
    Satisfaction,
    Value,
    Macro,
    Fun(Vars, Var),
}

#[derive(Debug, Clone)]
pub struct Context {
    pool: Vec<Kind>,
}

impl Context {
    pub fn new() -> Self {
        let mut pool = vec![
            Kind::Type,         // 0
            Kind::Constraint,   // 1
            Kind::Satisfaction, // 2
            Kind::Value,        // 3
            Kind::Macro,        // 4
        ];
        (0..=15).for_each(|_| pool.push(Kind::Type)); // 5 + 0 ..= 5 + 15
        Self { pool }
    }

    fn alloc_var(&mut self) -> Var {
        let var = Var::from_index(self.pool.len());
        self.pool.push(Kind::Var(var));
        var
    }

    fn alloc_bind(&mut self, k: Kind) -> Var {
        match k {
            Kind::Var(var) => var,
            Kind::Type => Var::from_index(0),
            Kind::Constraint => Var::from_index(1),
            Kind::Satisfaction => Var::from_index(2),
            Kind::Value => Var::from_index(3),
            Kind::Macro => Var::from_index(4),
            _ => {
                let var = Var::from_index(self.pool.len());
                self.pool.push(k);
                var
            }
        }
    }

    fn alloc_binds(&mut self, mut ks: Vec<Kind>) -> Vars {
        if ks.len() <= 16 && ks.iter().all(|k| *k == Kind::Type) {
            Vars::from_index_count(5, ks.len())
        } else if ks.len() == 1 {
            self.alloc_bind(ks[0]).into()
        } else {
            let vars = Vars::from_index_count(self.pool.len(), ks.len());
            self.pool.append(&mut ks);
            vars
        }
    }

    pub fn new_var(&mut self) -> Kind {
        Kind::Var(self.alloc_var())
    }

    pub fn unify<A, B>(&mut self, a: A, b: B) -> Result<(), Error>
    where
        Self: Resolve<A> + Resolve<B>,
    {
        let a = self.resolve(a);
        let b = self.resolve(b);
        match (a, b) {
            (a, b) if a == b => Ok(()),
            (Kind::Var(var), k) | (k, Kind::Var(var)) => {
                if self.occurs_check(k, var).is_err() {
                    Err(Error::OccursCheckFailed)
                } else {
                    self.pool[var.index()] = k;
                    Ok(())
                }
            }
            (Kind::Fun(a_args, a_ret), Kind::Fun(b_args, b_ret)) => {
                if a_args.len() != b_args.len() {
                    return Err(Error::Mismatch);
                }
                for (a, b) in a_args.iter().zip(b_args.iter()) {
                    self.unify(a, b)?;
                }
                self.unify(a_ret, b_ret)
            }
            _ => Err(Error::Mismatch),
        }
    }

    fn dfs<K, F, E>(&mut self, kind: K, f: &mut F) -> Result<(), E>
    where
        Self: Resolve<K>,
        F: FnMut(Kind) -> Result<(), E>,
    {
        let kind = self.resolve(kind);
        match kind {
            Kind::Var(_)
            | Kind::Type
            | Kind::Constraint
            | Kind::Satisfaction
            | Kind::Value
            | Kind::Macro => {}
            Kind::Fun(args, ret) => {
                for arg in args.iter() {
                    self.dfs::<Var, _, _>(arg, f)?;
                }
                self.dfs::<Var, _, _>(ret, f)?;
            }
        }
        f(kind)
    }

    fn occurs_check<K>(&mut self, kind: K, var: Var) -> Result<(), ()>
    where
        Self: Resolve<K>,
    {
        self.dfs(kind, &mut |kind| match kind {
            Kind::Var(v) if v == var => Err(()),
            _ => Ok(()),
        })
    }

    pub fn default_vars<K>(&mut self, kind: K, default_kind: Kind) -> Result<(), Error>
    where
        Self: Resolve<K>,
    {
        match self.resolve(kind) {
            Kind::Var(var) => self.unify::<Var, Kind>(var, default_kind),
            Kind::Type => Ok(()),
            Kind::Constraint => Ok(()),
            Kind::Satisfaction => Ok(()),
            Kind::Value => Ok(()),
            Kind::Macro => Ok(()),
            Kind::Fun(args, ret) => {
                for arg in args.iter() {
                    self.default_vars::<Var>(arg, default_kind)?;
                }
                self.default_vars::<Var>(ret, default_kind)
            }
        }
    }

    pub fn import(&mut self, kind: &ast::Kind) -> Kind {
        match kind {
            ast::Kind::Use(ref use_) => match *use_.get_resolved() {},
            ast::Kind::Type => build_kind!(*self, type),
            ast::Kind::Constraint => build_kind!(*self, constraint),
            ast::Kind::Satisfaction => build_kind!(*self, satisfaction),
            ast::Kind::Value => build_kind!(*self, value),
            ast::Kind::Macro => build_kind!(*self, macro),
            ast::Kind::Fun(ref args, ref ret) => {
                let args = args.iter().map(|arg| self.import(arg)).collect::<Vec<_>>();
                let ret = self.import(ret);
                build_kind!(*self, (-> ...{args} {ret}))
            }
            ast::Kind::Error(e) => panic!("Found Kind::Error at import: {}", e),
        }
    }

    pub fn export<K, B>(&mut self, kind: K, builder: &mut B) -> B::Result
    where
        Self: Resolve<K>,
        B: ast::KindBuilder,
    {
        match self.resolve(kind) {
            Kind::Var(var) => build_kind!(*builder, (error {var.to_string()})),
            Kind::Type => build_kind!(*builder, type),
            Kind::Constraint => build_kind!(*builder, constraint),
            Kind::Satisfaction => build_kind!(*builder, satisfaction),
            Kind::Value => build_kind!(*builder, value),
            Kind::Macro => build_kind!(*builder, macro),
            Kind::Fun(args, ret) => {
                let args = args
                    .iter()
                    .map(|arg| self.export::<Var, _>(arg, builder))
                    .collect::<Vec<_>>();
                let ret = self.export::<Var, _>(ret, builder);
                build_kind!(*builder, (-> ...{args} {ret}))
            }
        }
    }
}

impl ast::KindBuilder for Context {
    type Result = Kind;

    fn new_use(&mut self, id: ast::NodeId<ast::Use<ast::KindUse>>) -> Self::Result {
        panic!("Unsupported kind: Use({})", id)
    }

    fn new_type(&mut self) -> Self::Result {
        Kind::Type
    }

    fn new_constraint(&mut self) -> Self::Result {
        Kind::Constraint
    }

    fn new_satisfaction(&mut self) -> Self::Result {
        Kind::Satisfaction
    }

    fn new_value(&mut self) -> Self::Result {
        Kind::Value
    }

    fn new_macro(&mut self) -> Self::Result {
        Kind::Macro
    }

    fn new_fun(&mut self, args: Vec<Self::Result>, ret: Self::Result) -> Self::Result {
        if args.is_empty() {
            ret
        } else {
            let args = self.alloc_binds(args);
            let ret = self.alloc_bind(ret);
            Kind::Fun(args, ret)
        }
    }

    fn new_error(&mut self, e: impl Into<String>) -> Self::Result {
        panic!("Unsupported kind: Error({})", e.into())
    }
}

pub trait Resolve<K> {
    fn resolve(&mut self, k: K) -> Kind;
    fn unresolve(&mut self, k: K) -> Var;
}

impl Resolve<Var> for Context {
    fn resolve(&mut self, var: Var) -> Kind {
        match self.pool[var.index()] {
            Kind::Var(v) if v != var => {
                let k = self.resolve(v);
                self.pool[var.index()] = k;
                k
            }
            k => k,
        }
    }

    fn unresolve(&mut self, var: Var) -> Var {
        var
    }
}

impl Resolve<Kind> for Context {
    fn resolve(&mut self, k: Kind) -> Kind {
        match k {
            Kind::Var(var) => self.resolve(var),
            k => k,
        }
    }

    fn unresolve(&mut self, k: Kind) -> Var {
        self.alloc_bind(k)
    }
}

impl<'a> Resolve<&'a ast::Kind> for Context {
    fn resolve(&mut self, k: &'a ast::Kind) -> Kind {
        self.import(k)
    }

    fn unresolve(&mut self, k: &'a ast::Kind) -> Var {
        let k = self.import(k);
        self.unresolve(k)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Ast;

    #[test]
    fn test_vars_uniqueness() {
        let mut ctx = Context::new();
        let a = ctx.alloc_var();
        let b = ctx.alloc_var();
        let c = ctx.alloc_var();
        assert_ne!(a, b);
        assert_ne!(b, c);
    }

    #[test]
    fn test_default_vars() {
        let mut ctx = Context::new();
        let a = ctx.new_var();
        let b = ctx.new_var();
        let c = ctx.new_var();
        let f = build_kind!(ctx, (-> type {a} {b}));
        let g = build_kind!(ctx, (-> {f} {c} type));
        assert!(ctx.default_vars(g, Kind::Type).is_ok());
        assert_eq!(ctx.resolve(a), Kind::Type);
        assert_eq!(ctx.resolve(b), Kind::Type);
        assert_eq!(ctx.resolve(c), Kind::Type);
    }

    #[test]
    fn test_import_export() {
        let mut ctx = Context::new();
        let a = build_kind!(Ast, (-> (-> type type) type constraint));
        let b = ctx.import(&a);
        let b = ctx.export(b, &mut Ast);
        assert_eq!(a, b);
    }

    #[test]
    fn test_unify() {
        let mut ctx = Context::new();

        assert_eq!(ctx.unify(Kind::Type, Kind::Type), Ok(()));
        assert_eq!(ctx.unify(Kind::Constraint, Kind::Constraint), Ok(()));
        assert_eq!(
            ctx.unify(Kind::Type, Kind::Constraint),
            Err(Error::Mismatch)
        );
        {
            let a = ctx.new_var();
            let b = ctx.new_var();
            let c = build_kind!(ctx, (-> {a} {b}));
            assert_eq!(ctx.unify(Kind::Type, c), Err(Error::Mismatch));
        }
        {
            let a = ctx.new_var();
            let b = ctx.new_var();
            let f = build_kind!(ctx, (-> type type {a}));
            let g = build_kind!(ctx, (-> {b} {a}));
            assert_eq!(ctx.unify(f, g), Err(Error::Mismatch));
        }
        {
            let x = ctx.new_var();
            let y = ctx.new_var();
            let f = build_kind!(ctx, (-> type {y}));
            let g = build_kind!(ctx, (-> {x} constraint));
            assert_eq!(ctx.unify(f, g), Ok(()));
            assert_eq!(ctx.resolve(x), Kind::Type);
            assert_eq!(ctx.resolve(y), Kind::Constraint);
        }
        {
            let a = ctx.new_var();
            let b = ctx.new_var();
            let c = ctx.new_var();
            let d = ctx.new_var();
            let f = build_kind!(ctx, (-> {a} {a}));
            let g = build_kind!(ctx, (-> {b} {c}));
            let h = build_kind!(ctx, (-> {d} type));
            assert_eq!(ctx.unify(f, g), Ok(()));
            assert_eq!(ctx.unify(g, h), Ok(()));
            assert_eq!(ctx.resolve(a), Kind::Type);
            assert_eq!(ctx.resolve(b), Kind::Type);
            assert_eq!(ctx.resolve(c), Kind::Type);
            assert_eq!(ctx.resolve(d), Kind::Type);
        }
        {
            let x = ctx.new_var();
            let y = ctx.new_var();
            let f = build_kind!(ctx, (-> {x} {y}));
            assert_eq!(ctx.unify(f, x), Err(Error::OccursCheckFailed));
            assert_eq!(ctx.unify(x, f), Err(Error::OccursCheckFailed));
        }
    }
}
