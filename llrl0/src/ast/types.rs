use super::*;
use crate::formatting::ContextualDisplay;
use crate::topological_sort;
use if_chain::if_chain;
use std::fmt;

/// In llrl, `Type` represents a type-level expression or the type of values itself.
///
/// Only a `Type` whose kind is `Kind::Type` can have a type-of relationship with values.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone)]
pub enum Type {
    Use(Use<TypeUse>),
    Con(TypeCon),
    App(Box<Type>, Vec<Type>),
    Gen(NodeId<TypeParameter>),
    Error(String),
}

impl Type {
    pub fn contains_error(&self) -> bool {
        self.dfs(|kind| match kind {
            Self::Error(_) => Err(()),
            _ => Ok(()),
        })
        .is_err()
    }

    pub fn use_(use_: impl Into<Use<TypeUse>>) -> Self {
        Self::Use(use_.into())
    }

    pub fn is_tuple(&self) -> bool {
        self.matches_tuple().is_some()
    }

    pub fn matches_tuple(&self) -> Option<&[Self]> {
        match self {
            Self::App(ref callee, ref args) => if_chain! {
                if let Self::Con(con) = callee.as_ref();
                if let Some(n) = con.matches_tuple();
                if n != 0 && n == args.len();
                then { Some(args.as_slice()) }
                else { None }
            },
            Self::Con(con) if con.matches_tuple() == Some(0) => Some(&[]),
            _ => None,
        }
    }

    pub fn is_fun(&self) -> bool {
        self.matches_fun().is_some()
    }

    pub fn matches_fun(&self) -> Option<(&[Self], &Self)> {
        if_chain! {
            if let Self::App(ref callee, ref args) = self;
            if let Self::Con(con) = callee.as_ref();
            if let Some(n) = con.matches_fun();
            if args.len() == n + 1;
            then { Some((&args[0..n], &args[n])) }
            else { None }
        }
    }

    pub fn app(callee: Self, args: Vec<Self>) -> Self {
        if args.is_empty() {
            callee
        } else {
            Self::App(Box::new(callee), args)
        }
    }

    pub fn error(e: impl Into<String>) -> Self {
        Self::Error(e.into())
    }
}

impl Dfs for Type {
    fn dfs_<E>(&self, f: &mut impl FnMut(&Self) -> Result<(), E>) -> Result<(), E> {
        match self {
            Self::Use(_) => {}
            Self::Con(_) => {}
            Self::App(ref callee, ref args) => {
                callee.dfs_(f)?;
                for arg in args.iter() {
                    arg.dfs_(f)?;
                }
            }
            Self::Gen(_) => {}
            Self::Error(_) => {}
        }
        f(self)
    }
}

impl<'a> topological_sort::DependencyList<NodeId<DataTypeCon>> for Type {
    fn traverse_dependencies(&self, f: &mut impl FnMut(&NodeId<DataTypeCon>)) {
        self.dfs_do(|ty| match ty {
            Type::Con(TypeCon::Data(id)) => f(id),
            _ => {}
        });
    }
}

pub trait TypeBuilder {
    type Result: Sized;

    fn new_use(&mut self, id: impl Into<Use<TypeUse>>) -> Self::Result;
    fn new_con(&mut self, con: TypeCon) -> Self::Result;
    fn new_app(&mut self, callee: Self::Result, args: Vec<Self::Result>) -> Self::Result;
    fn new_gen(&mut self, gen: NodeId<TypeParameter>) -> Self::Result;
    fn new_error(&mut self, e: impl Into<String>) -> Self::Result;

    fn new_data_con(&mut self, id: NodeId<DataTypeCon>) -> Self::Result {
        self.new_con(TypeCon::Data(id))
    }

    fn new_builtin_con(&mut self, id: NodeId<BuiltinTypeCon>) -> Self::Result {
        self.new_con(TypeCon::Builtin(id))
    }

    fn new_unit(&mut self) -> Self::Result {
        self.new_con(TypeCon::unit())
    }

    fn new_tuple(&mut self, tys: Vec<Self::Result>) -> Self::Result {
        if tys.len() == 0 {
            self.new_con(TypeCon::tuple(0))
        } else {
            let callee = self.new_con(TypeCon::tuple(tys.len()));
            self.new_app(callee, tys)
        }
    }

    fn new_fun(&mut self, mut args: Vec<Self::Result>, ret: Self::Result) -> Self::Result {
        let callee = self.new_con(TypeCon::fun(args.len()));
        args.push(ret);
        self.new_app(callee, args)
    }
}

impl TypeBuilder for Ast {
    type Result = Type;

    fn new_use(&mut self, id: impl Into<Use<TypeUse>>) -> Self::Result {
        Type::use_(id)
    }

    fn new_con(&mut self, con: TypeCon) -> Self::Result {
        Type::Con(con)
    }

    fn new_app(&mut self, callee: Self::Result, args: Vec<Self::Result>) -> Self::Result {
        Type::app(callee, args)
    }

    fn new_gen(&mut self, gen: NodeId<TypeParameter>) -> Self::Result {
        Type::Gen(gen)
    }

    fn new_error(&mut self, e: impl Into<String>) -> Self::Result {
        Type::error(e)
    }
}

#[allow(unused_macros)]
macro_rules! build_type_init {
    ($ctx:expr, $dest:expr, $a:tt) => {
    };

    ($ctx:expr, $dest:expr, $a:tt $( $b:tt )+) => {
        $dest.push(build_type!($ctx, $a));
        build_type_init!($ctx, $dest, $( $b )+)
    };
}

#[allow(unused_macros)]
macro_rules! build_type_last {
    ($ctx:expr, $a:tt) => {
        build_type!($ctx, $a)
    };

    ($ctx:expr, $a:tt $( $b:tt )+) => {
        build_type_last!($ctx, $( $b )+)
    };
}

macro_rules! build_type {
    ($ctx:expr, $expr:block) => {
        ($expr)
    };

    ($ctx:expr, (use $id:block)) => {
        $crate::ast::TypeBuilder::new_use(&mut $ctx, $id)
    };

    ($ctx:expr, (con $con:block)) => {
        $crate::ast::TypeBuilder::new_con(&mut $ctx, $con)
    };

    ($ctx:expr, (gen $gen:block)) => {
        $crate::ast::TypeBuilder::new_gen(&mut $ctx, $gen)
    };

    ($ctx:expr, (error $e:literal)) => {
        $crate::ast::TypeBuilder::new_error(&mut $ctx, $e)
    };

    ($ctx:expr, (error $e:block)) => {
        $crate::ast::TypeBuilder::new_error(&mut $ctx, $e)
    };

    ($ctx:expr, (data $con:block)) => {
        $crate::ast::TypeBuilder::new_data_con(&mut $ctx, $con)
    };

    ($ctx:expr, (builtin $con:block)) => {
        $crate::ast::TypeBuilder::new_builtin_con(&mut $ctx, $con)
    };

    ($ctx:expr, unit) => {
        $crate::ast::TypeBuilder::new_unit(&mut $ctx)
    };

    ($ctx:expr, (: ...$tys:block)) => {
        {
            let tys = $tys;
            $crate::ast::TypeBuilder::new_tuple(&mut $ctx, tys)
        }
    };

    ($ctx:expr, (: $( $ty:tt )+)) => {
        {
            let mut tys = Vec::new();
            $( tys.push(build_type!($ctx, $ty)); )*
            $crate::ast::TypeBuilder::new_tuple(&mut $ctx, tys)
        }
    };

    ($ctx:expr, (-> ...$args:block $ret:tt)) => {
        {
            let args = $args;
            let ret = build_type!($ctx, $ret);
            $crate::ast::TypeBuilder::new_fun(&mut $ctx, args, ret)
        }
    };

    ($ctx:expr, (-> $( $s:tt )+)) => {
        {
            let mut args = Vec::new();
            build_type_init!($ctx, args, $( $s )+);
            let ret = build_type_last!($ctx, $( $s )+);
            $crate::ast::TypeBuilder::new_fun(&mut $ctx, args, ret)
        }
    };

    ($ctx:expr, ($callee:tt ...$args:block)) => {
        {
            let callee = build_type!($ctx, $callee);
            let args = $args;
            $crate::ast::TypeBuilder::new_app(&mut $ctx, callee, args)
        }
    };

    ($ctx:expr, ($callee:tt $( $arg:tt )+)) => {
        {
            let callee = build_type!($ctx, $callee);
            let mut args = Vec::new();
            $( args.push(build_type!($ctx, $arg)); )*
            $crate::ast::TypeBuilder::new_app(&mut $ctx, callee, args)
        }
    };
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
pub enum TypeUse {} // directly overwritten as Type::Con | Type::Gen

impl fmt::Display for TypeUse {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {}
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
pub enum TypeCon {
    Data(NodeId<DataTypeCon>),
    Builtin(NodeId<BuiltinTypeCon>),
}

impl TypeCon {
    pub fn unit() -> Self {
        Self::Data(builtin::tuple_type(0))
    }

    pub fn fun(arity: usize) -> Self {
        Self::Builtin(builtin::fun(arity))
    }

    pub fn matches_fun(self) -> Option<usize> {
        match self {
            Self::Builtin(id) => builtin::matches_fun(id),
            _ => None,
        }
    }

    pub fn tuple(size: usize) -> Self {
        Self::Data(builtin::tuple_type(size))
    }

    pub fn matches_tuple(self) -> Option<usize> {
        match self {
            Self::Data(id) => builtin::matches_tuple_type(id),
            _ => None,
        }
    }

    pub const BOOL: Self = Self::Data(builtin::BOOL);

    pub const I8: Self = Self::Builtin(builtin::I8);
    pub const I16: Self = Self::Builtin(builtin::I16);
    pub const I32: Self = Self::Builtin(builtin::I32);
    pub const I64: Self = Self::Builtin(builtin::I64);

    pub const U8: Self = Self::Builtin(builtin::U8);
    pub const U16: Self = Self::Builtin(builtin::U16);
    pub const U32: Self = Self::Builtin(builtin::U32);
    pub const U64: Self = Self::Builtin(builtin::U64);

    pub const F32: Self = Self::Builtin(builtin::F32);
    pub const F64: Self = Self::Builtin(builtin::F64);

    pub const STRING: Self = Self::Builtin(builtin::STRING);

    pub const CHAR: Self = Self::Builtin(builtin::CHAR);

    pub const CAPTURED_USE: Self = Self::Builtin(builtin::CAPTURED_USE);

    pub const OPTION: Self = Self::Data(builtin::OPTION);

    pub const RESULT: Self = Self::Data(builtin::RESULT);

    pub const SYNTAX: Self = Self::Builtin(builtin::SYNTAX_TYPE);

    pub const SEXP: Self = Self::Data(builtin::SEXP);
}

impl From<NodeId<DataTypeCon>> for TypeCon {
    fn from(id: NodeId<DataTypeCon>) -> Self {
        Self::Data(id)
    }
}

impl From<NodeId<BuiltinTypeCon>> for TypeCon {
    fn from(id: NodeId<BuiltinTypeCon>) -> Self {
        Self::Builtin(id)
    }
}

impl TryFrom<Construct> for TypeCon {
    type Error = ();

    fn try_from(value: Construct) -> Result<Self, Self::Error> {
        match value {
            Construct::DataTypeCon(id) => Ok(id.into()),
            Construct::BuiltinTypeCon(id) => Ok(id.into()),
            _ => Err(()),
        }
    }
}

#[derive(PartialEq, Debug, Clone, new)]
pub struct Constraint {
    pub id: NodeId<Constraint>,
    pub rep: ConstraintRep,
}

impl Constraint {
    pub fn class(
        id: NodeId<Constraint>,
        class: impl Into<Use<NodeId<ClassCon>>>,
        class_args: Vec<Type>,
    ) -> Self {
        Self::new(id, ConstraintRep::class(class, class_args))
    }

    pub fn number(ty: Type) -> Self {
        Self::class(builtin::NUMBER_CONSTRAINT, builtin::NUMBER, vec![ty])
    }

    pub fn fpnumber(ty: Type) -> Self {
        Self::class(builtin::FPNUMBER_CONSTRAINT, builtin::FPNUMBER, vec![ty])
    }
}

impl<'a> topological_sort::DependencyList<NodeId<ClassCon>> for Constraint {
    fn traverse_dependencies(&self, f: &mut impl FnMut(&NodeId<ClassCon>)) {
        let ConstraintRep::Class(ref class, _) = self.rep;
        f(class.get_resolved())
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum ConstraintRep {
    Class(Use<NodeId<ClassCon>>, Vec<Type>),
}

impl ConstraintRep {
    pub fn class(class: impl Into<Use<NodeId<ClassCon>>>, class_args: Vec<Type>) -> Self {
        Self::Class(class.into(), class_args)
    }
}

#[derive(Debug, Clone)]
pub struct TypeParameter {
    pub id: NodeId<TypeParameter>,
    pub ann: Option<Annotation<Kind>>,
}

#[derive(Debug, Clone, new)]
pub struct Scheme {
    pub ty_params: Vec<TypeParameter>,
    pub s_params: Vec<Constraint>,
    pub body: Type,
}

impl Generic for Scheme {
    fn generic_types(&self) -> &[TypeParameter] {
        self.ty_params.as_slice()
    }

    fn generic_constraints(&self) -> &[Constraint] {
        self.s_params.as_slice()
    }
}

#[derive(Debug, Clone)]
pub enum Satisfaction {
    ByPremise(SatisfactionByPremise),
    ByInstance(SatisfactionByInstance),
    Error(String),
}

impl Satisfaction {
    pub fn contains_error(&self) -> bool {
        self.dfs(|s| match s {
            Self::Error(_) => Err(()),
            _ => Ok(()),
        })
        .is_err()
    }
}

impl Dfs for Satisfaction {
    fn dfs_<E>(&self, f: &mut impl FnMut(&Self) -> Result<(), E>) -> Result<(), E> {
        match self {
            Self::ByPremise(_) => {}
            Self::ByInstance(by_instance) => {
                for s in by_instance.instantiation.s_args.iter() {
                    s.dfs_(f)?;
                }
            }
            Self::Error(_) => {}
        }
        f(self)
    }
}

impl From<SatisfactionByPremise> for Satisfaction {
    fn from(by_premise: SatisfactionByPremise) -> Self {
        Self::ByPremise(by_premise)
    }
}

impl From<SatisfactionByInstance> for Satisfaction {
    fn from(by_instance: SatisfactionByInstance) -> Self {
        Self::ByInstance(by_instance)
    }
}

#[derive(Debug, Clone)]
pub struct SatisfactionByPremise {
    pub id: NodeId<Constraint>,
    pub path: Vec<NodeId<Constraint>>,
}

#[derive(Debug, Clone)]
pub struct SatisfactionByInstance {
    pub use_: Use<NodeId<InstanceCon>>,
    pub instantiation: Instantiation,
}

#[derive(Debug, Clone, new)]
pub struct Instantiation {
    pub ty_args: Vec<Type>,
    pub s_args: Vec<Satisfaction>,
}

pub trait TypeFormatter: KindFormatter {
    fn fmt_type_use(&self, f: &mut fmt::Formatter<'_>, id: Use<TypeUse>) -> fmt::Result;

    fn fmt_data_type_con(&self, f: &mut fmt::Formatter<'_>, id: NodeId<DataTypeCon>)
        -> fmt::Result;

    fn fmt_builtin_type_con(
        &self,
        f: &mut fmt::Formatter<'_>,
        id: NodeId<BuiltinTypeCon>,
    ) -> fmt::Result;

    fn fmt_type_parameter(
        &self,
        f: &mut fmt::Formatter<'_>,
        id: NodeId<TypeParameter>,
    ) -> fmt::Result;

    fn fmt_class_use(&self, f: &mut fmt::Formatter<'_>, id: Use<NodeId<ClassCon>>) -> fmt::Result;
}

impl<F: TypeFormatter> ContextualDisplay<F> for Type {
    fn fmt_with(&self, ctx: &F, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some((args, ret)) = self.matches_fun() {
            write!(f, "(->")?;
            for arg in args {
                write!(f, " {}", arg.fmt_on(ctx))?;
            }
            return write!(f, " {})", ret.fmt_on(ctx));
        }

        if let Some(tys) = self.matches_tuple() {
            if tys.is_empty() {
                return write!(f, "unit");
            }
            write!(f, "(:")?;
            for ty in tys {
                write!(f, " {}", ty.fmt_on(ctx))?;
            }
            return write!(f, ")");
        }

        match self {
            Self::Use(use_) => write!(f, "{}", use_.fmt_on(ctx)),
            Self::Con(con) => write!(f, "{}", con.fmt_on(ctx)),
            Self::App(callee, args) => {
                write!(f, "({}", callee.fmt_on(ctx))?;
                for arg in args.iter() {
                    write!(f, " {}", arg.fmt_on(ctx))?;
                }
                write!(f, ")")
            }
            Self::Gen(gen) => write!(f, "{}", gen.fmt_on(ctx)),
            Self::Error(e) => write!(f, "{}", e),
        }
    }
}

impl<F: TypeFormatter> ContextualDisplay<F> for Use<TypeUse> {
    fn fmt_with(&self, ctx: &F, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        ctx.fmt_type_use(f, *self)
    }
}

impl<F: TypeFormatter> ContextualDisplay<F> for TypeCon {
    fn fmt_with(&self, ctx: &F, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Data(id) => write!(f, "{}", id.fmt_on(ctx)),
            Self::Builtin(id) => write!(f, "{}", id.fmt_on(ctx)),
        }
    }
}

impl<F: TypeFormatter> ContextualDisplay<F> for NodeId<DataTypeCon> {
    fn fmt_with(&self, ctx: &F, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        ctx.fmt_data_type_con(f, *self)
    }
}

impl<F: TypeFormatter> ContextualDisplay<F> for NodeId<BuiltinTypeCon> {
    fn fmt_with(&self, ctx: &F, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        ctx.fmt_builtin_type_con(f, *self)
    }
}

impl<F: TypeFormatter> ContextualDisplay<F> for NodeId<TypeParameter> {
    fn fmt_with(&self, ctx: &F, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        ctx.fmt_type_parameter(f, *self)
    }
}

impl<F: TypeFormatter> ContextualDisplay<F> for Constraint {
    fn fmt_with(&self, ctx: &F, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ConstraintRep::Class(ref use_, ref args) = self.rep;
        if args.is_empty() {
            write!(f, "{}", use_.fmt_on(ctx))
        } else {
            write!(f, "({}", use_.fmt_on(ctx))?;
            for arg in args.iter() {
                write!(f, " {}", arg.fmt_on(ctx))?;
            }
            write!(f, ")")
        }
    }
}

impl<F: TypeFormatter> ContextualDisplay<F> for Use<NodeId<ClassCon>> {
    fn fmt_with(&self, ctx: &F, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        ctx.fmt_class_use(f, *self)
    }
}

impl<F: TypeFormatter> ContextualDisplay<F> for TypeParameter {
    fn fmt_with(&self, ctx: &F, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.ann {
            Some(ref kind) => {
                write!(f, "[{} {}]", self.id.fmt_on(ctx), kind.body.fmt_on(ctx))
            }
            None => write!(f, "{}", self.id.fmt_on(ctx)),
        }
    }
}

impl<F: TypeFormatter> ContextualDisplay<F> for Scheme {
    fn fmt_with(&self, ctx: &F, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{")?;
        if !self.ty_params.is_empty() {
            write!(f, "(forall")?;
            for tp in self.ty_params.iter() {
                write!(f, " {}", tp.fmt_on(ctx))?;
            }
            write!(f, ") ")?;
        }
        write!(f, "{}", self.body.fmt_on(ctx))?;
        if !self.s_params.is_empty() {
            write!(f, " (where")?;
            for c in self.s_params.iter() {
                write!(f, " {}", c.fmt_on(ctx))?;
            }
            write!(f, ")")?;
        }
        write!(f, "}}")
    }
}

#[cfg(test)]
mod tests {
    use super::super::ModuleId;
    use super::*;

    #[test]
    fn test_build() {
        build_type!(Ast, (use {NodeId::new_unchecked(ModuleId::from_index(1), 0)}));
        build_type!(Ast, (con {TypeCon::F32}));
        build_type!(Ast, (unit ...{Vec::new()}));
        build_type!(Ast, (unit unit unit));
        build_type!(Ast, (gen {NodeId::new_unchecked(ModuleId::from_index(1), 0)}));
        build_type!(Ast, (data {builtin::BOOL}));
        build_type!(Ast, (builtin {builtin::I32}));
        build_type!(Ast, unit);
        build_type!(Ast, (: ...{Vec::new()}));
        build_type!(Ast, (: unit {build_type!(Ast, unit)}));
        build_type!(Ast, (-> ...{Vec::new()} unit));
        build_type!(Ast, (-> unit unit unit));
    }
}
