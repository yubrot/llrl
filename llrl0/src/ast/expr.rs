use super::*;
use crate::formatting::ContextualDisplay;
use crate::topological_sort;
use either::*;
use std::convert::TryFrom;
use std::fmt;

/// In llrl, `Expr` represents a value-level expression.
#[derive(Debug, Clone, new)]
pub struct Expr {
    pub id: NodeId<Expr>,
    pub rep: ExprRep,
}

impl Expr {
    pub fn same_level(&self) -> ExprDfsSameLevel {
        ExprDfsSameLevel { expr: self }
    }
}

impl Dfs for Expr {
    fn dfs_<E>(&self, f: &mut impl FnMut(&Self) -> Result<(), E>) -> Result<(), E> {
        match self.rep {
            ExprRep::Use(_) => {}
            ExprRep::Con(_) => {}
            ExprRep::Const(_) => {}
            ExprRep::App(ref app) => {
                app.callee.dfs_(f)?;
                for arg in app.args.iter() {
                    arg.dfs_(f)?;
                }
            }
            ExprRep::Capture(_) => {}
            ExprRep::Annotate(ref annotate) => {
                annotate.body.dfs_(f)?;
            }
            ExprRep::Let(ref let_) => {
                for def in let_.defs.iter() {
                    match def {
                        Left(local_var) => local_var.init.dfs_(f)?,
                        Right(local_function) => local_function.body.dfs_(f)?,
                    }
                }
                let_.body.dfs_(f)?;
            }
            ExprRep::Seq(ref seq) => {
                for stmt in seq.stmts.iter() {
                    stmt.dfs_(f)?;
                }
                seq.ret.dfs_(f)?;
            }
            ExprRep::If(ref if_) => {
                if_.cond.dfs_(f)?;
                if_.then.dfs_(f)?;
                if_.else_.dfs_(f)?;
            }
            ExprRep::While(ref while_) => {
                while_.cond.dfs_(f)?;
                while_.body.dfs_(f)?;
            }
            ExprRep::Match(ref match_) => {
                match_.target.dfs_(f)?;
                for (_, body) in match_.clauses.iter() {
                    body.dfs_(f)?;
                }
            }
            ExprRep::Return(ref e) => {
                for e in e.iter() {
                    e.dfs_(f)?;
                }
            }
        }
        f(self)
    }
}

pub struct ExprDfsSameLevel<'a> {
    pub expr: &'a Expr,
}

impl<'a> Dfs for ExprDfsSameLevel<'a> {
    fn dfs_<E>(&self, f: &mut impl FnMut(&Self) -> Result<(), E>) -> Result<(), E> {
        match self.expr.rep {
            ExprRep::Use(_) => {}
            ExprRep::Con(_) => {}
            ExprRep::Const(_) => {}
            ExprRep::App(ref app) => {
                app.callee.same_level().dfs_(f)?;
                for arg in app.args.iter() {
                    arg.same_level().dfs_(f)?;
                }
            }
            ExprRep::Capture(_) => {}
            ExprRep::Annotate(ref annotate) => {
                annotate.body.same_level().dfs_(f)?;
            }
            ExprRep::Let(ref let_) => {
                for local_var in let_.local_vars() {
                    local_var.init.same_level().dfs_(f)?;
                }
                let_.body.same_level().dfs_(f)?;
            }
            ExprRep::Seq(ref seq) => {
                for stmt in seq.stmts.iter() {
                    stmt.same_level().dfs_(f)?;
                }
                seq.ret.same_level().dfs_(f)?;
            }
            ExprRep::If(ref if_) => {
                if_.cond.same_level().dfs_(f)?;
                if_.then.same_level().dfs_(f)?;
                if_.else_.same_level().dfs_(f)?;
            }
            ExprRep::While(ref while_) => {
                while_.cond.same_level().dfs_(f)?;
                while_.body.same_level().dfs_(f)?;
            }
            ExprRep::Match(ref match_) => {
                match_.target.same_level().dfs_(f)?;
                for (_, body) in match_.clauses.iter() {
                    body.same_level().dfs_(f)?;
                }
            }
            ExprRep::Return(ref e) => {
                for e in e.iter() {
                    e.same_level().dfs_(f)?;
                }
            }
        }
        f(self)
    }
}

impl<'a> topological_sort::DependencyList<NodeId<Function>> for Expr {
    fn traverse_dependencies(&self, f: &mut impl FnMut(&NodeId<Function>)) {
        self.dfs_do(|e| match e.rep {
            ExprRep::Use(Use::Resolved(Value::Function(id), _)) => f(&id),
            _ => {}
        });
    }
}

impl<'a> topological_sort::DependencyList<NodeId<LocalFunction>> for Expr {
    fn traverse_dependencies(&self, f: &mut impl FnMut(&NodeId<LocalFunction>)) {
        self.dfs_do(|e| match e.rep {
            ExprRep::Use(Use::Resolved(Value::LocalFunction(id), _)) => f(&id),
            _ => {}
        });
    }
}

#[derive(Debug, Clone)]
pub enum ExprRep {
    Use(Use<Value>),
    Con(ValueCon),
    Const(Const),
    App(Box<ExprApply>),
    Capture(Use<Construct>),
    Annotate(Box<ExprAnnotate>),
    Let(Box<ExprLet>),
    Seq(Box<ExprSeq>),
    If(Box<ExprIf>),
    While(Box<ExprWhile>),
    Match(Box<ExprMatch>),
    Return(Box<Option<Expr>>),
}

impl ExprRep {
    pub fn use_(use_: impl Into<Use<Value>>) -> Self {
        Self::Use(use_.into())
    }

    pub fn unit() -> Self {
        Self::Con(ValueCon::unit())
    }

    pub fn tuple_con(size: usize) -> Self {
        Self::Con(ValueCon::tuple(size))
    }

    pub fn bool(value: bool) -> Self {
        Self::Con(ValueCon::bool(value))
    }

    pub fn app(callee: Expr, args: Vec<Expr>) -> Self {
        Self::App(Box::new(ExprApply { callee, args }))
    }

    pub fn capture(use_: impl Into<Use<Construct>>) -> Self {
        Self::Capture(use_.into())
    }

    pub fn annotate(body: Expr, ty: Annotation<Type>) -> Self {
        Self::Annotate(Box::new(ExprAnnotate { body, ty }))
    }

    pub fn let_(defs: Vec<Either<LocalVar, LocalFunction>>, body: Expr) -> Self {
        Self::Let(Box::new(ExprLet { defs, body }))
    }

    pub fn seq(stmts: Vec<Expr>, ret: Expr) -> Self {
        Self::Seq(Box::new(ExprSeq { stmts, ret }))
    }

    pub fn if_(cond: Expr, then: Expr, else_: Expr) -> Self {
        Self::If(Box::new(ExprIf { cond, then, else_ }))
    }

    pub fn while_(cond: Expr, body: Expr) -> Self {
        Self::While(Box::new(ExprWhile { cond, body }))
    }

    pub fn match_(target: Expr, clauses: Vec<(Pattern, Expr)>) -> Self {
        Self::Match(Box::new(ExprMatch { target, clauses }))
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
pub enum Value {
    Function(NodeId<Function>),
    CFunction(NodeId<CFunction>),
    BuiltinOp(NodeId<BuiltinOp>),
    ClassMethod(NodeId<ClassMethod>),
    Parameter(NodeId<Parameter>),
    LocalVar(NodeId<LocalVar>),
    LocalFunction(NodeId<LocalFunction>),
    PatternVar(NodeId<PatternVar>),
    // Con(ValueCon), // directly overwritten as ExprRep::Con
}

impl Value {
    pub fn is_static(self) -> bool {
        matches!(
            self,
            Self::Function(_) | Self::CFunction(_) | Self::BuiltinOp(_) | Self::ClassMethod(_)
        )
    }
}

impl From<NodeId<Function>> for Value {
    fn from(id: NodeId<Function>) -> Self {
        Self::Function(id)
    }
}

impl From<NodeId<CFunction>> for Value {
    fn from(id: NodeId<CFunction>) -> Self {
        Self::CFunction(id)
    }
}

impl From<NodeId<BuiltinOp>> for Value {
    fn from(id: NodeId<BuiltinOp>) -> Self {
        Self::BuiltinOp(id)
    }
}

impl From<NodeId<ClassMethod>> for Value {
    fn from(id: NodeId<ClassMethod>) -> Self {
        Self::ClassMethod(id)
    }
}

impl From<NodeId<Parameter>> for Value {
    fn from(id: NodeId<Parameter>) -> Self {
        Self::Parameter(id)
    }
}

impl From<NodeId<LocalVar>> for Value {
    fn from(id: NodeId<LocalVar>) -> Self {
        Self::LocalVar(id)
    }
}

impl From<NodeId<LocalFunction>> for Value {
    fn from(id: NodeId<LocalFunction>) -> Self {
        Self::LocalFunction(id)
    }
}

impl From<NodeId<PatternVar>> for Value {
    fn from(id: NodeId<PatternVar>) -> Self {
        Self::PatternVar(id)
    }
}

impl TryFrom<Construct> for Value {
    type Error = ();

    fn try_from(value: Construct) -> Result<Self, Self::Error> {
        match value {
            Construct::Function(id) => Ok(id.into()),
            Construct::CFunction(id) => Ok(id.into()),
            Construct::BuiltinOp(id) => Ok(id.into()),
            Construct::ClassMethod(id) => Ok(id.into()),
            Construct::Parameter(id) => Ok(id.into()),
            Construct::LocalVar(id) => Ok(id.into()),
            Construct::LocalFunction(id) => Ok(id.into()),
            Construct::PatternVar(id) => Ok(id.into()),
            _ => Err(()),
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
pub enum ValueCon {
    Data(NodeId<DataValueCon>),
    Builtin(NodeId<BuiltinValueCon>),
}

impl ValueCon {
    pub fn unit() -> Self {
        Self::Data(builtin::tuple_con(0))
    }

    pub fn tuple(size: usize) -> Self {
        Self::Data(builtin::tuple_con(size))
    }

    pub fn matches_tuple(self) -> Option<usize> {
        match self {
            Self::Data(id) => builtin::matches_tuple_con(id),
            Self::Builtin(_) => None,
        }
    }

    pub const fn bool(value: bool) -> Self {
        Self::Data(if value { builtin::TRUE } else { builtin::FALSE })
    }

    pub const NONE: Self = Self::Data(builtin::NONE);
    pub const SOME: Self = Self::Data(builtin::SOME);

    pub const ERR: Self = Self::Data(builtin::ERR);
    pub const OK: Self = Self::Data(builtin::OK);

    pub const SYNTAX: Self = Self::Builtin(builtin::SYNTAX_VALUE);
}

impl From<NodeId<DataValueCon>> for ValueCon {
    fn from(id: NodeId<DataValueCon>) -> Self {
        Self::Data(id)
    }
}

impl From<NodeId<BuiltinValueCon>> for ValueCon {
    fn from(id: NodeId<BuiltinValueCon>) -> Self {
        Self::Builtin(id)
    }
}

impl TryFrom<Construct> for ValueCon {
    type Error = ();

    fn try_from(value: Construct) -> Result<Self, Self::Error> {
        match value {
            Construct::DataValueCon(id) => Ok(id.into()),
            Construct::BuiltinValueCon(id) => Ok(id.into()),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExprApply {
    pub callee: Expr,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct ExprLet {
    pub defs: Vec<Either<LocalVar, LocalFunction>>,
    pub body: Expr,
}

impl ExprLet {
    pub fn local_vars(&self) -> impl Iterator<Item = &LocalVar> {
        self.defs.iter().filter_map(|def| def.as_ref().left())
    }

    pub fn local_functions(&self) -> impl Iterator<Item = &LocalFunction> {
        self.defs.iter().filter_map(|def| def.as_ref().right())
    }

    pub fn let_binding_context(&self) -> ExprLetBindingContext {
        ExprLetBindingContext(self)
    }
}

pub struct ExprLetBindingContext<'a>(&'a ExprLet);

impl<'a> ExprLetBindingContext<'a> {
    pub fn defs(&self) -> impl Iterator<Item = &LocalFunction> {
        // * local functions mutual recursion is allowed
        // * local variables are initialized after the local functions are prepared
        self.0.local_functions()
    }
}

#[derive(Debug, Clone)]
pub struct ExprSeq {
    pub stmts: Vec<Expr>,
    pub ret: Expr,
}

#[derive(Debug, Clone)]
pub struct ExprAnnotate {
    pub body: Expr,
    pub ty: Annotation<Type>,
}

#[derive(Debug, Clone)]
pub struct ExprIf {
    pub cond: Expr,
    pub then: Expr,
    pub else_: Expr,
}

#[derive(Debug, Clone)]
pub struct ExprWhile {
    pub cond: Expr,
    pub body: Expr,
}

#[derive(Debug, Clone)]
pub struct ExprMatch {
    pub target: Expr,
    pub clauses: Vec<(Pattern, Expr)>,
}

#[derive(Debug, Clone)]
pub struct LocalVar {
    pub id: NodeId<LocalVar>,
    pub ty: Option<Annotation<Type>>,
    pub init: Expr,
}

#[derive(Debug, Clone)]
pub struct LocalFunction {
    pub id: NodeId<LocalFunction>,
    pub params: Vec<Parameter>,
    pub scheme: Option<Annotation<Scheme>>,
    pub body: Expr,
}

impl<'a> topological_sort::DependencyList<NodeId<LocalFunction>> for LocalFunction {
    fn traverse_dependencies(&self, f: &mut impl FnMut(&NodeId<LocalFunction>)) {
        self.body.traverse_dependencies(f);
    }
}

#[derive(Debug, Clone)]
pub enum InitExpr {
    Eval(Expr),
    EnsureInitialized(ModuleId),
}

impl InitExpr {
    pub fn expr(&self) -> Option<&Expr> {
        match self {
            Self::Eval(e) => Some(e),
            Self::EnsureInitialized(_) => None,
        }
    }
}

pub trait ValueConFormatter {
    fn fmt_data_value_con(
        &self,
        f: &mut fmt::Formatter<'_>,
        id: NodeId<DataValueCon>,
    ) -> fmt::Result;

    fn fmt_builtin_value_con(
        &self,
        f: &mut fmt::Formatter<'_>,
        id: NodeId<BuiltinValueCon>,
    ) -> fmt::Result;
}

impl<F: ValueConFormatter> ContextualDisplay<F> for ValueCon {
    fn fmt_with(&self, ctx: &F, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Data(id) => write!(f, "{}", id.fmt_on(ctx)),
            Self::Builtin(id) => write!(f, "{}", id.fmt_on(ctx)),
        }
    }
}

impl<F: ValueConFormatter> ContextualDisplay<F> for NodeId<DataValueCon> {
    fn fmt_with(&self, ctx: &F, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        ctx.fmt_data_value_con(f, *self)
    }
}

impl<F: ValueConFormatter> ContextualDisplay<F> for NodeId<BuiltinValueCon> {
    fn fmt_with(&self, ctx: &F, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        ctx.fmt_builtin_value_con(f, *self)
    }
}
