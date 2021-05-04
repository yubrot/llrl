use super::{Ast, Dfs, NodeId, Use};
use crate::formatting::ContextualDisplay;
use std::fmt;

/// In llrl, `Kind` represents the type of the language construct.
///
/// Note that it is not only a representation of the type of the "type-level" expression.
/// Kind is partially provided to the user as a first-class representation, for example it can be
/// written as a kind annotation for type parameters.
/// In fact, since the structure of the llrl AST itself restricts the possible forms of its
/// components according to the type system in the host language (i.e. Rust), not all of the Kind
/// information is essential in semantic analysis, but rather it is more of a supplementary information.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash)]
pub enum Kind {
    Use(Use<KindUse>),
    Type,
    Constraint,
    Satisfaction,
    Value,
    Macro,
    Fun(Vec<Kind>, Box<Kind>),
    Error(String),
}

impl Kind {
    pub fn contains_error(&self) -> bool {
        self.dfs(|kind| match kind {
            Self::Error(_) => Err(()),
            _ => Ok(()),
        })
        .is_err()
    }

    pub fn is_first_class(&self) -> bool {
        self.dfs(|kind| match kind {
            Self::Use(use_) => match *use_.get_resolved() {},
            Self::Type => Ok(()),
            Self::Fun(_, _) => Ok(()),
            _ => Err(()),
        })
        .is_ok()
    }

    pub fn use_(use_: impl Into<Use<KindUse>>) -> Self {
        Self::Use(use_.into())
    }

    pub fn fun(args: Vec<Self>, ret: Self) -> Self {
        if args.is_empty() {
            ret
        } else {
            Self::Fun(args, Box::new(ret))
        }
    }

    pub fn error(e: impl Into<String>) -> Self {
        Self::Error(e.into())
    }
}

impl Dfs for Kind {
    fn dfs_<E>(&self, f: &mut impl FnMut(&Self) -> Result<(), E>) -> Result<(), E> {
        match self {
            Self::Use(_) => {}
            Self::Type => {}
            Self::Constraint => {}
            Self::Satisfaction => {}
            Self::Value => {}
            Self::Macro => {}
            Self::Fun(ref args, ref ret) => {
                for arg in args.iter() {
                    arg.dfs_(f)?;
                }
                ret.dfs_(f)?;
            }
            Self::Error(_) => {}
        }
        f(self)
    }
}

pub trait KindBuilder {
    type Result: Sized;

    fn new_use(&mut self, id: NodeId<Use<KindUse>>) -> Self::Result;
    fn new_type(&mut self) -> Self::Result;
    fn new_constraint(&mut self) -> Self::Result;
    fn new_satisfaction(&mut self) -> Self::Result;
    fn new_value(&mut self) -> Self::Result;
    fn new_macro(&mut self) -> Self::Result;
    fn new_fun(&mut self, args: Vec<Self::Result>, ret: Self::Result) -> Self::Result;
    fn new_error(&mut self, e: impl Into<String>) -> Self::Result;
}

impl KindBuilder for Ast {
    type Result = Kind;

    fn new_use(&mut self, id: NodeId<Use<KindUse>>) -> Self::Result {
        Kind::use_(id)
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
        Kind::fun(args, ret)
    }

    fn new_error(&mut self, e: impl Into<String>) -> Self::Result {
        Kind::error(e)
    }
}

#[allow(unused_macros)]
macro_rules! build_kind_init {
    ($ctx:expr, $dest:expr, $a:tt) => {
    };

    ($ctx:expr, $dest:expr, $a:tt $( $b:tt )+) => {
        $dest.push(build_kind!($ctx, $a));
        build_kind_init!($ctx, $dest, $( $b )+)
    };
}

#[allow(unused_macros)]
macro_rules! build_kind_last {
    ($ctx:expr, $a:tt) => {
        build_kind!($ctx, $a)
    };

    ($ctx:expr, $a:tt $( $b:tt )+) => {
        build_kind_last!($ctx, $( $b )+)
    };
}

macro_rules! build_kind {
    ($ctx:expr, $expr:block) => {
        ($expr)
    };

    ($ctx:expr, (use $id:block)) => {
        $crate::ast::KindBuilder::new_use(&mut $ctx, $id)
    };

    ($ctx:expr, type) => {
        $crate::ast::KindBuilder::new_type(&mut $ctx)
    };

    ($ctx:expr, constraint) => {
        $crate::ast::KindBuilder::new_constraint(&mut $ctx)
    };

    ($ctx:expr, satisfaction) => {
        $crate::ast::KindBuilder::new_satisfaction(&mut $ctx)
    };

    ($ctx:expr, value) => {
        $crate::ast::KindBuilder::new_value(&mut $ctx)
    };

    ($ctx:expr, macro) => {
        $crate::ast::KindBuilder::new_macro(&mut $ctx)
    };

    ($ctx:expr, (-> ...$args:block $ret:tt)) => {
        {
            let args = $args;
            let ret = build_kind!($ctx, $ret);
            $crate::ast::KindBuilder::new_fun(&mut $ctx, args, ret)
        }
    };

    ($ctx:expr, (-> $( $s:tt )+)) => {
        {
            let mut args = Vec::new();
            build_kind_init!($ctx, args, $( $s )+);
            let ret = build_kind_last!($ctx, $( $s )+);
            $crate::ast::KindBuilder::new_fun(&mut $ctx, args, ret)
        }
    };

    ($ctx:expr, (error $e:literal)) => {
        $crate::ast::KindBuilder::new_error(&mut $ctx, $e)
    };

    ($ctx:expr, (error $e:block)) => {
        $crate::ast::KindBuilder::new_error(&mut $ctx, $e)
    };
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
pub enum KindUse {} // There are no resolvable Kind components

impl fmt::Display for KindUse {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {}
    }
}

pub trait KindFormatter {
    fn fmt_kind_use(&self, f: &mut fmt::Formatter<'_>, id: Use<KindUse>) -> fmt::Result;
}

impl<F: KindFormatter> ContextualDisplay<F> for Kind {
    fn fmt_with(&self, ctx: &F, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Use(use_) => write!(f, "{}", use_.fmt_on(ctx)),
            Self::Type => write!(f, "*"),
            Self::Constraint => write!(f, "Constraint"),
            Self::Satisfaction => write!(f, "Satisfaction"),
            Self::Value => write!(f, "Value"),
            Self::Macro => write!(f, "Macro"),
            Self::Fun(args, ret) => {
                write!(f, "(->")?;
                for arg in args.iter() {
                    write!(f, " {}", arg.fmt_on(ctx))?;
                }
                write!(f, " {})", ret.fmt_on(ctx))
            }
            Self::Error(e) => write!(f, "{}", e),
        }
    }
}

impl<F: KindFormatter> ContextualDisplay<F> for Use<KindUse> {
    fn fmt_with(&self, ctx: &F, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        ctx.fmt_kind_use(f, *self)
    }
}

#[cfg(test)]
mod tests {
    use super::super::ModuleId;
    use super::*;

    #[test]
    fn test_build() {
        build_kind!(Ast, (use {NodeId::new_unchecked(ModuleId::from_index(1), 0)}));
        build_kind!(Ast, type);
        build_kind!(Ast, constraint);
        build_kind!(Ast, satisfaction);
        build_kind!(Ast, value);
        build_kind!(Ast, macro);
        build_kind!(Ast, (-> type type type));
        build_kind!(Ast, (-> type {Kind::Type}));
        build_kind!(Ast, (-> ...{Vec::new()} type));
        build_kind!(Ast, (error "a"));
    }
}
