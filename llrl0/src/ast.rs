use derive_new::new;
use std::collections::HashMap;
use std::fmt;

#[macro_use]
mod kind;
#[macro_use]
mod types;
pub mod builtin;
mod construct;
mod consts;
mod decl;
mod expr;
mod node_id;
mod pattern;
mod root;

pub use construct::Construct;
pub use consts::Const;
pub use decl::*;
pub use expr::*;
pub use kind::*;
pub use node_id::{ModuleId, NodeId, NodeIdGenerator};
pub use pattern::*;
pub use root::*;
pub use types::*;

/// a Zero-Sized Type used to provide an AST builder implementation.
#[derive(Debug, Clone, Copy)]
pub struct Ast;

/// A representation of the use of some language construct.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
pub enum Use<T> {
    Unresolved(NodeId<Use<T>>),
    Resolved(T, Option<NodeId<Use<T>>>),
}

impl<T> Use<T> {
    pub fn is_resolved(&self) -> bool {
        match self {
            Self::Unresolved(_) => false,
            Self::Resolved(_, _) => true,
        }
    }

    pub fn get_resolved(&self) -> &T {
        match self {
            Self::Resolved(ref v, _) => v,
            Self::Unresolved(id) => panic!("called get_resolved before set: {}", id),
        }
    }

    pub fn set_resolved(&mut self, value: T) {
        match self {
            Self::Unresolved(id) => *self = Self::Resolved(value, Some(*id)),
            Self::Resolved(_, _) => panic!("called set_resolved more than once"),
        }
    }
}

impl<T: fmt::Display> fmt::Display for Use<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Use::Unresolved(id) => id.fmt(f),
            Use::Resolved(ref v, _) => v.fmt(f),
        }
    }
}

impl<T> From<T> for Use<T> {
    fn from(value: T) -> Self {
        Self::Resolved(value, None)
    }
}

impl<T> From<NodeId<Use<T>>> for Use<T> {
    fn from(id: NodeId<Use<T>>) -> Self {
        Self::Unresolved(id)
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash, new)]
pub struct Annotation<T> {
    pub id: NodeId<Annotation<T>>,
    pub body: T,
}

/// A language construct with a set of type parameters and constraints.
pub trait Generic {
    fn generic_types(&self) -> &[TypeParameter];

    fn generic_constraints(&self) -> &[Constraint];

    fn is_monomorphic(&self) -> bool {
        self.generic_types().is_empty() && self.generic_constraints().is_empty()
    }
}

/// Depth first search.
pub trait Dfs: Sized {
    fn dfs_<E>(&self, f: &mut impl FnMut(&Self) -> Result<(), E>) -> Result<(), E>;

    fn dfs<E>(&self, mut f: impl FnMut(&Self) -> Result<(), E>) -> Result<(), E> {
        self.dfs_(&mut f)
    }

    fn dfs_do(&self, mut f: impl FnMut(&Self)) {
        self.dfs(|a| {
            f(a);
            Ok::<(), ()>(())
        })
        .unwrap(); // .into_ok();
    }
}
