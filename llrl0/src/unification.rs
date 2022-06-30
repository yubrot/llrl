//! An implementation of the unification algorithm.

pub mod kind;
pub mod types;

use std::fmt;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;

#[derive(Debug)]
pub struct Var<T>(u32, PhantomData<fn() -> T>);

impl<T> Var<T> {
    fn from_index(index: usize) -> Self {
        Self(index as u32, PhantomData)
    }

    fn index(self) -> usize {
        self.0 as usize
    }
}

// Due to rust-lang/rust#26925 we need some manual impls

impl<T> PartialEq for Var<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T> Eq for Var<T> {}

impl<T> PartialOrd for Var<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for Var<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl<T> Clone for Var<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Var<T> {}

impl<T> Hash for Var<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

impl<T> fmt::Display for Var<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "_{}", self.0)
    }
}

#[derive(Debug)]
pub struct Vars<T>(u32, u32, PhantomData<fn() -> T>);

impl<T> Vars<T> {
    fn from_index_count(len: usize, count: usize) -> Self {
        if count == 0 {
            Self(0, 0, PhantomData)
        } else {
            Self(len as u32, (len + count) as u32, PhantomData)
        }
    }

    fn range(self) -> std::ops::Range<u32> {
        self.0..self.1
    }

    pub fn is_empty(self) -> bool {
        self.0 == self.1
    }

    pub fn len(self) -> u32 {
        self.1 - self.0
    }

    pub fn iter(self) -> impl Iterator<Item = Var<T>> {
        (self.0..self.1).map(|i| Var(i, PhantomData))
    }
}

impl<T> PartialEq for Vars<T> {
    fn eq(&self, other: &Self) -> bool {
        self.range() == other.range()
    }
}

impl<T> Eq for Vars<T> {}

impl<T> PartialOrd for Vars<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for Vars<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.range().cmp(other.range())
    }
}

impl<T> Clone for Vars<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Vars<T> {}

impl<T> Hash for Vars<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.range().hash(state)
    }
}

impl<T> From<Var<T>> for Vars<T> {
    fn from(var: Var<T>) -> Self {
        Self::from_index_count(var.index(), 1)
    }
}

#[derive(PartialEq, PartialOrd, thiserror::Error, Debug, Clone)]
pub enum Error {
    #[error("Occurs check failed")]
    OccursCheckFailed,

    #[error("Mismatch")]
    Mismatch,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
pub struct Level(u32);

impl Level {
    pub fn top() -> Self {
        Self(0)
    }

    pub fn bottom() -> Self {
        Self(std::u32::MAX)
    }

    pub fn depth(self) -> u32 {
        self.0
    }

    pub fn down(self) -> Self {
        Self(self.0 + 1)
    }
}
