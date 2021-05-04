use std::fmt;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;

/// An identifier of the module.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug, Copy, Hash)]
pub struct ModuleId(u32);

impl ModuleId {
    pub const fn from_index(index: usize) -> Self {
        Self(index as u32)
    }

    pub const fn builtin() -> Self {
        Self(0)
    }

    pub fn is_builtin(self) -> bool {
        self.0 == 0
    }

    pub fn as_index(self) -> usize {
        self.0 as usize
    }
}

impl fmt::Display for ModuleId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<Module {}>", self.0)
    }
}

/// A globally unique identifier of the node in the module.
#[derive(Debug)]
pub struct NodeId<T: ?Sized>(u64, PhantomData<T>);

impl<T: ?Sized> NodeId<T> {
    pub const fn new_unchecked(mid: ModuleId, id: u32) -> Self {
        Self((mid.0 as u64) | (id as u64) << 32, PhantomData)
    }

    pub fn module(self) -> ModuleId {
        ModuleId(self.0 as u32)
    }

    pub fn index_in_module(self) -> u32 {
        (self.0 >> 32) as u32
    }

    pub fn reinterpret_unchecked<S: ?Sized>(self) -> NodeId<S> {
        NodeId(self.0, PhantomData)
    }
}

impl<T: ?Sized> Into<u64> for NodeId<T> {
    fn into(self) -> u64 {
        self.0
    }
}

// Due to rust-lang/rust#26925 we need some manual impls

impl<T: ?Sized> PartialEq for NodeId<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T: ?Sized> Eq for NodeId<T> {}

impl<T: ?Sized> PartialOrd for NodeId<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: ?Sized> Ord for NodeId<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl<T: ?Sized> fmt::Display for NodeId<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "#<Module {}. {} {}>",
            self.module().0,
            std::any::type_name::<T>().split("::").last().unwrap(),
            self.index_in_module(),
        )
    }
}

impl<T: ?Sized> Clone for NodeId<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: ?Sized> Copy for NodeId<T> {}

impl<T: ?Sized> Hash for NodeId<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

#[derive(Debug, Clone)]
pub struct NodeIdGenerator {
    module: ModuleId,
    latest: u32,
}

impl NodeIdGenerator {
    pub fn in_module(module: ModuleId) -> Self {
        // Reserved `NodeId`s are defined in the `builtin` module.
        let latest = if module.0 == 0 { 1000 } else { 0 };
        Self { module, latest }
    }

    pub fn module(&self) -> ModuleId {
        self.module
    }

    pub fn generate<T: ?Sized>(&mut self) -> NodeId<T> {
        self.latest += 1;
        NodeId::new_unchecked(self.module, self.latest)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn node_id_uniqueness() {
        let mut gen = NodeIdGenerator::in_module(ModuleId(1));
        let a = gen.generate::<()>();
        let b = gen.generate::<()>();
        let c = gen.generate::<()>();
        assert_ne!(a, b);
        assert_ne!(b, c);
    }
}
