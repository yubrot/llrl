use super::{pass, Error};
use crate::ast::*;
use crate::source_loc::SourceLocation;
use smallvec::{smallvec, SmallVec};
use std::collections::{HashMap, HashSet};

/// Representation of a binding to the scope.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
pub struct Binding {
    pub loc: SourceLocation,
    pub construct: Construct,
}

impl Binding {
    pub fn new(loc: SourceLocation, construct: impl Into<Construct>) -> Self {
        Self {
            loc,
            construct: construct.into(),
        }
    }

    pub fn with_loc(self, loc: SourceLocation) -> Self {
        Self { loc, ..self }
    }
}

/// A key-value store for language constructs.
pub trait Scope: Sized {
    fn get(&self, name: &str) -> Option<Binding>;
    fn enter_scope(&mut self) -> LocalScope;
    fn define(&mut self, name: &str, binding: Binding) -> pass::Result<()>;
}

impl<'a, T: Scope> Scope for &'a mut T {
    fn get(&self, name: &str) -> Option<Binding> {
        T::get(*self, name)
    }

    fn enter_scope(&mut self) -> LocalScope {
        T::enter_scope(*self)
    }

    fn define(&mut self, name: &str, binding: Binding) -> pass::Result<()> {
        T::define(*self, name, binding)
    }
}

#[derive(Debug, Clone)]
pub struct TopLevel {
    map: HashMap<String, SmallVec<[Binding; 1]>>,
}

impl TopLevel {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn iter<'a>(&'a self) -> impl Iterator<Item = (&'a str, Binding)> + 'a {
        self.map
            .iter()
            .map(|(name, bs)| (name.as_str(), *bs.first().unwrap()))
    }
}

impl Scope for TopLevel {
    fn get(&self, name: &str) -> Option<Binding> {
        self.map.get(name).and_then(|vec| vec.last().copied())
    }

    fn enter_scope(&mut self) -> LocalScope {
        LocalScope::new(&mut self.map)
    }

    fn define(&mut self, name: &str, binding: Binding) -> pass::Result<()> {
        match self.map.insert(name.to_string(), smallvec![binding]) {
            None => Ok(()),
            Some(vec) => match vec.as_slice() {
                [old_binding, ..] if old_binding.construct != binding.construct => {
                    Err(Error::multiple_declarations(name, *old_binding, binding))
                }
                _ => Ok(()),
            },
        }
    }
}

pub struct LocalScope<'a> {
    map: &'a mut HashMap<String, SmallVec<[Binding; 1]>>,
    scope_binds: HashSet<String>,
}

impl<'a> LocalScope<'a> {
    pub fn new(map: &'a mut HashMap<String, SmallVec<[Binding; 1]>>) -> Self {
        Self {
            map,
            scope_binds: HashSet::new(),
        }
    }
}

impl<'a> Drop for LocalScope<'a> {
    fn drop(&mut self) {
        for name in self.scope_binds.iter() {
            if let Some(v) = self.map.get_mut(name) {
                v.pop();
                if v.is_empty() {
                    self.map.remove(name);
                }
            }
        }
    }
}

impl<'a> Scope for LocalScope<'a> {
    fn get(&self, name: &str) -> Option<Binding> {
        self.map.get(name).and_then(|vec| vec.last().copied())
    }

    fn enter_scope(&mut self) -> LocalScope {
        LocalScope::new(&mut self.map)
    }

    fn define(&mut self, name: &str, binding: Binding) -> pass::Result<()> {
        if !self.scope_binds.insert(name.to_string()) {
            return Err(Error::duplicated_identifier(
                name,
                self.get(name).unwrap(),
                binding,
            ));
        }
        self.map.entry(name.to_string()).or_default().push(binding);
        Ok(())
    }
}
