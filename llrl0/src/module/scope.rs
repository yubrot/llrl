use super::{builder, Error, LocatedConstruct};
use std::collections::HashMap;

/// A key-value store for language constructs.
pub trait Scope: Sized {
    fn get(&self, name: &str) -> Option<LocatedConstruct>;
    fn enter_scope(&mut self) -> LocalScope;
    fn define(&mut self, name: &str, c: LocatedConstruct) -> builder::Result<()>;
}

impl<'a, T: Scope> Scope for &'a mut T {
    fn get(&self, name: &str) -> Option<LocatedConstruct> {
        T::get(*self, name)
    }

    fn enter_scope(&mut self) -> LocalScope {
        T::enter_scope(*self)
    }

    fn define(&mut self, name: &str, c: LocatedConstruct) -> builder::Result<()> {
        T::define(*self, name, c)
    }
}

#[derive(Debug, Clone)]
pub struct TopLevel {
    map: HashMap<String, LocatedConstruct>,
}

impl TopLevel {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn iter<'a>(&'a self) -> impl Iterator<Item = (&'a str, LocatedConstruct)> + 'a {
        self.map.iter().map(|(name, c)| (name.as_str(), *c))
    }
}

impl Scope for TopLevel {
    fn get(&self, name: &str) -> Option<LocatedConstruct> {
        self.map.get(name).copied()
    }

    fn enter_scope(&mut self) -> LocalScope {
        LocalScope::new(&mut self.map)
    }

    fn define(&mut self, name: &str, c: LocatedConstruct) -> builder::Result<()> {
        match self.map.insert(name.to_string(), c) {
            Some(old_c) if old_c.construct != c.construct => {
                Err(Error::multiple_declarations(name, old_c, c))
            }
            _ => Ok(()),
        }
    }
}

pub struct LocalScope<'a> {
    map: &'a mut HashMap<String, LocatedConstruct>,
    stash: HashMap<String, Option<LocatedConstruct>>,
}

impl<'a> LocalScope<'a> {
    pub fn new(map: &'a mut HashMap<String, LocatedConstruct>) -> Self {
        Self {
            map,
            stash: HashMap::new(),
        }
    }
}

impl<'a> Drop for LocalScope<'a> {
    fn drop(&mut self) {
        for (name, c) in self.stash.drain() {
            match c {
                Some(c) => self.map.insert(name, c),
                None => self.map.remove(&name),
            };
        }
    }
}

impl<'a> Scope for LocalScope<'a> {
    fn get(&self, name: &str) -> Option<LocatedConstruct> {
        self.map.get(name).copied()
    }

    fn enter_scope(&mut self) -> LocalScope {
        LocalScope::new(&mut self.map)
    }

    fn define(&mut self, name: &str, c: LocatedConstruct) -> builder::Result<()> {
        if self
            .stash
            .insert(name.to_string(), self.map.remove(name))
            .is_some()
        {
            return Err(Error::duplicated_identifier(
                name,
                self.stash.get(name).unwrap().unwrap(),
                c,
            ));
        }
        self.map.insert(name.to_string(), c);
        Ok(())
    }
}
