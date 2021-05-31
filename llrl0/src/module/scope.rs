use super::{pass, Error, LocatedConstruct};
use smallvec::{smallvec, SmallVec};
use std::collections::{HashMap, HashSet};

/// A key-value store for language constructs.
pub trait Scope: Sized {
    fn get(&self, name: &str) -> Option<LocatedConstruct>;
    fn enter_scope(&mut self) -> LocalScope;
    fn define(&mut self, name: &str, c: LocatedConstruct) -> pass::Result<()>;
}

impl<'a, T: Scope> Scope for &'a mut T {
    fn get(&self, name: &str) -> Option<LocatedConstruct> {
        T::get(*self, name)
    }

    fn enter_scope(&mut self) -> LocalScope {
        T::enter_scope(*self)
    }

    fn define(&mut self, name: &str, c: LocatedConstruct) -> pass::Result<()> {
        T::define(*self, name, c)
    }
}

#[derive(Debug, Clone)]
pub struct TopLevel {
    map: HashMap<String, SmallVec<[LocatedConstruct; 1]>>,
}

impl TopLevel {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn iter<'a>(&'a self) -> impl Iterator<Item = (&'a str, LocatedConstruct)> + 'a {
        self.map
            .iter()
            .map(|(name, cs)| (name.as_str(), *cs.first().unwrap()))
    }
}

impl Scope for TopLevel {
    fn get(&self, name: &str) -> Option<LocatedConstruct> {
        self.map.get(name).and_then(|vec| vec.last().copied())
    }

    fn enter_scope(&mut self) -> LocalScope {
        LocalScope::new(&mut self.map)
    }

    fn define(&mut self, name: &str, c: LocatedConstruct) -> pass::Result<()> {
        match self.map.insert(name.to_string(), smallvec![c]) {
            None => Ok(()),
            Some(vec) => match vec.as_slice() {
                [old_c, ..] if old_c.construct != c.construct => {
                    Err(Error::multiple_declarations(name, *old_c, c))
                }
                _ => Ok(()),
            },
        }
    }
}

pub struct LocalScope<'a> {
    map: &'a mut HashMap<String, SmallVec<[LocatedConstruct; 1]>>,
    scope_binds: HashSet<String>,
}

impl<'a> LocalScope<'a> {
    pub fn new(map: &'a mut HashMap<String, SmallVec<[LocatedConstruct; 1]>>) -> Self {
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
    fn get(&self, name: &str) -> Option<LocatedConstruct> {
        self.map.get(name).and_then(|vec| vec.last().copied())
    }

    fn enter_scope(&mut self) -> LocalScope {
        LocalScope::new(&mut self.map)
    }

    fn define(&mut self, name: &str, c: LocatedConstruct) -> pass::Result<()> {
        if !self.scope_binds.insert(name.to_string()) {
            return Err(Error::duplicated_identifier(
                name,
                self.get(name).unwrap(),
                c,
            ));
        }
        self.map.entry(name.to_string()).or_default().push(c);
        Ok(())
    }
}
