use super::{Code, Error};
use crate::path::{ModuleName, PackageName, Path};
use crate::topological_sort;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct CodeSet {
    map: HashMap<PackageName, HashMap<ModuleName, Code>>,
}

impl CodeSet {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn insert(&mut self, code: Code) {
        let path = code.path.clone();
        self.map
            .entry(path.package)
            .or_insert(HashMap::new())
            .insert(path.module, code);
    }

    pub fn packages(&self) -> &HashMap<PackageName, HashMap<ModuleName, Code>> {
        &self.map
    }

    pub fn codes(&self) -> impl Iterator<Item = &Code> {
        self.map.values().flat_map(|codes| codes.values())
    }

    pub fn errors(&self) -> impl Iterator<Item = (&Path, &Error)> {
        self.codes()
            .flat_map(|code| code.errors.iter().map(move |e| (&code.path, e)))
    }

    pub fn resolve_dependencies_order(self) -> Result<Vec<Code>, Vec<Path>> {
        let sorted_codes =
            topological_sort::run(self.map.into_iter().flat_map(|(_, codes)| {
                codes.into_iter().map(|(_, code)| (code.path.clone(), code))
            }));

        sorted_codes
            .into_iter()
            .map(|mut codes| {
                if codes.len() == 1 {
                    Ok(codes.swap_remove(0))
                } else {
                    Err(codes.into_iter().map(|code| code.path).collect::<Vec<_>>())
                }
            })
            .collect()
    }
}
