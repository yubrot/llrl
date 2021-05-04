use super::{Code, Error};
use crate::graph;
use crate::path::{ModuleName, PackageName, Path};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct CodeMap {
    tree: HashMap<PackageName, HashMap<ModuleName, Code>>,
}

impl CodeMap {
    pub fn new() -> Self {
        Self {
            tree: HashMap::new(),
        }
    }

    pub fn insert(&mut self, path: Path, code: Code) {
        self.tree
            .entry(path.package)
            .or_insert(HashMap::new())
            .insert(path.module, code);
    }

    pub fn packages(&self) -> &HashMap<PackageName, HashMap<ModuleName, Code>> {
        &self.tree
    }

    pub fn codes(&self) -> impl Iterator<Item = &Code> {
        self.tree.values().flat_map(|codes| codes.values())
    }

    pub fn errors(&self) -> impl Iterator<Item = (&Path, &Error)> {
        self.codes()
            .flat_map(|code| code.errors.iter().map(move |e| (&code.path, e)))
    }

    pub fn resolve_dependencies_order(self) -> Result<Vec<Code>, Vec<Path>> {
        let sorted_codes =
            graph::topological_sort(self.tree.into_iter().flat_map(|(_, codes)| {
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
