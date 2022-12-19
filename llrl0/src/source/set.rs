use super::{Error, Source};
use crate::path::{ModuleName, PackageName, Path};
use crate::topological_sort;
use std::collections::HashMap;

/// A set of loaded sources.
#[derive(Debug, Clone)]
pub struct SourceSet {
    map: HashMap<PackageName, HashMap<ModuleName, Source>>,
}

impl SourceSet {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn insert(&mut self, source: Source) {
        let path = source.path.clone();
        self.map
            .entry(path.package)
            .or_insert_with(HashMap::new)
            .insert(path.module, source);
    }

    pub fn packages(&self) -> &HashMap<PackageName, HashMap<ModuleName, Source>> {
        &self.map
    }

    pub fn sources(&self) -> impl Iterator<Item = &Source> {
        self.map.values().flat_map(|sources| sources.values())
    }

    pub fn errors(&self) -> impl Iterator<Item = (&Path, &Error)> {
        self.sources()
            .flat_map(|source| source.errors.iter().map(move |e| (&source.path, e)))
    }

    /// Resolve dependencies and order them from the dependent source.
    ///
    /// If there is a circular dependency, it is returned as an error.
    pub fn resolve_dependencies_order(self) -> Result<Vec<Source>, Vec<Path>> {
        let sorted_sources =
            topological_sort::run(self.map.into_iter().flat_map(|(_, sources)| {
                sources
                    .into_values()
                    .map(|source| (source.path.clone(), source))
            }));

        sorted_sources
            .into_iter()
            .map(|mut sources| {
                if sources.len() == 1 {
                    Ok(sources.swap_remove(0))
                } else {
                    Err(sources
                        .into_iter()
                        .map(|source| source.path)
                        .collect::<Vec<_>>())
                }
            })
            .collect()
    }
}
