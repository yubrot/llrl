use super::{Code, Error, SOURCE_CODE_EXTENSION};
use crate::path::{ModuleName, PackageName, Path};
use crate::sexp::Ss;
use crate::source_loc::SourceLocator;
use std::collections::{hash_map, HashMap};
use std::fs;
use std::io;
use std::path;

#[derive(Debug, Clone)]
pub struct Loader {
    loadable_packages: HashMap<PackageName, LoadablePackage>,
}

impl Loader {
    pub fn new() -> Self {
        Self {
            loadable_packages: HashMap::new(),
        }
    }

    pub fn add_package(&mut self, name: PackageName, p: impl Into<LoadablePackage>) -> bool {
        if let hash_map::Entry::Vacant(e) = self.loadable_packages.entry(name) {
            e.insert(p.into());
            true
        } else {
            false
        }
    }

    pub fn add_source(&mut self, path: Path, p: impl Into<LoadableSource>) -> bool {
        match self.loadable_packages.entry(path.package) {
            hash_map::Entry::Occupied(mut e) => match e.get_mut() {
                LoadablePackage::InMemory(map) => {
                    map.insert(path.module, p.into());
                    true
                }
                _ => false,
            },
            hash_map::Entry::Vacant(e) => {
                let mut map = HashMap::new();
                map.insert(path.module, p.into());
                e.insert(map.into());
                true
            }
        }
    }

    pub fn load(&self, path: Path, locator: &mut SourceLocator) -> Result<Code, (Path, Error)> {
        match self.loadable_packages.get(&path.package) {
            Some(loadable_package) => loadable_package.load(path, locator),
            None => Err((path, Error::PackageNotFound)),
        }
    }
}

#[derive(Debug, Clone)]
pub enum LoadablePackage {
    InMemory(HashMap<ModuleName, LoadableSource>),
    FileSystem(path::PathBuf),
}

impl LoadablePackage {
    pub fn load(&self, path: Path, locator: &mut SourceLocator) -> Result<Code, (Path, Error)> {
        match self {
            Self::InMemory(map) => match map.get(&path.module) {
                Some(source) => Ok(source.to_code(path, locator)),
                None => Err((path, Error::ModuleNotFound)),
            },
            Self::FileSystem(fs_path) => {
                let fs_path = {
                    let mut fs_path = fs_path.clone();
                    match path.module.raw_parts().as_slice() {
                        [] => fs_path.push("prelude"),
                        parts => {
                            for part in parts {
                                fs_path.push(part);
                            }
                        }
                    }
                    fs_path.set_extension(SOURCE_CODE_EXTENSION);
                    fs_path
                };

                match fs::read_to_string(fs_path) {
                    Ok(text) => Ok(Code::from_source_text(path, locator, &text)),
                    Err(e) => Err((
                        path,
                        if e.kind() == io::ErrorKind::NotFound {
                            Error::ModuleNotFound
                        } else {
                            Error::LoadFailed(e.to_string())
                        },
                    )),
                }
            }
        }
    }
}

impl<S: Into<LoadableSource>> From<HashMap<String, S>> for LoadablePackage {
    fn from(src: HashMap<String, S>) -> Self {
        Self::InMemory(
            src.into_iter()
                .map(|(name, source)| (name.parse::<ModuleName>().unwrap(), source.into()))
                .collect(),
        )
    }
}

impl<S: Into<LoadableSource>> From<HashMap<ModuleName, S>> for LoadablePackage {
    fn from(src: HashMap<ModuleName, S>) -> Self {
        Self::InMemory(
            src.into_iter()
                .map(|(name, source)| (name, source.into()))
                .collect(),
        )
    }
}

impl From<path::PathBuf> for LoadablePackage {
    fn from(src: path::PathBuf) -> Self {
        Self::FileSystem(src)
    }
}

impl From<String> for LoadablePackage {
    fn from(text: String) -> Self {
        Self::InMemory(
            vec![(ModuleName::prelude(), text.into())]
                .into_iter()
                .collect(),
        )
    }
}

impl From<Ss> for LoadablePackage {
    fn from(ss: Ss) -> Self {
        Self::InMemory(
            vec![(ModuleName::prelude(), ss.into())]
                .into_iter()
                .collect(),
        )
    }
}

#[derive(Debug, Clone)]
pub enum LoadableSource {
    Text(String),
    Just(Ss),
}

impl LoadableSource {
    pub fn to_code(&self, path: Path, locator: &mut SourceLocator) -> Code {
        match self {
            Self::Text(text) => Code::from_source_text(path, locator, text),
            Self::Just(ss) => Code::from_source(path, ss.clone()),
        }
    }
}

impl From<String> for LoadableSource {
    fn from(text: String) -> Self {
        Self::Text(text)
    }
}

impl From<Ss> for LoadableSource {
    fn from(ss: Ss) -> Self {
        Self::Just(ss)
    }
}
