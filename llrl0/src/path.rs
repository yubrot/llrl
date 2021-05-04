use itertools::Itertools;
use std::fmt;
use std::str::FromStr;

#[derive(PartialEq, PartialOrd, thiserror::Error, Debug, Clone)]
pub enum Error {
    #[error("Path contains illegal characters: \"{0}\"")]
    PathContainsIllegalCharacters(String),
}

/// Path of the llrl module.
///
/// In llrl, modules are identified by a string in the form of a path separated by a slash `/`.
/// The first part of the path points to the name of the package, and the rest of the parts
/// correspond to the file path on the package. If the file path on the package is omitted,
/// it is treated as equivalent to `<package-name>/prelude`.
#[derive(PartialOrd, Ord, PartialEq, Eq, Debug, Clone, Hash, Default)]
pub struct Path {
    pub package: PackageName,
    pub module: ModuleName,
}

impl Path {
    pub const fn new(package: PackageName, module: ModuleName) -> Self {
        Self { package, module }
    }

    pub const fn current() -> Self {
        Self::new(PackageName::current(), ModuleName::prelude())
    }

    pub fn builtin() -> Self {
        Self::new(PackageName::builtin(), ModuleName::prelude())
    }

    pub fn std() -> Self {
        Self::new(PackageName::std(), ModuleName::prelude())
    }
}

impl FromStr for Path {
    type Err = Error;

    fn from_str(path: &str) -> Result<Self, Self::Err> {
        if let Some(index) = path.find('/') {
            let path_head = &path[0..index];
            let path_tail = &path[index + '/'.len_utf8()..];
            let package = PackageName::from_str(path_head)?;
            let module = ModuleName::from_str(path_tail)?;
            Ok(Path::new(package, module))
        } else {
            let package = PackageName::from_str(path)?;
            let module = ModuleName::prelude();
            Ok(Path::new(package, module))
        }
    }
}

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.module.is_prelude() {
            write!(f, "{}", self.package)
        } else {
            write!(f, "{}/{}", self.package, self.module)
        }
    }
}

/// Name of the llrl package.
///
/// There are several well known package names:
/// - `~`: The special pakcage name that refers to the current package.
/// - `builtin`: a set of language built-in definitions used directly by numeric literals, etc.
/// - `std`: the llrl language standard library.
#[derive(PartialOrd, Ord, PartialEq, Eq, Debug, Clone, Hash, Default)]
pub struct PackageName(Option<String>);

impl PackageName {
    pub const fn current() -> Self {
        Self(None)
    }

    pub fn builtin() -> Self {
        Self::external("builtin").unwrap()
    }

    pub fn std() -> Self {
        Self::external("std").unwrap()
    }

    pub fn external(name: &str) -> Result<Self, Error> {
        if Self::is_valid_external_name(name) {
            Ok(Self(Some(name.to_string())))
        } else {
            Err(Error::PathContainsIllegalCharacters(name.to_string()))
        }
    }

    pub fn is_current(&self) -> bool {
        self.0.is_none()
    }

    pub fn is_external(&self) -> bool {
        self.0.is_some()
    }

    pub fn external_name(&self) -> Option<&String> {
        self.0.as_ref()
    }

    fn is_valid_external_name(name: &str) -> bool {
        !name.is_empty() && name.find('/').is_none() && name != "~" && name != "." && name != ".."
    }
}

impl FromStr for PackageName {
    type Err = Error;

    fn from_str(name: &str) -> Result<Self, Self::Err> {
        if name == "~" {
            Ok(Self::current())
        } else {
            Self::external(name)
        }
    }
}

impl fmt::Display for PackageName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.external_name() {
            None => write!(f, "~"),
            Some(name) => write!(f, "{}", name),
        }
    }
}

/// Path of the llrl module in a particular package.
#[derive(PartialOrd, Ord, PartialEq, Eq, Debug, Clone, Hash, Default)]
pub struct ModuleName(Vec<String>);

impl ModuleName {
    pub const fn prelude() -> Self {
        // Internally, prelude will be normalized as an empty path.
        Self(Vec::new())
    }

    pub fn from_parts<'a>(parts: impl IntoIterator<Item = &'a str>) -> Result<Self, Error> {
        let parts = parts.into_iter().map(|s| s.to_string()).collect::<Vec<_>>();
        if parts.len() == 1 && parts[0] == "prelude" {
            Ok(Self::prelude())
        } else if let Some(part) = parts.iter().find(|part| !Self::is_valid_part(part)) {
            Err(Error::PathContainsIllegalCharacters(part.to_string()))
        } else {
            Ok(Self(parts))
        }
    }

    pub fn is_prelude(&self) -> bool {
        self.0.is_empty()
    }

    /// Get the each part of the `ModuleName` separated by a slash '/'.
    /// If the path is equivalent to `prelude`, an empty Vec is returned.
    pub fn raw_parts(&self) -> &Vec<String> {
        &self.0
    }

    fn is_valid_part(part: &str) -> bool {
        !part.is_empty() && part.find('/').is_none() && part != "~" && part != "." && part != ".."
    }
}

impl FromStr for ModuleName {
    type Err = Error;

    fn from_str(name: &str) -> Result<Self, Self::Err> {
        Self::from_parts(name.split('/'))
    }
}

impl fmt::Display for ModuleName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.raw_parts().as_slice() {
            [] => write!(f, "prelude"),
            parts => write!(f, "{}", parts.iter().format("/")),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn testcases() -> Vec<(&'static str, Option<Path>, Option<&'static str>)> {
        return vec![
            ("", None, None),
            (
                "~",
                Some(Path {
                    package: PackageName::current(),
                    module: ModuleName::prelude(),
                }),
                None,
            ),
            (
                "hello",
                Some(Path {
                    package: PackageName::external("hello").unwrap(),
                    module: ModuleName::prelude(),
                }),
                None,
            ),
            ("~/", None, None),
            ("hello/", None, None),
            (
                "~/hello",
                Some(Path {
                    package: PackageName::current(),
                    module: ModuleName::from_parts(vec!["hello"]).unwrap(),
                }),
                None,
            ),
            (
                "hello/world",
                Some(Path {
                    package: PackageName::external("hello").unwrap(),
                    module: ModuleName::from_parts(vec!["world"]).unwrap(),
                }),
                None,
            ),
            (
                "~/prelude",
                Some(Path {
                    package: PackageName::current(),
                    module: ModuleName::prelude(),
                }),
                Some("~"),
            ),
            (
                "hello/prelude",
                Some(Path {
                    package: PackageName::external("hello").unwrap(),
                    module: ModuleName::prelude(),
                }),
                Some("hello"),
            ),
            ("~/hello/", None, None),
            ("hello/world/", None, None),
            (
                "~/hello/world",
                Some(Path {
                    package: PackageName::current(),
                    module: ModuleName::from_parts(vec!["hello", "world"]).unwrap(),
                }),
                None,
            ),
            (
                "hello/world/test",
                Some(Path {
                    package: PackageName::external("hello").unwrap(),
                    module: ModuleName::from_parts(vec!["world", "test"]).unwrap(),
                }),
                None,
            ),
        ];
    }

    #[test]
    fn test_from_str() {
        for (input, result, _) in testcases() {
            assert_eq!(Path::from_str(input).ok(), result);
        }
    }

    #[test]
    fn test_display() {
        for (input, result, output) in testcases() {
            if let Some(result) = result {
                assert_eq!(output.unwrap_or(input), result.to_string());
            }
        }
    }
}
