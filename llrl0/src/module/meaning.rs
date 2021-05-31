use crate::ast::*;
use crate::source_loc::SourceLocation;
use once_cell::sync::Lazy;
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::convert::TryFrom;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
pub struct LocatedConstruct {
    pub loc: SourceLocation,
    pub construct: Construct,
}

impl LocatedConstruct {
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

/// Dependencies of the module.
pub type Imports = HashSet<ModuleId>;

/// The set of located constructs that the module is exporting.
#[derive(Debug, Clone)]
pub struct Exports {
    map: HashMap<String, LocatedConstruct>,
}

impl Exports {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn add(&mut self, name: &str, c: LocatedConstruct) -> Option<LocatedConstruct> {
        match self.map.insert(name.to_string(), c) {
            Some(old_c) if old_c.construct != c.construct => Some(old_c),
            _ => None,
        }
    }

    pub fn iter<'a>(&'a self) -> impl Iterator<Item = (&'a str, LocatedConstruct)> + 'a {
        self.map.iter().map(|(name, def)| (name.as_str(), *def))
    }

    pub fn get(&self, name: &str) -> Option<LocatedConstruct> {
        self.map.get(name).copied()
    }
}

#[derive(Debug, Clone)]
pub struct TextualInformation {
    map: HashMap<Construct, TextualUnit>,
}

impl TextualInformation {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn get(&self, id: impl Into<Construct>) -> Option<&TextualUnit> {
        self.map.get(&id.into())
    }

    pub fn set(&mut self, id: impl Into<Construct>, loc: SourceLocation, name: impl Into<String>) {
        self.map
            .insert(id.into(), TextualUnit::new(loc, name.into()));
    }

    pub fn find<T: TryFrom<Construct>>(&self, name: &str) -> Vec<T> {
        self.map
            .iter()
            .filter_map(move |(construct, unit)| {
                if unit.name == *name {
                    T::try_from(*construct).ok()
                } else {
                    None
                }
            })
            .collect()
    }
}

#[derive(Debug, Clone)]
pub struct TextualUnit {
    pub loc: SourceLocation,
    pub name: String,
}

impl TextualUnit {
    pub fn new(loc: SourceLocation, name: impl Into<String>) -> Self {
        Self {
            loc,
            name: name.into(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct AvailableInstances {
    map: HashMap<NodeId<ClassCon>, HashSet<NodeId<InstanceCon>>>,
}

static DUMMY_INSTANCES: Lazy<HashSet<NodeId<InstanceCon>>> = Lazy::new(|| HashSet::new());

impl AvailableInstances {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn add(&mut self, con: &InstanceCon) {
        let ConstraintRep::Class(ref class, _) = con.target.rep;
        self.map
            .entry(*class.get_resolved())
            .or_default()
            .insert(con.id);
    }

    pub fn get(&self, id: NodeId<ClassCon>) -> &HashSet<NodeId<InstanceCon>> {
        self.map.get(&id).unwrap_or(&*DUMMY_INSTANCES)
    }

    pub fn iter(&self) -> impl Iterator<Item = (NodeId<ClassCon>, &HashSet<NodeId<InstanceCon>>)> {
        self.map.iter().map(|(k, v)| (*k, v))
    }
}

#[derive(Debug, Clone)]
pub struct InferredKinds {
    map: HashMap<Construct, Kind>,
}

impl InferredKinds {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn get(&self, construct: impl Into<Construct>) -> Option<Cow<Kind>> {
        let construct = construct.into();
        match construct {
            Construct::Function(_)
            | Construct::CFunction(_)
            | Construct::BuiltinOp(_)
            | Construct::DataTypeCon(_)
            | Construct::BuiltinTypeCon(_)
            | Construct::ClassCon(_)
            | Construct::ClassMethod(_)
            | Construct::InstanceCon(_)
            | Construct::InstanceMethod(_)
            | Construct::TypeParameter(_)
            | Construct::LocalFun(_) => self.map.get(&construct).map(Cow::Borrowed),
            Construct::Macro(_) => Some(Cow::Owned(Kind::Macro)),
            Construct::DataValueCon(_)
            | Construct::Parameter(_)
            | Construct::LocalVar(_)
            | Construct::PatternVar(_) => Some(Cow::Owned(Kind::Value)),
            _ => None,
        }
    }

    pub fn set(&mut self, construct: impl Into<Construct>, kind: Kind) {
        let construct = construct.into();
        match construct {
            Construct::Function(_)
            | Construct::CFunction(_)
            | Construct::BuiltinOp(_)
            | Construct::DataTypeCon(_)
            | Construct::BuiltinTypeCon(_)
            | Construct::ClassCon(_)
            | Construct::ClassMethod(_)
            | Construct::InstanceCon(_)
            | Construct::InstanceMethod(_)
            | Construct::TypeParameter(_)
            | Construct::LocalFun(_) => self.map.insert(construct, kind),
            Construct::Macro(_)
            | Construct::DataValueCon(_)
            | Construct::Parameter(_)
            | Construct::LocalVar(_)
            | Construct::PatternVar(_) => panic!("Kind of {} is reserved", construct),
            _ => panic!("Kind of {} is undefined", construct),
        };
    }
}

#[derive(Debug, Clone)]
pub struct InferredTypes {
    schemes: HashMap<Construct, Scheme>,
    types: HashMap<Construct, Type>,
    instantiations: HashMap<Construct, Instantiation>,
}

impl InferredTypes {
    pub fn new() -> Self {
        Self {
            schemes: HashMap::new(),
            types: HashMap::new(),
            instantiations: HashMap::new(),
        }
    }

    pub fn scheme(&self, construct: impl Into<Construct>) -> Option<&Scheme> {
        let construct = construct.into();
        match construct {
            Construct::Function(_)
            | Construct::BuiltinOp(_)
            | Construct::DataValueCon(_)
            | Construct::BuiltinValueCon(_)
            | Construct::ClassMethod(_)
            | Construct::InstanceMethod(_)
            | Construct::LocalFun(_) => self.schemes.get(&construct),
            _ => None,
        }
    }

    pub fn set_scheme(&mut self, construct: impl Into<Construct>, scheme: Scheme) {
        let construct = construct.into();
        match construct {
            Construct::Function(_)
            | Construct::BuiltinOp(_)
            | Construct::DataValueCon(_)
            | Construct::BuiltinValueCon(_)
            | Construct::ClassMethod(_)
            | Construct::InstanceMethod(_)
            | Construct::LocalFun(_) => self.schemes.insert(construct, scheme),
            _ => panic!("{} cannot have scheme", construct),
        };
    }

    pub fn type_(&self, construct: impl Into<Construct>) -> Option<&Type> {
        let construct = construct.into();
        match construct {
            Construct::CFunction(_)
            | Construct::LocalVar(_)
            | Construct::PatternVar(_)
            | Construct::Parameter(_)
            | Construct::Expr(_)
            | Construct::Pattern(_) => self.types.get(&construct),
            _ => None,
        }
    }

    pub fn set_type(&mut self, construct: impl Into<Construct>, ty: Type) {
        let construct = construct.into();
        match construct {
            Construct::CFunction(_)
            | Construct::LocalVar(_)
            | Construct::PatternVar(_)
            | Construct::Parameter(_)
            | Construct::Expr(_)
            | Construct::Pattern(_) => self.types.insert(construct, ty),
            _ => panic!("{} cannot have type", construct),
        };
    }

    pub fn instantiation(&self, construct: impl Into<Construct>) -> Option<&Instantiation> {
        let construct = construct.into();
        match construct {
            Construct::InstanceCon(_) | Construct::Expr(_) | Construct::Pattern(_) => {
                self.instantiations.get(&construct)
            }
            _ => None,
        }
    }

    pub fn set_instantiation(
        &mut self,
        construct: impl Into<Construct>,
        instantiation: Instantiation,
    ) {
        let construct = construct.into();
        match construct {
            Construct::InstanceCon(_) | Construct::Expr(_) | Construct::Pattern(_) => {
                self.instantiations.insert(construct, instantiation)
            }
            _ => panic!("{} cannot have instantiation", construct),
        };
    }
}
