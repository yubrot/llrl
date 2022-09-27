#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
pub struct CtId(u32);

impl CtId {
    pub fn index(self) -> u32 {
        self.0
    }
}

#[derive(Debug, Clone)]
pub struct CtIdGen(CtId);

impl CtIdGen {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self(CtId(0))
    }

    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> CtId {
        let id = self.0;
        self.0 = CtId(id.0 + 1);
        id
    }
}

/// An expression that is evaluated at compile time.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash)]
pub enum Ct {
    Id(CtId),
    GenericInst(Box<(Ct, Vec<Ct>)>), // erased by normalizer
    TableGet(Box<(Ct, CtId)>),       // erased by normalizer
    Ptr(Box<Ct>),
    Clos(Box<(Vec<Ct>, Ct)>),
    S(usize),
    U(usize),
    F32,
    F64,
    String,
    Char,
    Array(Box<Ct>),
    CapturedUse,
    Unit,
    Env,
    Syntax(Box<Ct>),
    Hole,
}

impl Ct {
    pub fn id(&self) -> CtId {
        match self {
            Ct::Id(id) => *id,
            _ => panic!("Ct::id() on {}", self),
        }
    }

    pub fn generic_inst(ct: Self, args: Vec<Self>) -> Self {
        if args.is_empty() {
            ct
        } else {
            Self::GenericInst(Box::new((ct, args)))
        }
    }

    pub fn table_get(table: Self, id: CtId) -> Self {
        Self::TableGet(Box::new((table, id)))
    }

    pub fn ptr(ty: Self) -> Self {
        Self::Ptr(Box::new(ty))
    }

    pub fn clos(args: Vec<Self>, ret: Self) -> Self {
        Self::Clos(Box::new((args, ret)))
    }

    pub fn array(ty: Self) -> Self {
        Self::Array(Box::new(ty))
    }

    pub fn syntax(ty: Self) -> Self {
        Self::Syntax(Box::new(ty))
    }

    pub fn is_compatible_with(&self, other: &Self) -> bool {
        use Ct::*;
        match (self, other) {
            (Hole, _) | (_, Hole) => true,
            (Id(a), Id(b)) => a == b,
            (Ptr(a), Ptr(b)) => a.is_compatible_with(b),
            (Clos(a), Clos(b)) if a.0.len() == b.0.len() => {
                a.1.is_compatible_with(&b.1)
                    && a.0.iter().zip(&b.0).all(|(a, b)| a.is_compatible_with(b))
            }
            (S(a), S(b)) => a == b,
            (U(a), U(b)) => a == b,
            (a, b) => a == b,
        }
    }
}

impl Default for Ct {
    fn default() -> Self {
        Self::Unit
    }
}
