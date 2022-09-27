use std::borrow::Cow;

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
    Id(CtId),                        // this may be replaced by normalizer
    GenericInst(Box<(Ct, Vec<Ct>)>), // this will be erased by normalizer
    TableGet(Box<(Ct, CtId)>),       // this will be erased by normalizer
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

    // Due to rust-lang/rust#73448, using `Self` produces some warnings
    pub const BOOL: Ct = Ct::U(1);

    pub fn clos_ret(ct: Cow<Self>) -> Cow<Self> {
        match ct {
            Cow::Borrowed(Self::Clos(clos)) => Cow::Borrowed(&clos.1),
            Cow::Owned(Self::Clos(clos)) => Cow::Owned(clos.1),
            _ => panic!("Cannot extract the return type: {}", ct),
        }
    }

    pub fn ptr_elem(ct: Cow<Self>) -> Cow<Self> {
        match ct {
            Cow::Borrowed(Self::Ptr(ty)) => Cow::Borrowed(ty.as_ref()),
            Cow::Owned(Self::Ptr(ty)) => Cow::Owned(*ty),
            _ => panic!("Cannot extract the pointer element type: {}", ct),
        }
    }

    pub fn ptr_to_array(ct: Cow<Self>) -> Cow<Self> {
        match ct.into_owned() {
            Self::Ptr(ty) => Cow::Owned(Self::Array(ty)),
            ct => panic!("Cannot extract the array element type: {}", ct),
        }
    }

    pub fn array_to_ptr(ct: Cow<Self>) -> Cow<Self> {
        match ct.into_owned() {
            Self::Array(ty) => Cow::Owned(Self::Ptr(ty)),
            ct => panic!("Cannot extract the array element type: {}", ct),
        }
    }

    pub fn array_elem(ct: Cow<Self>) -> Cow<Self> {
        match ct {
            Cow::Borrowed(Self::Array(ty)) => Cow::Borrowed(ty.as_ref()),
            Cow::Owned(Self::Array(ty)) => Cow::Owned(*ty),
            _ => panic!("Cannot extract the array element type: {}", ct),
        }
    }

    pub fn syntax_body(ct: Cow<Self>) -> Cow<Self> {
        match ct {
            Cow::Borrowed(Self::Syntax(ty)) => Cow::Borrowed(ty.as_ref()),
            Cow::Owned(Self::Syntax(ty)) => Cow::Owned(*ty),
            _ => panic!("Cannot extract the syntax body type: {}", ct),
        }
    }
}

impl Default for Ct {
    fn default() -> Self {
        Self::Unit
    }
}
