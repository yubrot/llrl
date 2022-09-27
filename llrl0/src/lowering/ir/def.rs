use super::{Const, Ct, CtId, Rt, RtId, RtParam};
use derive_new::new;
use std::collections::BTreeMap;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Def {
    Alias(Ct),                    // this will be erased by normalizer
    AliasTable(AliasTable),       // this will be erased by normalizer
    Generic(Vec<CtId>, Box<Def>), // this will be erased by normalizer
    Data(Data),                   // this will be erased by data_expander
    Struct(Struct),
    Union(Union),
    Function(Function),
}

impl Def {
    pub fn generic(params: Vec<CtId>, def: Def) -> Self {
        if params.is_empty() {
            def
        } else {
            Self::Generic(params, Box::new(def))
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct AliasTable {
    map: BTreeMap<CtId, Ct>,
}

impl AliasTable {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            map: BTreeMap::new(),
        }
    }

    pub fn entries(&self) -> impl Iterator<Item = (CtId, &Ct)> {
        self.map.iter().map(|(id, ct)| (*id, ct))
    }

    pub fn entries_mut(&mut self) -> impl Iterator<Item = (CtId, &mut Ct)> {
        self.map.iter_mut().map(|(id, ct)| (*id, ct))
    }

    pub fn insert(&mut self, id: CtId, ct: Ct) {
        self.map.insert(id, ct);
    }

    pub fn get(&self, id: CtId) -> Option<&Ct> {
        self.map.get(&id)
    }
}

#[derive(PartialEq, Eq, Debug, Clone, new)]
pub struct Data {
    pub repr: DataRepr,
    pub cons: Vec<Vec<Ct>>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
pub enum DataRepr {
    Boxed,
    Value,
    C,
}

#[derive(PartialEq, Eq, Debug, Clone, new)]
pub struct Struct {
    pub repr: StructRepr,
    pub fields: Vec<Ct>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
pub enum StructRepr {
    Standard,
    C,
}

#[derive(PartialEq, Eq, Debug, Clone, new)]
pub struct Union {
    pub tys: Vec<Ct>,
}

#[derive(PartialEq, Eq, Debug, Clone, new)]
pub struct Function {
    pub env: Option<FunctionEnv>,
    pub params: Vec<RtParam>,
    pub ret: Ct,
    pub body: Rt,
    pub kind: FunctionKind,
}

impl Function {
    pub fn ty(&self) -> Ct {
        Ct::clos(
            self.params.iter().map(|p| p.ty.clone()).collect(),
            self.ret.clone(),
        )
    }

    pub fn main_ty() -> Ct {
        Ct::clos(Vec::new(), Ct::BOOL)
    }

    pub fn r#macro(param: RtParam, ret: Ct, body: Rt) -> Self {
        Self::new(None, vec![param], ret, body, FunctionKind::Macro)
    }

    pub fn main(mut inits: Vec<Init>) -> Self {
        let ret = if matches!(inits.last(), Some(init) if init.ty == Ct::BOOL) {
            inits.pop().unwrap()
        } else {
            Init::new(Ct::BOOL, Rt::Const(Const::bool(false)))
        };
        let stmts = inits.into_iter().map(|init| init.expr).collect();

        Self::new(
            None,
            Vec::new(),
            ret.ty,
            Rt::seq(stmts, ret.expr),
            FunctionKind::Main,
        )
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
pub enum FunctionKind {
    Standard,
    Macro,
    Transparent,
    Main,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash, new)]
pub struct FunctionEnv {
    pub id: RtId,
    pub elems: Vec<RtParam>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash, new)]
pub struct Init {
    pub ty: Ct,
    pub expr: Rt,
}
