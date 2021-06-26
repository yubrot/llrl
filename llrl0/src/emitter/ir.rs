//! Provides an intermediate representation for emitter.

use derive_new::new;
use ordered_float::OrderedFloat;
use std::collections::BTreeMap;

pub use crate::ast::builtin::{CapturedUse, Sexp, Syntax, SyntaxBody, SyntaxMetadata};

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
    pub fn new() -> Self {
        Self(CtId(0))
    }

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

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum CtDef {
    Alias(Ct),                      // erased by normalizer
    AliasTable(AliasTable),         // erased by normalizer
    Generic(Vec<CtId>, Box<CtDef>), // erased by normalizer
    Data(Data),                     // erased by data_expander
    Struct(Struct),
    Union(Union),
    Function(Function),
}

impl CtDef {
    pub fn generic(params: Vec<CtId>, def: CtDef) -> Self {
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
    pub params: Vec<FunctionParam>,
    pub ret: Ct,
    pub body: Rt,
    pub kind: FunctionKind,
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
    pub elems: Vec<FunctionParam>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash, new)]
pub struct FunctionParam {
    pub id: RtId,
    pub ty: Ct,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash, new)]
pub struct Init {
    pub ty: Ct,
    pub expr: Rt,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
pub struct RtId(u32);

impl RtId {
    pub fn index(self) -> u32 {
        self.0
    }

    pub const ARGC: Self = Self(-1 as i32 as u32);
    pub const ARGV: Self = Self(-2 as i32 as u32);
}

#[derive(Debug, Clone)]
pub struct RtIdGen(RtId);

impl RtIdGen {
    pub fn new() -> Self {
        Self(RtId(0))
    }

    pub fn next(&mut self) -> RtId {
        let id = self.0;
        self.0 = RtId(id.0 + 1);
        id
    }
}

/// An expression that is evaluated at runtime.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash)]
pub enum Rt {
    Local(RtId),
    LocalFun(RtId, Vec<Ct>), // erased by normalizer
    StaticFun(Ct, Option<Box<Rt>>),
    Const(Const),

    Call(Box<(Rt, Vec<Rt>)>),
    CCall(Box<(String, Ct, Vec<Rt>)>),
    Nullary(Nullary),
    Unary(Box<(Unary, Rt)>),
    Binary(Box<(Binary, Rt, Rt)>),
    Ternary(Box<(Ternary, Rt, Rt, Rt)>),

    Alloc(Box<(Location, Rt)>),
    AllocArray(Box<(Location, Ct, Rt)>),
    ConstructEnv(Box<(Location, Vec<Rt>)>),
    ConstructData(Box<(Ct, usize, Vec<Rt>)>), // erased by data_expander
    ConstructStruct(Box<(Ct, Vec<Rt>)>),
    ConstructSyntax(Box<(SyntaxMetadata, Rt)>),

    Seq(Box<(Vec<Rt>, Rt)>),
    If(Box<(Rt, Rt, Rt)>),
    While(Box<(Rt, Rt)>),
    And(Box<(Rt, Rt)>),              // erased by branch_expander
    Or(Box<(Rt, Rt)>),               // erased by branch_expander
    Match(Box<(Rt, Vec<RtClause>)>), // erased by branch_expander
    Return(Box<Rt>),
    Cont(RtId, Vec<Rt>),
    Never,

    LetFunction(Box<(Vec<RtFunction>, Rt)>), // erased by normalizer
    LetVar(Box<(Vec<RtVar>, Rt)>),
    LetCont(Box<(Vec<RtCont>, Rt)>),
}

impl Rt {
    pub fn capture(ct: Ct, env: Option<Self>) -> Self {
        Self::StaticFun(ct, env.map(Box::new))
    }

    pub fn call(callee: Self, args: Vec<Self>) -> Self {
        Self::Call(Box::new((callee, args)))
    }

    pub fn c_call(name: String, ty: Ct, args: Vec<Self>) -> Self {
        Self::CCall(Box::new((name, ty, args)))
    }

    pub fn autocall(rt: Self, autocall: bool) -> Self {
        if autocall {
            Self::call(rt, Vec::new())
        } else {
            rt
        }
    }

    pub fn nullary(nullary: Nullary) -> Self {
        Self::Nullary(nullary)
    }

    pub fn unary(unary: Unary, a: Self) -> Self {
        Self::Unary(Box::new((unary, a)))
    }

    pub fn binary(binary: Binary, a: Self, b: Self) -> Self {
        Self::Binary(Box::new((binary, a, b)))
    }

    pub fn ternary(ternary: Ternary, a: Self, b: Self, c: Self) -> Self {
        Self::Ternary(Box::new((ternary, a, b, c)))
    }

    pub fn alloc(location: Location, rt: Self) -> Self {
        Self::Alloc(Box::new((location, rt)))
    }

    pub fn alloc_array(location: Location, ty: Ct, len: Self) -> Self {
        Self::AllocArray(Box::new((location, ty, len)))
    }

    pub fn construct_env(location: Location, elems: Vec<Self>) -> Self {
        Self::ConstructEnv(Box::new((location, elems)))
    }

    pub fn construct_data(ty: Ct, index: usize, args: Vec<Self>) -> Self {
        Self::ConstructData(Box::new((ty, index, args)))
    }

    pub fn construct_struct(ty: Ct, args: Vec<Self>) -> Self {
        Self::ConstructStruct(Box::new((ty, args)))
    }

    pub fn construct_syntax(meta: SyntaxMetadata, body: Self) -> Self {
        Self::ConstructSyntax(Box::new((meta, body)))
    }

    pub fn seq(stmts: Vec<Self>, ret: Self) -> Self {
        if stmts.is_empty() {
            ret
        } else if let Rt::Seq(mut seq) = ret {
            let mut inner_stmts = std::mem::replace(&mut seq.0, stmts);
            seq.0.append(&mut inner_stmts);
            Self::Seq(seq)
        } else {
            Self::Seq(Box::new((stmts, ret)))
        }
    }

    pub fn if_(cond: Self, then: Self, else_: Self) -> Self {
        Self::If(Box::new((cond, then, else_)))
    }

    pub fn while_(cond: Self, body: Self) -> Self {
        Self::While(Box::new((cond, body)))
    }

    pub fn and(a: Self, b: Self) -> Self {
        Self::And(Box::new((a, b)))
    }

    pub fn or(a: Self, b: Self) -> Self {
        Self::Or(Box::new((a, b)))
    }

    pub fn match_(target: Self, clauses: Vec<RtClause>) -> Self {
        Self::Match(Box::new((target, clauses)))
    }

    pub fn return_(ret: Self) -> Self {
        Self::Return(Box::new(ret))
    }

    pub fn let_function(funs: Vec<RtFunction>, body: Self) -> Self {
        if funs.is_empty() {
            body
        } else {
            Self::LetFunction(Box::new((funs, body)))
        }
    }

    pub fn let_var(vars: Vec<RtVar>, body: Self) -> Self {
        if vars.is_empty() {
            body
        } else if let Rt::LetVar(mut let_) = body {
            let mut inner_vars = std::mem::replace(&mut let_.0, vars);
            let_.0.append(&mut inner_vars);
            Self::LetVar(let_)
        } else {
            Self::LetVar(Box::new((vars, body)))
        }
    }

    pub fn let_cont(conts: Vec<RtCont>, body: Self) -> Self {
        if conts.is_empty() {
            body
        } else if let Rt::LetCont(mut let_) = body {
            let mut inner_conts = std::mem::replace(&mut let_.0, conts);
            let_.0.append(&mut inner_conts);
            Self::LetCont(let_)
        } else {
            Self::LetCont(Box::new((conts, body)))
        }
    }
}

impl Default for Rt {
    fn default() -> Self {
        Self::Const(Const::default())
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash)]
pub enum Nullary {
    Uninitialized(Ct), // () -> 'a
    Null(Ct),          // () -> ptr('a)
    GenId,             // () -> string
    SizeOf(Ct),        // () -> u64
    AlignOf(Ct),       // () -> u64
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash)]
pub enum Unary {
    Not,                   // (bool) -> bool
    Load,                  // (ptr('a)) -> 'a
    StructElem(Ct, usize), // (#struct) -> #elem
    Reinterpret(Ct, Ct),   // (#from) -> #to
    SyntaxBody(Ct),        // (syntax) -> #syntax-body
    Panic,                 // (string) -> !
    BitCast(Ct),           // ('a) -> 'b
    PtrToI,                // (ptr('a)) -> u64
    IToPtr(Ct),            // (u64) -> ptr('a)
    IComplement,           // (iX) -> iX
    ITrunc(Ct),            // (iX) -> iX
    IPopCount,             // (iX) -> iX
    SExt(Ct),              // (sX) -> iX
    UExt(Ct),              // (uX) -> iX
    SToF(Ct),              // (uX) -> fX
    UToF(Ct),              // (uX) -> fX
    FToS(Ct),              // (fX) -> sX
    FToU(Ct),              // (fX) -> uX
    FTrunc(Ct),            // (fX) -> fX
    FExt(Ct),              // (fX) -> fX
    RealCeil,              // (fX) -> fX
    RealFloor,             // (fX) -> fX
    RealTrunc,             // (fX) -> fX
    RealRound,             // (fX) -> fX
    MathSqrt,              // (fX) -> fX
    MathSin,               // (fX) -> fX
    MathCos,               // (fX) -> fX
    MathExp,               // (fX) -> fX
    MathLog,               // (fX) -> fX
    StringPtr,             // (string) -> ptr(u8)
    StringLength,          // (string) -> u64
    ArrayPtr,              // (array('a)) -> ptr('a)
    ArrayLength,           // (array('a)) -> u64
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash)]
pub enum Binary {
    Store,           // ('a, ptr('a)) -> unit
    Offset,          // (I64, ptr('a)) -> ptr('a)
    PtrEq,           // (ptr('a), ptr('a)) -> bool
    PtrLt,           // (ptr('a), ptr('a)) -> bool
    PtrLe,           // (ptr('a), ptr('a)) -> bool
    PtrGt,           // (ptr('a), ptr('a)) -> bool
    PtrGe,           // (ptr('a), ptr('a)) -> bool
    IEq,             // (iX, iX) -> bool
    IShl,            // (iX, iX) -> iX
    IAShr,           // (iX, iX) -> iX
    ILShr,           // (iX, iX) -> iX
    IAnd,            // (iX, iX) -> iX
    IOr,             // (iX, iX) -> iX
    IXor,            // (iX, iX) -> iX
    SLt,             // (sX, sX) -> bool
    SLe,             // (sX, sX) -> bool
    SGt,             // (sX, sX) -> bool
    SGe,             // (sX, sX) -> bool
    SAdd,            // (sX, sX) -> sX
    SSub,            // (sX, sX) -> sX
    SMul,            // (sX, sX) -> sX
    SDiv,            // (sX, sX) -> sX
    SRem,            // (sX, sX) -> sX
    ULt,             // (uX, uX) -> bool
    ULe,             // (uX, uX) -> bool
    UGt,             // (uX, uX) -> bool
    UGe,             // (uX, uX) -> bool
    UAdd,            // (uX, uX) -> uX
    USub,            // (uX, uX) -> uX
    UMul,            // (uX, uX) -> uX
    UDiv,            // (uX, uX) -> uX
    URem,            // (uX, uX) -> uX
    FEq,             // (fX, fX) -> bool
    FLt,             // (iX, iX) -> bool
    FLe,             // (iX, iX) -> bool
    FGt,             // (iX, iX) -> bool
    FGe,             // (iX, iX) -> bool
    FAdd,            // (fX, fX) -> fX
    FSub,            // (fX, fX) -> fX
    FMul,            // (fX, fX) -> fX
    FDiv,            // (fX, fX) -> fX
    FRem,            // (fX, fX) -> fX
    MathPow,         // (fX ,fX) -> fX
    StringConstruct, // (ptr(u8), u64) -> string
    StringEq,        // (string, string) -> bool
    StringCmp,       // (string, string) -> i32
    StringConcat,    // (string, string) -> string
    CharEq,          // (char, char) -> bool
    ArrayConstruct,  // (ptr('a), u64) -> array('a)
    ArrayLoad,       // (u64, array('a)) -> 'a
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash)]
pub enum Ternary {
    PtrCopy,    // (ptr('a), u64, ptr('a)) -> unit
    PtrMove,    // (ptr('a), u64, ptr('a)) -> unit
    ArrayStore, // (u64, 'a, array('a)) -> unit
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
pub enum Location {
    Heap,
    Stack,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash, new)]
pub struct RtClause {
    pub pat: RtPat,
    pub body: Rt,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash)]
pub enum RtPat {
    Var(RtId, Ct, Option<Box<RtPat>>),
    Wildcard,
    Deref(Box<RtPat>),
    NonNull(Ct, Box<RtPat>),
    Null(Ct),
    Data(Ct, usize, Vec<RtPat>), // erased by data_expander
    Struct(Ct, Vec<RtPat>),
    Reinterpret(Ct, Ct, Box<RtPat>),
    Syntax(Ct, Box<RtPat>),
    Const(Const),
}

impl RtPat {
    pub fn deref(pat: Self) -> Self {
        Self::Deref(Box::new(pat))
    }

    pub fn non_null(ty: Ct, pat: Self) -> Self {
        Self::NonNull(ty, Box::new(pat))
    }
}

impl Default for RtPat {
    fn default() -> Self {
        Self::Const(Const::default())
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash, new)]
pub struct RtFunction {
    pub id: RtId,
    pub ct_params: Vec<CtId>,
    pub params: Vec<FunctionParam>,
    pub ret: Ct,
    pub body: Rt,
}

impl RtFunction {
    pub fn ty(&self) -> Ct {
        Ct::clos(
            self.params.iter().map(|param| param.ty.clone()).collect(),
            self.ret.clone(),
        )
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash, new)]
pub struct RtVar {
    pub id: RtId,
    pub ty: Ct,
    pub init: Rt,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash, new)]
pub struct RtCont {
    pub id: RtId,
    pub params: Vec<FunctionParam>,
    pub body: Rt,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash)]
pub enum Const {
    Integer(Ct, bool, u64),
    FPNumber(Ct, OrderedFloat<f64>),
    String(String),
    Char(char),
    SyntaxSexp(Ct, Box<Syntax<Sexp>>),
    Unit,
}

impl Const {
    pub fn bool(value: bool) -> Self {
        Self::Integer(Ct::U(1), false, if value { 1 } else { 0 })
    }

    pub fn ty(&self) -> Ct {
        match self {
            Self::Integer(ty, _, _) => ty.clone(),
            Self::FPNumber(ty, _) => ty.clone(),
            Self::String(_) => Ct::String,
            Self::Char(_) => Ct::Char,
            Self::SyntaxSexp(ty, _) => Ct::syntax(ty.clone()),
            Self::Unit => Ct::Unit,
        }
    }
}

impl Default for Const {
    fn default() -> Self {
        Self::Unit
    }
}
