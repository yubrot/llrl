use super::{Ct, CtId, Sexp, Syntax, SyntaxMetadata};
use derive_new::new;
use ordered_float::OrderedFloat;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
pub struct RtId(u32);

impl RtId {
    pub fn index(self) -> u32 {
        self.0
    }
}

#[derive(Debug, Clone)]
pub struct RtIdGen(RtId);

impl RtIdGen {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self(RtId(0))
    }

    #[allow(clippy::should_implement_trait)]
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
    StackStatic,
    StackDynamic,
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
    pub params: Vec<RtParam>,
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
    pub params: Vec<RtParam>,
    pub body: Rt,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash, new)]
pub struct RtParam {
    pub id: RtId,
    pub ty: Ct,
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
