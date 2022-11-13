use super::{Ct, CtId, Sexp, Syntax, SyntaxMetadata};
use derive_new::new;
use ordered_float::OrderedFloat;
use std::borrow::Cow;

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
    Var(RtId, Ct),
    LocalFun(Box<RtLocalFunInst>), // this will be erased by normalizer
    StaticFun(Box<RtStaticFunCapture>),
    Const(Const),

    Call(Box<RtCall>),
    CCall(Box<RtCCall>),
    ContCall(Box<RtContCall>),
    Nullary(Nullary),
    Unary(Box<(Unary, Rt)>),
    Binary(Box<(Binary, Rt, Rt)>),
    Ternary(Box<(Ternary, Rt, Rt, Rt)>),

    Alloc(Box<(Location, Rt)>),
    AllocArray(Box<(Location, Ct, Rt)>),
    ConstructEnv(Box<(Location, Vec<Rt>)>),
    ConstructData(Box<(Ct, usize, Vec<Rt>)>), // this will be erased by data_expander
    ConstructStruct(Box<(Ct, Vec<Rt>)>),
    ConstructSyntax(Box<(SyntaxMetadata, Rt)>),

    Seq(Box<(Vec<Rt>, Rt)>),
    If(Box<(Rt, Rt, Rt)>),
    While(Box<(Rt, Rt)>),
    And(Box<(Rt, Rt)>),              // this will be erased by branch_expander
    Or(Box<(Rt, Rt)>),               // this will be erased by branch_expander
    Match(Box<(Rt, Vec<RtClause>)>), // this will be erased by branch_expander
    Return(Box<Rt>),
    Never,

    LetVar(Box<(Vec<RtVar>, Rt)>),
    LetLocalFun(Box<(Vec<RtLocalFun>, Rt)>), // this will be erased by normalizer
    LetCont(Box<(Vec<RtCont>, Rt)>),
}

impl Rt {
    pub fn local_fun(fun: RtId, args: Vec<Ct>, ty: Ct) -> Self {
        Self::LocalFun(Box::new(RtLocalFunInst::new(fun, args, ty)))
    }

    pub fn static_fun(fun: Ct, ty: Ct, env: Option<Self>) -> Self {
        Self::StaticFun(Box::new(RtStaticFunCapture::new(fun, ty, env)))
    }

    pub fn call(callee: Self, args: Vec<Self>) -> Self {
        Self::Call(Box::new(RtCall::new(callee, args)))
    }

    pub fn c_call(name: String, ty: Ct, args: Vec<Self>) -> Self {
        Self::CCall(Box::new(RtCCall::new(name, ty, args)))
    }

    pub fn cont_call(id: RtId, args: Vec<Self>, ret: Option<Ct>) -> Self {
        Self::ContCall(Box::new(RtContCall::new(id, args, ret)))
    }

    pub fn autocall(fun: Ct, ty: Ct, autocall: bool) -> Self {
        if autocall {
            Self::call(
                Rt::static_fun(fun, Ct::clos(Vec::new(), ty), None),
                Vec::new(),
            )
        } else {
            Rt::static_fun(fun, ty, None)
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

    pub fn let_local_fun(funs: Vec<RtLocalFun>, body: Self) -> Self {
        if funs.is_empty() {
            body
        } else {
            Self::LetLocalFun(Box::new((funs, body)))
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

    /// Get the type of value obtained when this Rt is evaluated.
    pub fn ty(&self) -> Option<Cow<Ct>> {
        Some(match self {
            Rt::Var(_, ty) => Cow::Borrowed(ty),
            Rt::LocalFun(inst) => Cow::Borrowed(&inst.ty),
            Rt::StaticFun(capture) => Cow::Borrowed(&capture.ty),
            Rt::Const(c) => c.ty(),
            Rt::Call(call) => Ct::clos_ret(call.callee.ty()?),
            Rt::CCall(call) => Ct::clos_ret(Cow::Borrowed(&call.ty)),
            Rt::ContCall(call) => Cow::Borrowed(call.ret.as_ref()?),
            Rt::Nullary(nullary) => nullary.ty()?,
            Rt::Unary(unary) => unary.0.ty(&unary.1)?,
            Rt::Binary(binary) => binary.0.ty(&binary.1, &binary.2)?,
            Rt::Ternary(ternary) => ternary.0.ty(&ternary.1, &ternary.2, &ternary.3)?,
            Rt::Alloc(alloc) => Cow::Owned(Ct::ptr(alloc.1.ty()?.into_owned())),
            Rt::AllocArray(alloc) => Cow::Owned(Ct::array(alloc.1.clone())),
            Rt::ConstructEnv(_) => Cow::Owned(Ct::Env),
            Rt::ConstructData(c) => Cow::Borrowed(&c.0),
            Rt::ConstructStruct(c) => Cow::Borrowed(&c.0),
            Rt::ConstructSyntax(c) => Cow::Owned(Ct::syntax(c.1.ty()?.into_owned())),
            Rt::Seq(seq) => seq.1.ty()?,
            Rt::If(if_) => if_.1.ty().or_else(|| if_.2.ty())?,
            Rt::While(_) => Cow::Owned(Ct::Unit),
            Rt::And(_) | Rt::Or(_) => Cow::Owned(Ct::U(1)),
            Rt::Match(m) => m.1.iter().find_map(|c| c.body.ty())?,
            Rt::Return(_) => return None,
            Rt::Never => return None,
            Rt::LetLocalFun(let_local_fun) => let_local_fun.1.ty()?,
            Rt::LetVar(let_var) => let_var.1.ty()?,
            Rt::LetCont(let_cont) => let_cont.1.ty()?,
        })
    }
}

impl Default for Rt {
    fn default() -> Self {
        Self::Const(Const::default())
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash, new)]
pub struct RtCall {
    pub callee: Rt,
    pub args: Vec<Rt>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash, new)]
pub struct RtCCall {
    pub sym: String,
    pub ty: Ct,
    pub args: Vec<Rt>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash, new)]
pub struct RtContCall {
    pub cont: RtId,
    pub args: Vec<Rt>,
    pub ret: Option<Ct>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash, new)]
pub struct RtLocalFunInst {
    pub fun: RtId,
    pub args: Vec<Ct>,
    pub ty: Ct,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash, new)]
pub struct RtStaticFunCapture {
    pub fun: Ct, // this will be simplified to Ct::Id(_) by normalizer
    pub ty: Ct,
    pub env: Option<Rt>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash)]
pub enum Nullary {
    Uninitialized(Ct), // () -> 'a
    Null(Ct),          // () -> ptr('a)
    GenId,             // () -> string
    SizeOf(Ct),        // () -> u64
    AlignOf(Ct),       // () -> u64
}

impl Nullary {
    fn ty(&self) -> Option<Cow<Ct>> {
        use Nullary::*;
        Some(match self {
            Uninitialized(ty) => Cow::Borrowed(ty),
            Null(ty) => Cow::Owned(Ct::ptr(ty.clone())),
            GenId => Cow::Owned(Ct::String),
            SizeOf(_) | AlignOf(_) => Cow::Owned(Ct::U(64)),
        })
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash)]
pub enum Unary {
    Not,                   // (bool) -> bool
    Load,                  // (ptr('a)) -> 'a
    StructElem(Ct, usize), // ('struct) -> #elem
    Reinterpret(Ct),       // ('from) -> #to
    SyntaxBody,            // ('syntax) -> 'syntax-body
    Panic,                 // (string) -> !
    BitCast(Ct),           // ('a) -> #b
    PtrToI,                // (ptr('a)) -> u64
    IToPtr(Ct),            // (u64) -> ptr(#a)
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

impl Unary {
    fn ty<'a>(&'a self, a: &'a Rt) -> Option<Cow<'a, Ct>> {
        use Unary::*;
        Some(match self {
            Not => Cow::Owned(Ct::BOOL),
            Load => Ct::ptr_elem(a.ty()?),
            StructElem(elem, _) => Cow::Borrowed(elem),
            Reinterpret(to) => Cow::Borrowed(to),
            SyntaxBody => Ct::syntax_body(a.ty()?),
            Panic => return None,
            BitCast(ty) => Cow::Borrowed(ty),
            PtrToI => Cow::Owned(Ct::U(64)),
            IToPtr(ty) => Cow::Owned(Ct::ptr(ty.clone())),
            ITrunc(ty) | SExt(ty) | UExt(ty) | SToF(ty) | UToF(ty) | FToS(ty) | FToU(ty)
            | FTrunc(ty) | FExt(ty) => Cow::Borrowed(ty),
            IComplement | IPopCount | RealCeil | RealFloor | RealTrunc | RealRound | MathSqrt
            | MathSin | MathCos | MathExp | MathLog => a.ty()?,
            StringPtr => Cow::Owned(Ct::ptr(Ct::U(8))),
            StringLength => Cow::Owned(Ct::U(64)),
            ArrayPtr => Cow::Owned(Ct::ptr(Ct::array_elem(a.ty()?).into_owned())),
            ArrayLength => Cow::Owned(Ct::U(64)),
        })
    }
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
    ArrayConstruct,  // (ptr('a), u64) -> array('a)
    ArrayLoad,       // (u64, array('a)) -> 'a
}

impl Binary {
    fn ty<'a>(&'a self, a: &'a Rt, b: &'a Rt) -> Option<Cow<'a, Ct>> {
        use Binary::*;
        Some(match self {
            Store => Cow::Owned(Ct::Unit),
            Offset => b.ty()?,
            IShl | IAShr | ILShr | IAnd | IOr | IXor | SAdd | SSub | SMul | SDiv | SRem | UAdd
            | USub | UMul | UDiv | URem | FGe | FAdd | FSub | FMul | FDiv | FRem | MathPow => {
                a.ty()?
            }
            PtrEq | PtrLt | PtrLe | PtrGt | PtrGe | IEq | SLt | SLe | SGt | SGe | ULt | ULe
            | UGt | UGe | FEq | FLt | FLe | FGt | StringEq => Cow::Owned(Ct::BOOL),
            StringConstruct => Cow::Owned(Ct::String),
            StringCmp => Cow::Owned(Ct::S(32)),
            StringConcat => Cow::Owned(Ct::String),
            ArrayConstruct => Cow::Owned(Ct::array(Ct::ptr_elem(a.ty()?).into_owned())),
            ArrayLoad => Ct::array_elem(b.ty()?),
        })
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash)]
pub enum Ternary {
    PtrCopy,    // (ptr('a), u64, ptr('a)) -> unit
    PtrMove,    // (ptr('a), u64, ptr('a)) -> unit
    ArrayStore, // (u64, 'a, array('a)) -> unit
}

impl Ternary {
    fn ty<'a>(&'a self, _a: &'a Rt, _b: &'a Rt, _c: &'a Rt) -> Option<Cow<'a, Ct>> {
        Some(Cow::Owned(Ct::Unit))
    }
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
    Wildcard(Ct),
    Deref(Box<RtPat>),
    NonNull(Box<RtPat>),
    Null(Ct),
    Data(Ct, usize, Vec<RtPat>), // this will be erased by data_expander
    Struct(Ct, Vec<RtPat>),
    Reinterpret(Ct, Box<RtPat>),
    Syntax(Box<RtPat>),
    Const(Const),
}

impl RtPat {
    /// Get the target type of this pattern.
    pub fn ty(&self) -> Cow<Ct> {
        match self {
            Self::Var(_, ty, _) => Cow::Borrowed(ty),
            Self::Wildcard(ty) => Cow::Borrowed(ty),
            Self::Deref(pat) => Cow::Owned(Ct::ptr(pat.ty().into_owned())),
            Self::NonNull(pat) => Cow::Owned(Ct::ptr(pat.ty().into_owned())),
            Self::Null(ty) => Cow::Owned(Ct::ptr(ty.to_owned())),
            Self::Data(ty, _, _) => Cow::Borrowed(ty),
            Self::Struct(ty, _) => Cow::Borrowed(ty),
            Self::Reinterpret(ty, _) => Cow::Borrowed(ty),
            Self::Syntax(pat) => Cow::Owned(Ct::syntax(pat.ty().into_owned())),
            Self::Const(c) => c.ty(),
        }
    }

    pub fn deref(pat: Self) -> Self {
        Self::Deref(Box::new(pat))
    }

    pub fn non_null(pat: Self) -> Self {
        Self::NonNull(Box::new(pat))
    }

    pub fn reinterpret(ty: Ct, pat: Self) -> Self {
        Self::Reinterpret(ty, Box::new(pat))
    }

    pub fn syntax(pat: Self) -> Self {
        Self::Syntax(Box::new(pat))
    }
}

impl Default for RtPat {
    fn default() -> Self {
        Self::Const(Const::default())
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash, new)]
pub struct RtLocalFun {
    pub id: RtId,
    pub ct_params: Vec<CtId>,
    pub params: Vec<RtParam>,
    pub ret: Ct,
    pub body: Rt,
}

impl RtLocalFun {
    pub fn ty(params: &[RtParam], ret: &Ct) -> Ct {
        Ct::clos(
            params.iter().map(|param| param.ty.clone()).collect(),
            ret.clone(),
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
        Self::Integer(Ct::BOOL, false, u64::from(value))
    }

    pub fn syntax_sexp(s_ty: Ct, s: Syntax<Sexp>) -> Self {
        Self::SyntaxSexp(s_ty, Box::new(s))
    }

    /// Get the type of this constant.
    pub fn ty(&self) -> Cow<Ct> {
        match self {
            Self::Integer(ty, _, _) => Cow::Borrowed(ty),
            Self::FPNumber(ty, _) => Cow::Borrowed(ty),
            Self::String(_) => Cow::Owned(Ct::String),
            Self::Char(_) => Cow::Owned(Ct::Char),
            Self::SyntaxSexp(ty, _) => Cow::Owned(Ct::syntax(ty.clone())),
            Self::Unit => Cow::Owned(Ct::Unit),
        }
    }
}

impl Default for Const {
    fn default() -> Self {
        Self::Unit
    }
}
