use super::ir::*;
use crate::string;
use itertools::Itertools;
use std::fmt;

impl fmt::Display for CtId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#{}", self.index())
    }
}

impl fmt::Display for Ct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Id(id) => write!(f, "{}", id),
            Self::GenericInst(inst) => {
                write!(f, "{}[{}]", inst.0, inst.1.iter().format(", "))
            }
            Self::TableGet(get) => write!(f, "{}{}", get.0, get.1),
            Self::Ptr(ct) => write!(f, "ptr({})", ct),
            Self::Clos(clos) => {
                write!(f, "({}) -> {}", clos.0.iter().format(", "), clos.1)
            }
            Self::S(s) => write!(f, "s{}", s),
            Self::U(s) => write!(f, "u{}", s),
            Self::F32 => write!(f, "f32"),
            Self::F64 => write!(f, "f64"),
            Self::String => write!(f, "string"),
            Self::Char => write!(f, "char"),
            Self::Array(ct) => write!(f, "array({})", ct),
            Self::CapturedUse => write!(f, "captured-use"),
            Self::Unit => write!(f, "unit"),
            Self::Env => write!(f, "env"),
            Self::Syntax(ty) => write!(f, "syntax({})", ty),
            Self::Hole => write!(f, "_"),
        }
    }
}

impl fmt::Display for CtDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Alias(ct) => write!(f, "{}", ct),
            Self::AliasTable(table) => write!(f, "{}", table),
            Self::Generic(args, def) => {
                write!(f, "[{}] -> {}", args.iter().format(", "), def)
            }
            Self::Data(data) => write!(f, "{}", data),
            Self::Struct(s) => write!(f, "{}", s),
            Self::Union(u) => write!(f, "{}", u),
            Self::Function(function) => write!(f, "{}", function),
        }
    }
}

impl fmt::Display for AliasTable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "alias-table {{")?;
        for (id, ct) in self.entries() {
            writeln!(f, "  {} = {}", id, ct)?;
        }
        write!(f, "}}")
    }
}

impl fmt::Display for Data {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ty = self
            .cons
            .iter()
            .map(|con| format!("{{{}}}", con.iter().format(", ")))
            .format(", ");
        writeln!(f, "data {{")?;
        writeln!(f, "  repr = {}", self.repr)?;
        writeln!(f, "  ty = {{{}}}", ty)?;
        write!(f, "}}")
    }
}

impl fmt::Display for DataRepr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Boxed => write!(f, "boxed"),
            Self::Value => write!(f, "value"),
            Self::C => write!(f, "c"),
        }
    }
}

impl fmt::Display for Struct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "struct {{")?;
        writeln!(f, "  repr = {}", self.repr)?;
        writeln!(f, "  fields = {{{}}}", self.fields.iter().format(", "))?;
        write!(f, "}}")
    }
}

impl fmt::Display for StructRepr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StructRepr::Standard => write!(f, "standard"),
            StructRepr::C => write!(f, "c"),
        }
    }
}

impl fmt::Display for Union {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "union {{{}}}", self.tys.iter().format(", "))
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "function {{")?;
        writeln!(f, "  params = {{{}}}", self.params.iter().format(", "))?;
        match self.env {
            Some(ref env) => writeln!(f, "  env = {}", env)?,
            None => writeln!(f, "  env = none")?,
        }
        writeln!(f, "  ret = {}", self.ret)?;
        writeln!(f, "  body = {}", self.body)?;
        writeln!(f, "  kind = {}", self.kind)?;
        write!(f, "}}")
    }
}

impl fmt::Display for FunctionKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Standard => write!(f, "standard"),
            Self::Macro => write!(f, "macro"),
            Self::Transparent => write!(f, "transparent"),
            Self::Main => write!(f, "main"),
        }
    }
}

impl fmt::Display for FunctionEnv {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {{{}}}", self.id, self.elems.iter().format(", "))
    }
}

impl fmt::Display for FunctionParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.id, self.ty)
    }
}

impl fmt::Display for Init {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}] {}", self.ty, self.expr)
    }
}

impl fmt::Display for RtId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "${}", self.index())
    }
}

impl fmt::Display for Rt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Local(id) => write!(f, "{}", id),
            Self::LocalFun(id, args) => write!(f, "{}[{}]", id, args.iter().format(", ")),
            Self::Capture(id, None) => write!(f, "{}", id),
            Self::Capture(id, Some(env)) => write!(f, "{}{{{}}}", id, env),
            Self::Const(c) => write!(f, "{}", c),
            Self::Call(call) => write!(f, "{}({})", call.0, call.1.iter().format(", ")),
            Self::CCall(c_call) => write!(f, "@{}({})", c_call.0, c_call.2.iter().format(", ")),
            Self::Nullary(nullary) => write!(f, "{}()", nullary),
            Self::Unary(unary) => write!(f, "{}({})", unary.0, unary.1),
            Self::Binary(binary) => write!(f, "{}({}, {})", binary.0, binary.1, binary.2),
            Self::Ternary(ternary) => write!(
                f,
                "{}({}, {}, {})",
                ternary.0, ternary.1, ternary.2, ternary.3
            ),
            Self::Alloc(alloc) => write!(f, "alloc-{}({})", alloc.0, alloc.1),
            Self::AllocArray(alloc) => {
                write!(f, "alloc-array[{}]-{}({})", alloc.1, alloc.0, alloc.2)
            }
            Self::ConstructEnv(con) => write!(f, "env-{}({})", con.0, con.1.iter().format(", ")),
            Self::ConstructData(con) => {
                write!(f, "{}@{}({})", con.0, con.1, con.2.iter().format(", "))
            }
            Self::ConstructStruct(con) => {
                write!(f, "{}({})", con.0, con.1.iter().format(", "))
            }
            Self::ConstructSyntax(con) => write!(f, "syntax[{}]({})", con.1, con.2),
            Self::Seq(seq) => write!(f, "{}; {}", seq.0.iter().format("; "), seq.1),
            Self::If(if_) => {
                write!(f, "if({} then {} else {})", if_.0, if_.1, if_.2)
            }
            Self::While(while_) => write!(f, "while({} do {})", while_.0, while_.1),
            Self::And(and) => write!(f, "and({}, {})", and.0, and.1),
            Self::Or(or) => write!(f, "or({}, {})", or.0, or.1),
            Self::Match(m) => {
                write!(f, "match({} with {})", m.0, m.1.iter().format(", "))
            }
            Self::Return(ret) => write!(f, "return({})", ret),
            Self::Cont(id, args) => {
                if args.is_empty() {
                    write!(f, "cont({})", id)
                } else {
                    write!(f, "cont({} with {})", id, args.iter().format(", "))
                }
            }
            Self::Never => write!(f, "never"),
            Self::LetFunction(let_) => {
                write!(f, "letf({} in {})", let_.0.iter().format(", "), let_.1)
            }
            Self::LetVar(let_) => {
                write!(f, "letv({} in {})", let_.0.iter().format(", "), let_.1)
            }
            Self::LetCont(let_) => {
                write!(f, "letc({} in {})", let_.0.iter().format(", "), let_.1)
            }
        }
    }
}

impl fmt::Display for Nullary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Nullary::*;
        match self {
            Uninitialized(ty) => write!(f, "uninitialized[{}]", ty),
            Null(ty) => write!(f, "null[{}]", ty),
            GenId => write!(f, "genid"),
            SizeOf(ty) => write!(f, "size-of[{}]", ty),
            AlignOf(ty) => write!(f, "align-of[{}]", ty),
        }
    }
}

impl fmt::Display for Unary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Unary::*;
        match self {
            Not => write!(f, "not"),
            Load => write!(f, "load"),
            StructElem(ty, index) => write!(f, "{}.{}", ty, index),
            Reinterpret(from, to) => write!(f, "reinterpret[{} -> {}]", from, to),
            SyntaxBody(ty) => write!(f, "syntax-body[{}]", ty),
            Panic => write!(f, "panic"),
            BitCast(ty) => write!(f, "bitcast[{}]", ty),
            PtrToI => write!(f, "ptr-to-integer"),
            IToPtr(ty) => write!(f, "integer-to-ptr[{}]", ty),
            IComplement => write!(f, "integer-complement"),
            ITrunc(ty) => write!(f, "integer-trunc[{}]", ty),
            SExt(ty) => write!(f, "signed-ext[{}]", ty),
            UExt(ty) => write!(f, "unsinged-ext[{}]", ty),
            SToF(ty) => write!(f, "signed-to-float[{}]", ty),
            UToF(ty) => write!(f, "unsigned-to-float[{}]", ty),
            FToS(ty) => write!(f, "float-to-signed[{}]", ty),
            FToU(ty) => write!(f, "float-to-unsigned[{}]", ty),
            FTrunc(ty) => write!(f, "float-trunc[{}]", ty),
            FExt(ty) => write!(f, "float-ext[{}]", ty),
            RealCeil => write!(f, "float-ceil"),
            RealFloor => write!(f, "float-real"),
            RealTrunc => write!(f, "float-trunc"),
            RealRound => write!(f, "float-round"),
            MathSqrt => write!(f, "math-sqrt"),
            MathSin => write!(f, "math-sin"),
            MathCos => write!(f, "math-cos"),
            MathExp => write!(f, "math-exp"),
            MathLog => write!(f, "math-log"),
            StringPtr => write!(f, "string-ptr"),
            StringLength => write!(f, "string-length"),
            ArrayPtr => write!(f, "array-ptr"),
            ArrayLength => write!(f, "array-length"),
        }
    }
}

impl fmt::Display for Binary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Binary::*;
        match self {
            Store => write!(f, "store"),
            Offset => write!(f, "offset"),
            PtrEq => write!(f, "ptr-eq"),
            PtrLt => write!(f, "ptr-lt"),
            PtrLe => write!(f, "ptr-le"),
            PtrGt => write!(f, "ptr-gt"),
            PtrGe => write!(f, "ptr-ge"),
            IEq => write!(f, "integer-eq"),
            IShl => write!(f, "integer-shl"),
            IAShr => write!(f, "integer-ashr"),
            ILShr => write!(f, "integer-lshr"),
            IAnd => write!(f, "integer-and"),
            IOr => write!(f, "integer-or"),
            IXor => write!(f, "integer-xor"),
            SLt => write!(f, "signed-lt"),
            SLe => write!(f, "signed-le"),
            SGt => write!(f, "signed-gt"),
            SGe => write!(f, "signed-ge"),
            SAdd => write!(f, "signed-add"),
            SSub => write!(f, "signed-sub"),
            SMul => write!(f, "signed-mul"),
            SDiv => write!(f, "signed-div"),
            SRem => write!(f, "signed-rem"),
            ULt => write!(f, "unsigned-lt"),
            ULe => write!(f, "unsigned-le"),
            UGt => write!(f, "unsigned-gt"),
            UGe => write!(f, "unsigned-ge"),
            UAdd => write!(f, "unsigned-add"),
            USub => write!(f, "unsigned-sub"),
            UMul => write!(f, "unsigned-mul"),
            UDiv => write!(f, "unsigned-div"),
            URem => write!(f, "unsigned-rem"),
            FEq => write!(f, "float-eq"),
            FLt => write!(f, "float-lt"),
            FLe => write!(f, "float-le"),
            FGt => write!(f, "float-gt"),
            FGe => write!(f, "float-ge"),
            FAdd => write!(f, "float-add"),
            FSub => write!(f, "float-sub"),
            FMul => write!(f, "float-mul"),
            FDiv => write!(f, "float-div"),
            FRem => write!(f, "float-rem"),
            MathPow => write!(f, "math-pow"),
            StringConstruct => write!(f, "string-construct"),
            StringEq => write!(f, "string-eq"),
            StringCmp => write!(f, "string-cmp"),
            StringConcat => write!(f, "string-concat"),
            CharEq => write!(f, "char-eq"),
            ArrayLoad => write!(f, "array-load"),
            ArrayConstruct => write!(f, "array-construct"),
        }
    }
}

impl fmt::Display for Ternary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Ternary::*;
        match self {
            PtrCopy => write!(f, "ptr-copy"),
            PtrMove => write!(f, "ptr-move"),
            ArrayStore => write!(f, "array-store"),
        }
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Location::Heap => write!(f, "heap"),
            Location::Stack => write!(f, "stack"),
        }
    }
}

impl fmt::Display for RtClause {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} -> {}", self.pat, self.body)
    }
}

impl fmt::Display for RtPat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Var(id, ty, None) => write!(f, "{}: {}", id, ty),
            Self::Var(id, ty, Some(pat)) => write!(f, "as({}, {}): {}", id, pat, ty),
            Self::Wildcard => write!(f, "_"),
            Self::Ptr(pat) => write!(f, "ptr({})", pat),
            Self::Syntax(ty, body) => write!(f, "syntax[{}]({})", ty, body),
            Self::Data(ty, index, args) => {
                write!(f, "{}@{}({})", ty, index, args.iter().format(", "))
            }
            Self::Struct(ty, fields) => {
                write!(f, "{}({})", ty, fields.iter().format(", "))
            }
            Self::Reinterpret(from, to, x) => write!(f, "reinterpret[{} -> {}]({})", from, to, x),
            Self::Const(c) => write!(f, "{}", c),
        }
    }
}

impl fmt::Display for RtFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.id)?;
        if !self.ct_params.is_empty() {
            write!(f, "[{}]", self.ct_params.iter().format(", "))?;
        }
        write!(
            f,
            "({}) -> {} = {}",
            self.params.iter().format(", "),
            self.ret,
            self.body
        )
    }
}

impl fmt::Display for RtVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {} = {}", self.id, self.ty, self.init)
    }
}

impl fmt::Display for RtCont {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}({}) = {}",
            self.id,
            self.params.iter().format(", "),
            self.body
        )
    }
}

impl fmt::Display for Const {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Integer(ty, true, v) => write!(f, "<{} {}>", ty, *v as i64),
            Self::Integer(ty, false, v) => write!(f, "<{} {}>", ty, v),
            Self::FPNumber(ty, v) => write!(f, "<{} {}>", ty, v),
            Self::String(s) => write!(f, "<string \"{}\">", string::escape(s)),
            Self::Char(c) => write!(f, "<char #\\{}>", string::escape(&c.to_string())),
            Self::SyntaxSexp(ty, s) => write!(f, "<syntax[{}] '{}>", ty, s),
            Self::Unit => write!(f, "<unit>"),
        }
    }
}
