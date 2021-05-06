use super::ir::*;
use std::collections::HashSet;
use std::iter::FromIterator;

pub fn traverse<T: Traverser>(src: &impl Traverse, traverser: &mut T) -> Result<(), T::Error> {
    Traverse::traverse(src, traverser)
}

pub trait Traverser: Sized {
    type Error;

    fn traverse<T: Traverse>(&mut self, src: &T) -> Result<(), Self::Error> {
        src.traverse(self)
    }

    fn before_ct(&mut self, _ct: &Ct) -> Result<bool, Self::Error> {
        Ok(true)
    }

    fn after_ct(&mut self, _ct: &Ct) -> Result<(), Self::Error> {
        Ok(())
    }

    fn before_rt(&mut self, _rt: &Rt) -> Result<bool, Self::Error> {
        Ok(true)
    }

    fn after_rt(&mut self, _rt: &Rt) -> Result<(), Self::Error> {
        Ok(())
    }

    fn before_rt_pat(&mut self, _pat: &RtPat) -> Result<bool, Self::Error> {
        Ok(true)
    }

    fn after_rt_pat(&mut self, _pat: &RtPat) -> Result<(), Self::Error> {
        Ok(())
    }
    fn after_rt_use(&mut self, _id: RtId) -> Result<(), Self::Error> {
        Ok(())
    }

    fn after_rt_def(&mut self, _id: RtId, _ty: impl FnOnce() -> Ct) -> Result<(), Self::Error> {
        Ok(())
    }
}

pub trait Traverse {
    fn traverse<T: Traverser>(&self, traverser: &mut T) -> Result<(), T::Error>;
}

impl<A: Traverse> Traverse for Box<A> {
    fn traverse<T: Traverser>(&self, traverser: &mut T) -> Result<(), T::Error> {
        traverser.traverse(&**self)
    }
}

impl<A: Traverse> Traverse for Option<A> {
    fn traverse<T: Traverser>(&self, traverser: &mut T) -> Result<(), T::Error> {
        match self {
            Some(x) => traverser.traverse(x),
            _ => Ok(()),
        }
    }
}

impl<A: Traverse> Traverse for Vec<A> {
    fn traverse<T: Traverser>(&self, traverser: &mut T) -> Result<(), T::Error> {
        for x in self.iter() {
            traverser.traverse(x)?;
        }
        Ok(())
    }
}

impl Traverse for Ct {
    fn traverse<T: Traverser>(&self, traverser: &mut T) -> Result<(), T::Error> {
        if traverser.before_ct(self)? {
            match self {
                Self::Id(_) => {}
                Self::GenericInst(inst) => {
                    traverser.traverse(&inst.0)?;
                    traverser.traverse(&inst.1)?;
                }
                Self::TableGet(get) => {
                    traverser.traverse(&get.0)?;
                }
                Self::Ptr(ty) => {
                    traverser.traverse(ty)?;
                }
                Self::Clos(clos) => {
                    traverser.traverse(&clos.0)?;
                    traverser.traverse(&clos.1)?;
                }
                Self::Syntax(ty) => {
                    traverser.traverse(ty)?;
                }
                Self::Array(ty) => {
                    traverser.traverse(ty)?;
                }
                Self::S(_)
                | Self::U(_)
                | Self::F32
                | Self::F64
                | Self::String
                | Self::Char
                | Self::CapturedUse
                | Self::Unit
                | Self::Env
                | Self::Hole => {}
            };
            traverser.after_ct(self)
        } else {
            Ok(())
        }
    }
}

impl Traverse for CtDef {
    fn traverse<T: Traverser>(&self, traverser: &mut T) -> Result<(), T::Error> {
        match self {
            Self::Alias(ct) => traverser.traverse(ct),
            Self::AliasTable(table) => traverser.traverse(table),
            Self::Generic(_, ret) => traverser.traverse(ret),
            Self::Data(ty) => traverser.traverse(ty),
            Self::Struct(ty) => traverser.traverse(ty),
            Self::Union(ty) => traverser.traverse(ty),
            Self::Function(f) => traverser.traverse(f),
        }
    }
}

impl Traverse for AliasTable {
    fn traverse<T: Traverser>(&self, traverser: &mut T) -> Result<(), T::Error> {
        for (_, ct) in self.entries() {
            traverser.traverse(ct)?;
        }
        Ok(())
    }
}

impl Traverse for Data {
    fn traverse<T: Traverser>(&self, traverser: &mut T) -> Result<(), T::Error> {
        traverser.traverse(&self.cons)
    }
}

impl Traverse for Struct {
    fn traverse<T: Traverser>(&self, traverser: &mut T) -> Result<(), T::Error> {
        traverser.traverse(&self.fields)
    }
}

impl Traverse for Union {
    fn traverse<T: Traverser>(&self, traverser: &mut T) -> Result<(), T::Error> {
        traverser.traverse(&self.tys)
    }
}

impl Traverse for Function {
    fn traverse<T: Traverser>(&self, traverser: &mut T) -> Result<(), T::Error> {
        traverser.traverse(&self.env)?;
        traverser.traverse(&self.params)?;
        traverser.traverse(&self.ret)?;
        traverser.traverse(&self.body)
    }
}

impl Traverse for FunctionEnv {
    fn traverse<T: Traverser>(&self, traverser: &mut T) -> Result<(), T::Error> {
        traverser.after_rt_def(self.id, || Ct::Env)?;
        traverser.traverse(&self.elems)
    }
}

impl Traverse for FunctionParam {
    fn traverse<T: Traverser>(&self, traverser: &mut T) -> Result<(), T::Error> {
        traverser.traverse(&self.ty)?;
        traverser.after_rt_def(self.id, || self.ty.clone())
    }
}

impl Traverse for Init {
    fn traverse<T: Traverser>(&self, traverser: &mut T) -> Result<(), T::Error> {
        traverser.traverse(&self.ty)?;
        traverser.traverse(&self.expr)
    }
}

impl Traverse for Rt {
    fn traverse<T: Traverser>(&self, traverser: &mut T) -> Result<(), T::Error> {
        if traverser.before_rt(self)? {
            match self {
                Self::Local(id) => {
                    traverser.after_rt_use(*id)?;
                }
                Self::LocalFun(id, args) => {
                    traverser.after_rt_use(*id)?;
                    traverser.traverse(args)?;
                }
                Self::Capture(ct, env) => {
                    traverser.traverse(ct)?;
                    traverser.traverse(env)?;
                }
                Self::Const(c) => {
                    traverser.traverse(c)?;
                }
                Self::Call(call) => {
                    traverser.traverse(&call.0)?;
                    traverser.traverse(&call.1)?;
                }
                Self::CCall(c_call) => {
                    traverser.traverse(&c_call.1)?;
                    traverser.traverse(&c_call.2)?;
                }
                Self::Nullary(nullary) => {
                    traverser.traverse(nullary)?;
                }
                Self::Unary(unary) => {
                    traverser.traverse(&unary.0)?;
                    traverser.traverse(&unary.1)?;
                }
                Self::Binary(binary) => {
                    traverser.traverse(&binary.0)?;
                    traverser.traverse(&binary.1)?;
                    traverser.traverse(&binary.2)?;
                }
                Self::Ternary(ternary) => {
                    traverser.traverse(&ternary.0)?;
                    traverser.traverse(&ternary.1)?;
                    traverser.traverse(&ternary.2)?;
                    traverser.traverse(&ternary.3)?;
                }
                Self::Alloc(alloc) => {
                    traverser.traverse(&alloc.1)?;
                }
                Self::AllocArray(alloc) => {
                    traverser.traverse(&alloc.1)?;
                    traverser.traverse(&alloc.2)?;
                }
                Self::ConstructEnv(con) => {
                    traverser.traverse(&con.1)?;
                }
                Self::ConstructData(con) => {
                    traverser.traverse(&con.0)?;
                    traverser.traverse(&con.2)?;
                }
                Self::ConstructStruct(con) => {
                    traverser.traverse(&con.0)?;
                    traverser.traverse(&con.1)?;
                }
                Self::ConstructSyntax(con) => {
                    traverser.traverse(&con.1)?;
                    traverser.traverse(&con.2)?;
                }
                Self::Seq(seq) => {
                    traverser.traverse(&seq.0)?;
                    traverser.traverse(&seq.1)?;
                }
                Self::If(if_) => {
                    traverser.traverse(&if_.0)?;
                    traverser.traverse(&if_.1)?;
                    traverser.traverse(&if_.2)?;
                }
                Self::While(while_) => {
                    traverser.traverse(&while_.0)?;
                    traverser.traverse(&while_.1)?;
                }
                Self::And(ab) | Self::Or(ab) => {
                    traverser.traverse(&ab.0)?;
                    traverser.traverse(&ab.1)?;
                }
                Self::Match(match_) => {
                    traverser.traverse(&match_.0)?;
                    traverser.traverse(&match_.1)?;
                }
                Self::Return(ret) => {
                    traverser.traverse(ret)?;
                }
                Self::Cont(_, args) => {
                    traverser.traverse(args)?;
                }
                Self::Never => {}
                Self::LetFunction(let_) => {
                    traverser.traverse(&let_.0)?;
                    traverser.traverse(&let_.1)?;
                }
                Self::LetVar(let_) => {
                    traverser.traverse(&let_.0)?;
                    traverser.traverse(&let_.1)?;
                }
                Self::LetCont(let_) => {
                    traverser.traverse(&let_.0)?;
                    traverser.traverse(&let_.1)?;
                }
            };
            traverser.after_rt(self)
        } else {
            Ok(())
        }
    }
}

impl Traverse for Nullary {
    fn traverse<T: Traverser>(&self, traverser: &mut T) -> Result<(), T::Error> {
        use Nullary::*;
        match self {
            Uninitialized(ty) => {
                traverser.traverse(ty)?;
            }
            Null(ty) => {
                traverser.traverse(ty)?;
            }
            GenId => {}
            SizeOf(ty) | AlignOf(ty) => {
                traverser.traverse(ty)?;
            }
        }
        Ok(())
    }
}

impl Traverse for Unary {
    fn traverse<T: Traverser>(&self, traverser: &mut T) -> Result<(), T::Error> {
        use Unary::*;
        match self {
            Not => {}
            Load => {}
            StructElem(ty, _) => {
                traverser.traverse(ty)?;
            }
            Reinterpret(from, to) => {
                traverser.traverse(from)?;
                traverser.traverse(to)?;
            }
            SyntaxBody(ty) => {
                traverser.traverse(ty)?;
            }
            Panic => {}
            BitCast(ty) => {
                traverser.traverse(ty)?;
            }
            PtrToI => {}
            IToPtr(ty) => {
                traverser.traverse(ty)?;
            }
            IComplement => {}
            ITrunc(ty) => {
                traverser.traverse(ty)?;
            }
            IPopCount => {}
            SExt(ty) | SToF(ty) => {
                traverser.traverse(ty)?;
            }
            UExt(ty) | UToF(ty) => {
                traverser.traverse(ty)?;
            }
            FToS(ty) | FToU(ty) | FTrunc(ty) | FExt(ty) => {
                traverser.traverse(ty)?;
            }
            RealCeil | RealFloor | RealTrunc | RealRound => {}
            MathSqrt | MathSin | MathCos | MathExp | MathLog => {}
            StringPtr | StringLength => {}
            ArrayPtr | ArrayLength => {}
        }
        Ok(())
    }
}

impl Traverse for Binary {
    fn traverse<T: Traverser>(&self, _: &mut T) -> Result<(), T::Error> {
        use Binary::*;
        match self {
            Store => {}
            Offset => {}
            IEq | IShl | IAShr | ILShr | IAnd | IOr | IXor => {}
            SLt | SLe | SGt | SGe | SAdd | SSub | SMul | SDiv | SRem => {}
            ULt | ULe | UGt | UGe | UAdd | USub | UMul | UDiv | URem => {}
            FEq | FLt | FLe | FGt | FGe | FAdd | FSub | FMul | FDiv | FRem => {}
            MathPow => {}
            StringConstruct | StringEq | StringCmp | StringConcat => {}
            CharEq => {}
            PtrEq | PtrLt | PtrLe | PtrGt | PtrGe => {}
            ArrayConstruct | ArrayLoad => {}
        }
        Ok(())
    }
}

impl Traverse for Ternary {
    fn traverse<T: Traverser>(&self, _: &mut T) -> Result<(), T::Error> {
        use Ternary::*;
        match self {
            PtrCopy | PtrMove => {}
            ArrayStore => {}
        }
        Ok(())
    }
}

impl Traverse for RtClause {
    fn traverse<T: Traverser>(&self, traverser: &mut T) -> Result<(), T::Error> {
        traverser.traverse(&self.pat)?;
        traverser.traverse(&self.body)
    }
}

impl Traverse for RtPat {
    fn traverse<T: Traverser>(&self, traverser: &mut T) -> Result<(), T::Error> {
        if traverser.before_rt_pat(self)? {
            match self {
                Self::Var(id, ty, p) => {
                    traverser.traverse(ty)?;
                    traverser.after_rt_def(*id, || ty.clone())?;
                    traverser.traverse(p)?;
                }
                Self::Wildcard => {}
                Self::Deref(x) => {
                    traverser.traverse(x)?;
                }
                Self::NonNull(ty, x) => {
                    traverser.traverse(ty)?;
                    traverser.traverse(x)?;
                }
                Self::Null(ty) => {
                    traverser.traverse(ty)?;
                }
                Self::Data(ty, _index, args) => {
                    traverser.traverse(ty)?;
                    traverser.traverse(args)?;
                }
                Self::Struct(ty, fields) => {
                    traverser.traverse(ty)?;
                    traverser.traverse(fields)?;
                }
                Self::Reinterpret(from, to, x) => {
                    traverser.traverse(from)?;
                    traverser.traverse(to)?;
                    traverser.traverse(x)?;
                }
                Self::Syntax(ty, body) => {
                    traverser.traverse(ty)?;
                    traverser.traverse(body)?;
                }
                Self::Const(c) => {
                    traverser.traverse(c)?;
                }
            }
            traverser.after_rt_pat(self)
        } else {
            Ok(())
        }
    }
}

impl Traverse for RtFunction {
    fn traverse<T: Traverser>(&self, traverser: &mut T) -> Result<(), T::Error> {
        traverser.traverse(&self.params)?;
        traverser.traverse(&self.ret)?;
        traverser.after_rt_def(self.id, || self.ty())?;
        traverser.traverse(&self.body)
    }
}

impl Traverse for RtVar {
    fn traverse<T: Traverser>(&self, traverser: &mut T) -> Result<(), T::Error> {
        traverser.traverse(&self.ty)?;
        traverser.after_rt_def(self.id, || self.ty.clone())?;
        traverser.traverse(&self.init)
    }
}

impl Traverse for RtCont {
    fn traverse<T: Traverser>(&self, traverser: &mut T) -> Result<(), T::Error> {
        traverser.traverse(&self.params)?;
        traverser.traverse(&self.body)
    }
}

impl Traverse for Const {
    fn traverse<T: Traverser>(&self, traverser: &mut T) -> Result<(), T::Error> {
        match self {
            Self::Integer(ty, _, _) => traverser.traverse(ty),
            Self::FPNumber(ty, _) => traverser.traverse(ty),
            Self::SyntaxSexp(ty, _) => traverser.traverse(ty),
            Self::String(_) | Self::Char(_) | Self::Unit => Ok(()),
        }
    }
}

pub fn captured_vars<S: Traverse, T: FromIterator<RtId>>(src: &S) -> T {
    let mut captured_vars = CapturedVars {
        uses: HashSet::new(),
        defs: HashSet::new(),
    };
    let _ = traverse(src, &mut captured_vars);
    captured_vars
        .uses
        .difference(&captured_vars.defs)
        .copied()
        .collect()
}

#[derive(Debug)]
struct CapturedVars {
    uses: HashSet<RtId>,
    defs: HashSet<RtId>,
}

impl Traverser for CapturedVars {
    type Error = ();

    fn after_rt_use(&mut self, id: RtId) -> Result<(), ()> {
        self.uses.insert(id);
        Ok(())
    }

    fn after_rt_def(&mut self, id: RtId, _ty: impl FnOnce() -> Ct) -> Result<(), ()> {
        self.defs.insert(id);
        Ok(())
    }
}

pub fn visit_ct_uses<T: Traverse, F: FnMut(&CtId)>(src: &T, f: F) {
    let _ = src.traverse(&mut CtUsesVisitor(f));
}

#[derive(Debug)]
struct CtUsesVisitor<F>(F);

impl<F: FnMut(&CtId)> Traverser for CtUsesVisitor<F> {
    type Error = ();

    fn after_ct(&mut self, ct: &Ct) -> Result<(), ()> {
        if let Ct::Id(id) = ct {
            (self.0)(id);
        }
        Ok(())
    }
}
