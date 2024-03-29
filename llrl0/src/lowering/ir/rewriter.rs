use super::*;
use std::collections::HashMap;

pub fn rewrite<T: Rewriter>(src: &mut impl Rewrite, rewriter: &mut T) -> Result<(), T::Error> {
    Rewrite::rewrite(src, rewriter)
}

pub trait Rewriter: Sized {
    type Error;

    fn rewrite<T: Rewrite>(&mut self, src: &mut T) -> Result<(), Self::Error> {
        src.rewrite(self)
    }

    fn before_ct(&mut self, _ct: &mut Ct) -> Result<bool, Self::Error> {
        Ok(true)
    }

    fn after_ct(&mut self, _ct: &mut Ct) -> Result<(), Self::Error> {
        Ok(())
    }

    fn before_rt(&mut self, _rt: &mut Rt) -> Result<bool, Self::Error> {
        Ok(true)
    }

    fn after_rt(&mut self, _rt: &mut Rt) -> Result<(), Self::Error> {
        Ok(())
    }

    fn before_rt_pat(&mut self, _pat: &mut RtPat) -> Result<bool, Self::Error> {
        Ok(true)
    }

    fn after_rt_pat(&mut self, _pat: &mut RtPat) -> Result<(), Self::Error> {
        Ok(())
    }
    fn after_rt_use(&mut self, _id: &mut RtId) -> Result<(), Self::Error> {
        Ok(())
    }

    fn after_rt_def(
        &mut self,
        _id: &mut RtId,
        _ty: impl FnOnce() -> Ct,
    ) -> Result<(), Self::Error> {
        Ok(())
    }
}

pub trait Rewrite {
    fn rewrite<T: Rewriter>(&mut self, rewriter: &mut T) -> Result<(), T::Error>;
}

impl<A: Rewrite> Rewrite for Box<A> {
    fn rewrite<T: Rewriter>(&mut self, rewriter: &mut T) -> Result<(), T::Error> {
        rewriter.rewrite(&mut **self)
    }
}

impl<A: Rewrite> Rewrite for Option<A> {
    fn rewrite<T: Rewriter>(&mut self, rewriter: &mut T) -> Result<(), T::Error> {
        match self {
            Some(x) => rewriter.rewrite(x),
            _ => Ok(()),
        }
    }
}

impl<A: Rewrite> Rewrite for Vec<A> {
    fn rewrite<T: Rewriter>(&mut self, rewriter: &mut T) -> Result<(), T::Error> {
        for x in self.iter_mut() {
            rewriter.rewrite(x)?;
        }
        Ok(())
    }
}

impl Rewrite for Ct {
    fn rewrite<T: Rewriter>(&mut self, rewriter: &mut T) -> Result<(), T::Error> {
        if rewriter.before_ct(self)? {
            match self {
                Self::Id(_) => {}
                Self::GenericInst(inst) => {
                    rewriter.rewrite(&mut inst.0)?;
                    rewriter.rewrite(&mut inst.1)?;
                }
                Self::TableGet(get) => {
                    rewriter.rewrite(&mut get.0)?;
                }
                Self::Ptr(ty) => {
                    rewriter.rewrite(ty)?;
                }
                Self::Clos(clos) => {
                    rewriter.rewrite(&mut clos.0)?;
                    rewriter.rewrite(&mut clos.1)?;
                }
                Self::Array(ty) => {
                    rewriter.rewrite(ty)?;
                }
                Self::Syntax(ty) => {
                    rewriter.rewrite(ty)?;
                }
                Self::S(_)
                | Self::U(_)
                | Self::F32
                | Self::F64
                | Self::String
                | Self::Char
                | Self::CapturedUse
                | Self::Unit
                | Self::Env => {}
            };
            rewriter.after_ct(self)
        } else {
            Ok(())
        }
    }
}

impl Rewrite for Def {
    fn rewrite<T: Rewriter>(&mut self, rewriter: &mut T) -> Result<(), T::Error> {
        match self {
            Self::Alias(ct) => rewriter.rewrite(ct),
            Self::AliasTable(table) => rewriter.rewrite(table),
            Self::Generic(_, ret) => rewriter.rewrite(ret),
            Self::Data(ty) => rewriter.rewrite(ty),
            Self::Struct(ty) => rewriter.rewrite(ty),
            Self::Union(ty) => rewriter.rewrite(ty),
            Self::Function(f) => rewriter.rewrite(f),
        }
    }
}

impl Rewrite for AliasTable {
    fn rewrite<T: Rewriter>(&mut self, rewriter: &mut T) -> Result<(), T::Error> {
        for (_, ct) in self.entries_mut() {
            rewriter.rewrite(ct)?;
        }
        Ok(())
    }
}

impl Rewrite for Data {
    fn rewrite<T: Rewriter>(&mut self, rewriter: &mut T) -> Result<(), T::Error> {
        rewriter.rewrite(&mut self.cons)
    }
}

impl Rewrite for Struct {
    fn rewrite<T: Rewriter>(&mut self, rewriter: &mut T) -> Result<(), T::Error> {
        rewriter.rewrite(&mut self.fields)
    }
}

impl Rewrite for Union {
    fn rewrite<T: Rewriter>(&mut self, rewriter: &mut T) -> Result<(), T::Error> {
        rewriter.rewrite(&mut self.tys)
    }
}

impl Rewrite for Function {
    fn rewrite<T: Rewriter>(&mut self, rewriter: &mut T) -> Result<(), T::Error> {
        rewriter.rewrite(&mut self.env)?;
        rewriter.rewrite(&mut self.params)?;
        rewriter.rewrite(&mut self.ret)?;
        rewriter.rewrite(&mut self.body)
    }
}

impl Rewrite for FunctionEnv {
    fn rewrite<T: Rewriter>(&mut self, rewriter: &mut T) -> Result<(), T::Error> {
        rewriter.after_rt_def(&mut self.id, || Ct::Env)?;
        rewriter.rewrite(&mut self.elems)
    }
}

impl Rewrite for RtParam {
    fn rewrite<T: Rewriter>(&mut self, rewriter: &mut T) -> Result<(), T::Error> {
        rewriter.rewrite(&mut self.ty)?;
        rewriter.after_rt_def(&mut self.id, || self.ty.clone())
    }
}

impl Rewrite for Init {
    fn rewrite<T: Rewriter>(&mut self, rewriter: &mut T) -> Result<(), T::Error> {
        rewriter.rewrite(&mut self.ty)?;
        rewriter.rewrite(&mut self.expr)
    }
}

impl Rewrite for Rt {
    fn rewrite<T: Rewriter>(&mut self, rewriter: &mut T) -> Result<(), T::Error> {
        if rewriter.before_rt(self)? {
            match self {
                Self::Var(id, ty) => {
                    rewriter.after_rt_use(id)?;
                    rewriter.rewrite(ty)?;
                }
                Self::LocalFun(inst) => {
                    rewriter.after_rt_use(&mut inst.fun)?;
                    rewriter.rewrite(&mut inst.args)?;
                    rewriter.rewrite(&mut inst.ty)?;
                }
                Self::StaticFun(capture) => {
                    rewriter.rewrite(&mut capture.fun)?;
                    rewriter.rewrite(&mut capture.ty)?;
                    rewriter.rewrite(&mut capture.env)?;
                }
                Self::Const(c) => {
                    rewriter.rewrite(c)?;
                }
                Self::Call(call) => {
                    rewriter.rewrite(&mut call.callee)?;
                    rewriter.rewrite(&mut call.args)?;
                }
                Self::ContCall(call) => {
                    rewriter.rewrite(&mut call.args)?;
                    rewriter.rewrite(&mut call.ret)?;
                }
                Self::Nullary(nullary) => {
                    rewriter.rewrite(nullary)?;
                }
                Self::Unary(unary) => {
                    rewriter.rewrite(&mut unary.0)?;
                    rewriter.rewrite(&mut unary.1)?;
                }
                Self::Binary(binary) => {
                    rewriter.rewrite(&mut binary.0)?;
                    rewriter.rewrite(&mut binary.1)?;
                    rewriter.rewrite(&mut binary.2)?;
                }
                Self::Ternary(ternary) => {
                    rewriter.rewrite(&mut ternary.0)?;
                    rewriter.rewrite(&mut ternary.1)?;
                    rewriter.rewrite(&mut ternary.2)?;
                    rewriter.rewrite(&mut ternary.3)?;
                }
                Self::Alloc(alloc) => {
                    rewriter.rewrite(&mut alloc.1)?;
                }
                Self::AllocArray(alloc) => {
                    rewriter.rewrite(&mut alloc.1)?;
                    rewriter.rewrite(&mut alloc.2)?;
                }
                Self::ConstructEnv(con) => {
                    rewriter.rewrite(&mut con.1)?;
                }
                Self::ConstructData(con) => {
                    rewriter.rewrite(&mut con.0)?;
                    rewriter.rewrite(&mut con.2)?;
                }
                Self::ConstructStruct(con) => {
                    rewriter.rewrite(&mut con.0)?;
                    rewriter.rewrite(&mut con.1)?;
                }
                Self::ConstructSyntax(con) => {
                    rewriter.rewrite(&mut con.1)?;
                }
                Self::Seq(seq) => {
                    rewriter.rewrite(&mut seq.0)?;
                    rewriter.rewrite(&mut seq.1)?;
                }
                Self::If(if_) => {
                    rewriter.rewrite(&mut if_.0)?;
                    rewriter.rewrite(&mut if_.1)?;
                    rewriter.rewrite(&mut if_.2)?;
                }
                Self::While(while_) => {
                    rewriter.rewrite(&mut while_.0)?;
                    rewriter.rewrite(&mut while_.1)?;
                }
                Self::And(ab) | Self::Or(ab) => {
                    rewriter.rewrite(&mut ab.0)?;
                    rewriter.rewrite(&mut ab.1)?;
                }
                Self::Match(match_) => {
                    rewriter.rewrite(&mut match_.0)?;
                    rewriter.rewrite(&mut match_.1)?;
                }
                Self::Return(ret) => {
                    rewriter.rewrite(ret)?;
                }
                Self::Never => {}
                Self::LetLocalFun(let_) => {
                    rewriter.rewrite(&mut let_.0)?;
                    rewriter.rewrite(&mut let_.1)?;
                }
                Self::LetVar(let_) => {
                    rewriter.rewrite(&mut let_.0)?;
                    rewriter.rewrite(&mut let_.1)?;
                }
                Self::LetCont(let_) => {
                    rewriter.rewrite(&mut let_.0)?;
                    rewriter.rewrite(&mut let_.1)?;
                }
            };
            rewriter.after_rt(self)
        } else {
            Ok(())
        }
    }
}

impl Rewrite for RtCallee {
    fn rewrite<T: Rewriter>(&mut self, rewriter: &mut T) -> Result<(), T::Error> {
        match self {
            Self::Standard(rt) => {
                rewriter.rewrite(rt)?;
            }
            Self::CDirect(_, ret) => {
                rewriter.rewrite(ret)?;
            }
            Self::CIndirect(rt, ret) => {
                rewriter.rewrite(rt)?;
                rewriter.rewrite(ret)?;
            }
            Self::MainIndirect(rt) => {
                rewriter.rewrite(rt)?;
            }
            Self::MacroIndirect(rt, ret) => {
                rewriter.rewrite(rt)?;
                rewriter.rewrite(ret)?;
            }
        }
        Ok(())
    }
}

impl Rewrite for Nullary {
    fn rewrite<T: Rewriter>(&mut self, rewriter: &mut T) -> Result<(), T::Error> {
        use Nullary::*;
        match self {
            Uninitialized(ty) => {
                rewriter.rewrite(ty)?;
            }
            Null(ty) => {
                rewriter.rewrite(ty)?;
            }
            GenId => {}
            SizeOf(ty) | AlignOf(ty) => {
                rewriter.rewrite(ty)?;
            }
        }
        Ok(())
    }
}

impl Rewrite for Unary {
    fn rewrite<T: Rewriter>(&mut self, rewriter: &mut T) -> Result<(), T::Error> {
        use Unary::*;
        match self {
            Not => {}
            Load => {}
            StructElem(elem_ty, _) => {
                rewriter.rewrite(elem_ty)?;
            }
            Reinterpret(to) => {
                rewriter.rewrite(to)?;
            }
            SyntaxBody => {}
            Panic => {}
            BitCast(ty) => {
                rewriter.rewrite(ty)?;
            }
            PtrToI => {}
            IToPtr(ty) => {
                rewriter.rewrite(ty)?;
            }
            IComplement => {}
            ITrunc(ty) => {
                rewriter.rewrite(ty)?;
            }
            IPopCount => {}
            SExt(ty) | SToF(ty) => {
                rewriter.rewrite(ty)?;
            }
            UExt(ty) | UToF(ty) => {
                rewriter.rewrite(ty)?;
            }
            FToS(ty) | FToU(ty) | FTrunc(ty) | FExt(ty) => {
                rewriter.rewrite(ty)?;
            }
            RealCeil | RealFloor | RealTrunc | RealRound => {}
            MathSqrt | MathSin | MathCos | MathExp | MathLog => {}
            StringPtr | StringLength => {}
            ArrayPtr | ArrayLength => {}
        }
        Ok(())
    }
}

impl Rewrite for Binary {
    fn rewrite<T: Rewriter>(&mut self, _: &mut T) -> Result<(), T::Error> {
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
            PtrEq | PtrLt | PtrLe | PtrGt | PtrGe => {}
            ArrayConstruct | ArrayLoad => {}
        }
        Ok(())
    }
}

impl Rewrite for Ternary {
    fn rewrite<T: Rewriter>(&mut self, _: &mut T) -> Result<(), T::Error> {
        use Ternary::*;
        match self {
            PtrCopy | PtrMove => {}
            ArrayStore => {}
        }
        Ok(())
    }
}

impl Rewrite for RtClause {
    fn rewrite<T: Rewriter>(&mut self, rewriter: &mut T) -> Result<(), T::Error> {
        rewriter.rewrite(&mut self.pat)?;
        rewriter.rewrite(&mut self.body)
    }
}

impl Rewrite for RtPat {
    fn rewrite<T: Rewriter>(&mut self, rewriter: &mut T) -> Result<(), T::Error> {
        if rewriter.before_rt_pat(self)? {
            match self {
                Self::Var(id, ty, p) => {
                    rewriter.rewrite(ty)?;
                    rewriter.after_rt_def(id, || ty.clone())?;
                    rewriter.rewrite(p)?;
                }
                Self::Wildcard(ty) => {
                    rewriter.rewrite(ty)?;
                }
                Self::Deref(x) => {
                    rewriter.rewrite(x)?;
                }
                Self::NonNull(x) => {
                    rewriter.rewrite(x)?;
                }
                Self::Null(ty) => {
                    rewriter.rewrite(ty)?;
                }
                Self::Data(ty, _index, args) => {
                    rewriter.rewrite(ty)?;
                    rewriter.rewrite(args)?;
                }
                Self::Struct(ty, fields) => {
                    rewriter.rewrite(ty)?;
                    rewriter.rewrite(fields)?;
                }
                Self::Reinterpret(ty, x) => {
                    rewriter.rewrite(ty)?;
                    rewriter.rewrite(x)?;
                }
                Self::Syntax(body) => {
                    rewriter.rewrite(body)?;
                }
                Self::Const(c) => {
                    rewriter.rewrite(c)?;
                }
            }
            rewriter.after_rt_pat(self)
        } else {
            Ok(())
        }
    }
}

impl Rewrite for RtLocalFun {
    fn rewrite<T: Rewriter>(&mut self, rewriter: &mut T) -> Result<(), T::Error> {
        rewriter.rewrite(&mut self.params)?;
        rewriter.rewrite(&mut self.ret)?;
        rewriter.after_rt_def(&mut self.id, || Self::ty(&self.params, &self.ret))?;
        rewriter.rewrite(&mut self.body)
    }
}

impl Rewrite for RtVar {
    fn rewrite<T: Rewriter>(&mut self, rewriter: &mut T) -> Result<(), T::Error> {
        rewriter.rewrite(&mut self.ty)?;
        rewriter.after_rt_def(&mut self.id, || self.ty.clone())?;
        rewriter.rewrite(&mut self.init)
    }
}

impl Rewrite for RtCont {
    fn rewrite<T: Rewriter>(&mut self, rewriter: &mut T) -> Result<(), T::Error> {
        rewriter.rewrite(&mut self.params)?;
        rewriter.rewrite(&mut self.body)
    }
}

impl Rewrite for Const {
    fn rewrite<T: Rewriter>(&mut self, rewriter: &mut T) -> Result<(), T::Error> {
        match self {
            Self::Integer(ty, _, _) => rewriter.rewrite(ty),
            Self::FPNumber(ty, _) => rewriter.rewrite(ty),
            Self::SyntaxSexp(ty, _) => rewriter.rewrite(ty),
            Self::String(_) | Self::Char(_) | Self::Unit => Ok(()),
        }
    }
}

pub fn replace_ct(src: &mut impl Rewrite, mut map: HashMap<CtId, Ct>) {
    let _ = rewrite(src, &mut map);
}

impl Rewriter for HashMap<CtId, Ct> {
    type Error = ();

    fn after_ct(&mut self, ct: &mut Ct) -> Result<(), ()> {
        if let Ct::Id(id) = *ct {
            if let Some(x) = self.get(&id) {
                *ct = x.clone();
            }
        }
        Ok(())
    }
}

pub fn replace_rt(src: &mut impl Rewrite, mut map: HashMap<RtId, Rt>) {
    let _ = src.rewrite(&mut map);
}

impl Rewriter for HashMap<RtId, Rt> {
    type Error = ();

    fn after_rt(&mut self, rt: &mut Rt) -> Result<(), ()> {
        if let Rt::Var(id, _) = *rt {
            if let Some(x) = self.get(&id) {
                *rt = x.clone();
            }
        }
        Ok(())
    }
}

pub fn realloc_rts(src: &mut impl Rewrite, alloc: impl FnMut() -> RtId) {
    let _ = src.rewrite(&mut ReallocRts(alloc, HashMap::new()));
}

#[derive(Debug)]
struct ReallocRts<F>(F, HashMap<RtId, RtId>);

impl<F: FnMut() -> RtId> Rewriter for ReallocRts<F> {
    type Error = ();

    fn after_rt_def(&mut self, id: &mut RtId, _ty: impl FnOnce() -> Ct) -> Result<(), Self::Error> {
        let new_id = (self.0)();
        assert!(self.1.insert(*id, new_id).is_none());
        *id = new_id;
        Ok(())
    }

    fn after_rt_use(&mut self, id: &mut RtId) -> Result<(), Self::Error> {
        if let Some(new_id) = self.1.get(id) {
            *id = *new_id;
        }
        Ok(())
    }
}
