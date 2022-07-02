use super::Size::{self, *};
use heapless::Vec as HVec;
use std::fmt;
use Operand::*;

/// An operand of the instruction.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
pub enum Operand {
    Rel(Size),                      // A relative address
    R(Option<Size>),                // General-purpose registers
    Imm(Size),                      // An immediate value
    Rm(Option<Size>, Option<Size>), // General-purpose registers or memory
    M(Option<Size>),                // Memory. usually specified by ModR/M where mod != 0b11
    Xmm,                            // XMM registers
    Xmmm(Option<Size>),             // XMM registers or memory
    Fixed(&'static str),            // Fixed (Operand is part of the opcode)
}

impl Operand {
    /// Requires operand-size override prefix for this operand.
    pub fn requires_osop(self) -> bool {
        // r16 | r/m16 | AX | imm16
        matches!(
            self,
            R(Some(W)) | Rm(Some(W), Some(W)) | Fixed("_Ax") | Imm(W)
        )
    }

    pub fn monomorphise(self) -> HVec<Self, 2> {
        let mut vec = HVec::new();
        match self {
            Rm(r, m) => {
                let _ = vec.push(R(r));
                let _ = vec.push(M(m));
            }
            Xmmm(m) => {
                let _ = vec.push(Xmm);
                let _ = vec.push(M(m));
            }
            _ => {
                let _ = vec.push(self);
            }
        }
        vec
    }
}

impl TryFrom<&str> for Operand {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, ()> {
        Ok(match value {
            "rel8" => Rel(B),
            "rel16" => Rel(W),
            "rel32" => Rel(D),
            "r8" => R(Some(B)),
            "r16" => R(Some(W)),
            "r32" => R(Some(D)),
            "r64" => R(Some(O)),
            "reg" => R(None),
            "imm8" => Imm(B),
            "imm16" => Imm(W),
            "imm32" => Imm(D),
            "imm64" => Imm(O),
            "r/m8" => Rm(Some(B), Some(B)),
            "r/m16" => Rm(Some(W), Some(W)),
            "r/m32" => Rm(Some(D), Some(D)),
            "r/m64" => Rm(Some(O), Some(O)),
            "r32/m8" => Rm(Some(D), Some(B)),
            "r32/m16" => Rm(Some(D), Some(W)),
            "reg/m8" => Rm(None, Some(B)),
            "reg/m16" => Rm(None, Some(W)),
            "reg/m32" => Rm(None, Some(D)),
            "m" => M(None),
            "m32" => M(Some(D)),
            "m64" => M(Some(O)),
            "m128" => M(Some(O2)),
            "AL" => Fixed("_Al"),
            "AX" => Fixed("_Ax"),
            "EAX" => Fixed("_Eax"),
            "RAX" => Fixed("_Rax"),
            "CL" => Fixed("_Cl"),
            "DX" => Fixed("_Dx"),
            "<XMM0>" => Fixed("_Xmm0"),
            "1" => Fixed("_1"),
            "3" => Fixed("_3"),
            "xmm" | "xmm1" | "xmm2" => Xmm,
            "xmm/m16" | "xmm1/m16" | "xmm2/m16" => Xmmm(Some(W)),
            "xmm/m32" | "xmm1/m32" | "xmm2/m32" => Xmmm(Some(D)),
            "xmm/m64" | "xmm1/m64" | "xmm2/m64" => Xmmm(Some(O)),
            "xmm/m128" | "xmm1/m128" | "xmm2/m128" => Xmmm(Some(O2)),
            _ => Err(())?,
        })
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Rel(s) => write!(f, "rel{}", s.bits()),
            R(None) => write!(f, "reg"),
            R(Some(s)) => write!(f, "r{}", s.bits()),
            Imm(s) => write!(f, "imm{}", s.bits()),
            Rm(Some(r), Some(m)) if r == m => write!(f, "r/m{}", r.bits()),
            Rm(r, m) => write!(f, "{}/{}", R(r), M(m)),
            M(None) => write!(f, "m"),
            M(Some(s)) => write!(f, "m{}", s.bits()),
            Xmm => write!(f, "xmm"),
            Xmmm(s) => write!(f, "{}/{}", Xmm, M(s)),
            Fixed(a) => write!(f, "{}", a),
        }
    }
}

/// A table of correspondence between the encoding positions and the operands.
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub struct OperandMap {
    pub reg: Option<usize>,           // At ModR/M reg field (+REX.r, if required)
    pub rm: Option<usize>, // At ModR/M r/m field (+SIB +Displacement +REX.x +REX.b, if required)
    pub code_offset: Option<usize>, // Immediate after opcode, corresponds to {cb,cw,cd,co}.
    pub imm: Option<usize>, // Immediate after ModR/M (+SIB +Displacement), corresponds to {ib,iw,id,io}.
    pub reg_in_opcode: Option<usize>, // At the lower 3 bits of the opcode last byte (+REX.b, if required), corresponds to {+rb,+rw,+rd,+ro}.
}

impl OperandMap {
    /// Build an operand map from operands and hints for the encoding of those operands.
    ///
    /// Hints are specified in Operand Encoding (Op/En) in Intel SDM.
    /// Basically, each character of Op/En corresponds to an operand and expresses which
    /// operand is encoded in which position.
    /// As Intel SDM says, the letters of this field apply ONLY the encoding definition table
    /// immediately following the instruction summary table. Therefore, there is no strict
    /// consistency between instructions regarding this field. This is why we call them `hints`.
    pub fn build(
        operands_with_hints: impl IntoIterator<Item = (Operand, Option<char>)>,
    ) -> Result<Self, String> {
        fn set(pos: &mut Option<usize>, index: usize) -> Result<(), String> {
            match pos.replace(index) {
                None => Ok(()),
                Some(i2) => Err(format!("Operand conflict: {} <=> {}", index, i2)),
            }
        }

        let mut reg = None;
        let mut rm = None;
        let mut code_offset = None;
        let mut imm = None;
        let mut reg_in_opcode = None;

        for (index, (operand, hint)) in operands_with_hints.into_iter().enumerate() {
            match (operand, hint) {
                (Rel(_), h) if !matches!(h, Some('R' | 'M')) => set(&mut code_offset, index)?,
                (R(_), Some('R') | None) => set(&mut reg, index)?,
                (R(_), Some('M')) => set(&mut rm, index)?,
                (R(_), Some('O')) => set(&mut reg_in_opcode, index)?,
                (Imm(_), h) if !matches!(h, Some('R' | 'M')) => set(&mut imm, index)?,
                (Rm(_, _), Some('R')) => set(&mut reg, index)?, // mod == 0b11
                (Rm(_, _), Some('M') | None) => set(&mut rm, index)?,
                (M(_), Some('M') | None) => set(&mut rm, index)?, // mod != 0b11
                (Xmm, Some('R') | None) => set(&mut reg, index)?,
                (Xmm, Some('M')) => set(&mut rm, index)?,
                (Xmmm(_), Some('R')) => set(&mut reg, index)?, // mod == 0b11
                (Xmmm(_), Some('M') | None) => set(&mut rm, index)?,
                (Fixed(_), _) => {}
                (o, Some(h)) => Err(format!(
                    "Cannot determine the encoding position of {} (hint: {})",
                    o, h
                ))?,
                (o, None) => Err(format!("Cannot determine the encoding position of {}", o))?,
            }
        }

        Ok(Self {
            reg,
            rm,
            code_offset,
            imm,
            reg_in_opcode,
        })
    }
}

impl fmt::Display for OperandMap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, (index, name)) in [
            (self.reg, "reg"),
            (self.rm, "rm"),
            (self.code_offset, "c"),
            (self.imm, "i"),
            (self.reg_in_opcode, "r"),
        ]
        .into_iter()
        .filter_map(|(index, name)| Some((index?, name)))
        .enumerate()
        {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}={}", name, index)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn operand_parse_display() {
        for (src, dest) in [
            ("r/m16", "r/m16"),
            ("AX", "_Ax"),
            ("rel32", "rel32"),
            ("reg/m32", "reg/m32"),
            ("imm64", "imm64"),
            ("r64", "r64"),
            ("xmm1", "xmm"),
            ("xmm2/m32", "xmm/m32"),
        ] {
            assert_eq!(
                Ok(dest.to_string()),
                Operand::try_from(src).map(|o| o.to_string())
            );
        }
    }

    #[test]
    fn operand_map_build() {
        // shrdq r/m64, r64, imm8 :: "MRI"
        assert_eq!(
            OperandMap::build([
                (Rm(Some(O), Some(O)), Some('M')),
                (R(Some(O)), Some('R')),
                (Imm(B), Some('I'))
            ]),
            Ok(OperandMap {
                reg: Some(1),
                rm: Some(0),
                code_offset: None,
                imm: Some(2),
                reg_in_opcode: None,
            }),
        );

        // xchgq RAX, r64 :: " O"
        assert_eq!(
            OperandMap::build([(Fixed("RAX"), Some(' ')), (R(Some(O)), Some('O'))]),
            Ok(OperandMap {
                reg: None,
                rm: None,
                code_offset: None,
                imm: None,
                reg_in_opcode: Some(1),
            }),
        );
    }

    #[test]
    fn operand_map_display() {
        assert_eq!(
            &OperandMap {
                reg: Some(1),
                rm: Some(0),
                code_offset: None,
                imm: Some(2),
                reg_in_opcode: None,
            }
            .to_string(),
            "reg=1, rm=0, i=2"
        );
    }
}
