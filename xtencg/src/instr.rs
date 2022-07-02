use std::collections::BTreeMap;
use std::fmt;

mod encoding;
mod operand;
mod size;
mod spec;

pub use encoding::{Component, Encoding};
pub use operand::{Operand, OperandMap};
pub use size::Size;
pub use spec::Spec;

pub type InstructionSet = BTreeMap<Mnemonic, Vec<Instruction>>;

/// A mnemonic of the instruction. (ex. "movq")
pub type Mnemonic = String;

/// x86 instruction.
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Instruction {
    mnemonic: Mnemonic,
    operands: Vec<Operand>,
    encoding: Encoding,
    operand_map: OperandMap,
    description: String,
}

impl Instruction {
    pub fn mnemonic(&self) -> &Mnemonic {
        &self.mnemonic
    }

    pub fn operands(&self) -> &[Operand] {
        &self.operands
    }

    pub fn encoding(&self) -> &Encoding {
        &self.encoding
    }

    pub fn operand_map(&self) -> &OperandMap {
        &self.operand_map
    }

    /// A Human-readable instruction description.
    pub fn description(&self) -> &str {
        &self.description
    }

    /// Merge two instruction definitions, if possible.
    pub fn merge(&mut self, mut other: Self) -> Result<(), String> {
        if self.mnemonic == other.mnemonic || self.operands == other.operands {
            if self.encoding.components_count() > other.encoding.components_count() {
                // To make self=base, other=variation
                std::mem::swap(self, &mut other);
            }

            // Since REX prefixes are added as needed during instruction encoding,
            // `<prefixes> <rex> <opcode> ...` is part of `<prefix> <opcode> ...`
            if other.encoding.is_rex_variation_of(&self.encoding) {
                return Ok(());
            }
        }

        Err(format!("Cannot merge {} into {}", other, self))
    }

    /// Is it intended that the encoding of two instructions overlap?
    pub fn allows_encoding_overlap_with(&self, other: &Self) -> bool {
        let (a, b) = (
            self.mnemonic.as_str().min(&other.mnemonic),
            self.mnemonic.as_str().max(&other.mnemonic),
        );

        (a.starts_with('j') && b.starts_with('j')) // Both are jCC
            || (a.starts_with("set") && b.starts_with("set")) // Both are setCC
            || (a.starts_with("sal") && b.starts_with("shl")) // shl and sal
            || (a == "movhps" && b == "movlhps")
            || (a == "movhlps" && b == "movlps")
            || (a.starts_with("xchg") && a == b && self.operands[0] == other.operands[1]) // operand swapped xchg
            || (a == "nop" && b.starts_with("xchg")) // nop and xchg
    }

    pub fn normalize(&mut self) -> Result<(), String> {
        // Since the specification is not defined specifically for 64-bit mode and may not include
        // operand-size override prefix (66h), which must be added as needed.
        if self.operands.iter().any(|o| o.requires_osop())
            && !matches!(
                self.mnemonic.as_str(),
                "movswl" | "movswq" | "movzwl" | "movzwq" | "retq" | "verr" | "verw" | "lmsw"
            )
        {
            self.encoding.put_prefix(0x66)?;
        }

        match (
            self.encoding.mod_rm(),
            self.operand_map.reg,
            self.operand_map.rm,
        ) {
            // Add missing ModR/M
            (None, _, Some(_)) | (None, Some(_), _) => self.encoding.set_mod_rm(None)?,
            // Found inconsistency
            (Some(Some(_)), Some(_), _) => Err("Cannot encode an operand to ModRM.mod")?,
            _ => {}
        }

        if let (None, Some(index)) = (self.encoding.immediate(), self.operand_map.imm) {
            // Add missing immediate (i_)
            match self.operands[index] {
                Operand::Imm(size) => self.encoding.set_immediate(size)?,
                operand => Err(format!("Cannot encode {} as an immediate", operand))?,
            }
        }

        // code_offset (c_) and reg_in_opcode (+r_) are always specified in the specification, just validate it
        if self.encoding.code_offset().is_some() != self.operand_map.code_offset.is_some() {
            Err("Code offset operand encoding unspecified")?;
        }
        if self.encoding.reg_in_opcode().is_some() != self.operand_map.reg_in_opcode.is_some() {
            Err("In-opcode register operand encoding unspecified")?;
        }

        Ok(())
    }
}

impl TryFrom<Spec> for Instruction {
    type Error = String;

    fn try_from(spec: Spec) -> Result<Self, Self::Error> {
        let encoding = Encoding::try_from(&spec)?;
        let mnemonic = spec.mnemonic;
        let operands = spec
            .operands
            .iter()
            .map(|o| {
                Operand::try_from(o.as_str())
                    .map_err(|_| format!("{}: Unknown operand {}", mnemonic, o))
            })
            .collect::<Result<Vec<_>, _>>()?;
        let operand_map = OperandMap::build(
            operands
                .iter()
                .copied()
                .zip(spec.op_en.chars().map(Some).chain(std::iter::repeat(None))),
        )?;

        Ok(Self {
            mnemonic,
            operands,
            operand_map,
            encoding,
            description: spec.description,
        })
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.mnemonic)?;
        for o in self.operands.iter() {
            write!(f, " {}", o)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn inst_adc_for_test() -> Instruction {
        Instruction {
            mnemonic: "adc".to_string(),
            operands: vec![
                Operand::R(Some(Size::B)),
                Operand::Rm(Some(Size::B), Some(Size::B)),
            ],
            operand_map: OperandMap {
                reg: Some(0),
                rm: Some(1),
                code_offset: None,
                imm: None,
                reg_in_opcode: None,
            },
            encoding: Encoding::from_components(vec![
                Component::Const(0x12),
                Component::ModRM(None),
            ])
            .unwrap(),
            description: "Add with carry r/m8 to byte register.".to_string(),
        }
    }

    #[test]
    fn instruction_from_spec() {
        let spec = Spec {
            mnemonic: "movq".to_string(),
            operands: vec!["r64".to_string(), "r/m64".to_string()],
            opcode: vec!["REX.W+".to_string(), "8B".to_string(), "/r".to_string()],
            op_en: "RM".to_string(),
            description: "Move r/m64 to r64.".to_string(),
        };
        let encoding = Encoding::try_from(&spec).unwrap();
        assert_eq!(
            Instruction::try_from(spec),
            Ok(Instruction {
                mnemonic: "movq".to_string(),
                operands: vec![
                    Operand::R(Some(Size::O)),
                    Operand::Rm(Some(Size::O), Some(Size::O)),
                ],
                operand_map: OperandMap {
                    reg: Some(0),
                    rm: Some(1),
                    code_offset: None,
                    imm: None,
                    reg_in_opcode: None,
                },
                encoding,
                description: "Move r/m64 to r64.".to_string(),
            })
        );
    }

    #[test]
    fn instruction_display() {
        assert_eq!(&inst_adc_for_test().to_string(), "adc r8 r/m8");
    }

    #[test]
    fn instruction_merge() {
        let adc_base = inst_adc_for_test();
        let adc_variation = Instruction {
            encoding: Encoding::from_components(vec![
                Component::Rex,
                Component::Const(0x12),
                Component::ModRM(None),
            ])
            .unwrap(),
            ..adc_base.clone()
        };
        let mut adc_merged = adc_base.clone();
        assert_eq!(Ok(()), adc_merged.merge(adc_variation));
        assert_eq!(adc_base, adc_merged);
    }
}
