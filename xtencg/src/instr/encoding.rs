use super::Size::*;
use super::{Size, Spec};
use heapless::Vec as HVec;
use once_cell::sync::Lazy;
use regex::Regex;
use std::fmt;
use Component::*;

/// An encoding component of the instruction.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
pub enum Component {
    Const(u8),
    Imm(Size),         // An immediate value
    ModRM(Option<u8>), // A ModR/M byte where reg is fixed if some
    R(Size), // Indicates the lower 3 bits of the previous component byte is used to encode the register operand.
    C(Size), // A value following the opcode. Used as a code offset.
    Rex,     // Marker that REX prefix required
    RexW,    // Marker that REX prefix required with REX.w = 1
}

impl TryFrom<&str> for Component {
    type Error = ();

    fn try_from(unit: &str) -> Result<Self, ()> {
        static CONST: Lazy<Regex> = Lazy::new(|| Regex::new(r"[0-9A-Fa-f][0-9A-Fa-f]").unwrap());

        Ok(match unit {
            "ib" => Imm(B),
            "iw" => Imm(W),
            "id" => Imm(D),
            "io" => Imm(O),

            // A ModR/M that ModRM.reg are used to extend opcode. ModRM.rm is used as a operand:
            "/0" => ModRM(Some(0)),
            "/1" => ModRM(Some(1)),
            "/2" => ModRM(Some(2)),
            "/3" => ModRM(Some(3)),
            "/4" => ModRM(Some(4)),
            "/5" => ModRM(Some(5)),
            "/6" => ModRM(Some(6)),
            "/7" => ModRM(Some(7)),
            // A ModR/M that both ModRM.reg and ModRM.rm are used as operands:
            "/r" => ModRM(None),

            "+rb" => R(B),
            "+rw" => R(W),
            "+rd" => R(D),
            "+ro" => R(O),
            "cb" => C(B),
            "cw" => C(W),
            "cd" => C(D),
            "co" => C(O),
            "REX+" => Rex,
            "REX.W+" => RexW,
            "PREF.66+" => Const(0x66),

            n if CONST.is_match(n) => Const(u8::from_str_radix(n, 16).map_err(|_| ())?),
            _ => Err(())?,
        })
    }
}

impl fmt::Display for Component {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Const(n) => write!(f, "{:02X}", n),
            Imm(s) => write!(f, "i{}", s),
            ModRM(Some(n)) => write!(f, "/{}", n),
            ModRM(_) => write!(f, "/r"),
            R(s) => write!(f, "+r{}", s),
            C(s) => write!(f, "c{}", s),
            Rex => write!(f, "REX+"),
            RexW => write!(f, "REX.W+"),
        }
    }
}

/// Encoding of the instruction.
///
/// An encoded x86 instruction consists of the following components in the given order:
/// 1. Instruction prefixes
///    Especially an operand-size override prefix (0x66) is important for 16-bit operands.
///    Some prefixes are also used as a mandatory prefix. (see below)
/// 2. Opcode
///    Opcode consists of the following:
///    1. Mandatory prefix (0xf2, 0xf3, or 0x66)
///    2. REX prefix
///    3. A primary opcode
/// 3. ModR/M
/// 4. SIB
/// 5. Displacement
/// 6. Immediate
/// In this implementation, we call 1 to 2-1 `prefix`, 2-2 `rex`, 2-3 `opcode`, and 1 to 2-3 `operator`.
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Encoding {
    prefixes: HVec<u8, 4>,       // corresponds to Const(u8)
    rex: Option<bool>,           // corresponds to Rex | RexW
    opcode: HVec<u8, 3>,         // corresponds to Const(u8)
    modrm: Option<Option<u8>>,   // corresponds to ModRM(Option<Size>)
    reg_in_opcode: Option<Size>, // corresponds to R(Size)
    immediate: Option<Size>,     // corresponds to Imm(Size)
    code_offset: Option<Size>,   // corresponds to C(Size)
}

impl Encoding {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            prefixes: HVec::new(),
            rex: None,
            opcode: HVec::new(),
            modrm: None,
            reg_in_opcode: None,
            immediate: None,
            code_offset: None,
        }
    }

    pub fn prefixes(&self) -> impl ExactSizeIterator<Item = u8> + '_ {
        self.prefixes.iter().copied()
    }

    pub fn put_prefix(&mut self, prefix: u8) -> Result<(), &'static str> {
        assert!(matches!(prefix, 0xf2 | 0xf3 | 0x66));
        if self.prefixes().any(|p| p == prefix) {
            Ok(())
        } else {
            self.prefixes.push(prefix).map_err(|_| "Too much prefixes")
        }
    }

    pub fn rex(&self) -> Option<bool> {
        self.rex
    }

    pub fn set_rex(&mut self, rex_w: bool) -> Result<(), &'static str> {
        if self.rex.is_none() {
            self.rex = Some(rex_w);
            Ok(())
        } else {
            Err("Duplicate REX")
        }
    }

    pub fn is_rex_variation_of(&self, other: &Self) -> bool {
        self.rex == Some(false)
            && &Self {
                rex: None,
                ..self.clone()
            } == other
    }

    pub fn opcode(&self) -> impl ExactSizeIterator<Item = u8> + '_ {
        self.opcode.iter().copied()
    }

    pub fn put_opcode_part(&mut self, byte: u8) -> Result<(), &'static str> {
        if self.opcode().any(|b| b == byte) {
            Ok(())
        } else {
            self.opcode.push(byte).map_err(|_| "Too much opcode")
        }
    }

    pub fn reg_in_opcode(&self) -> Option<Size> {
        self.reg_in_opcode
    }

    pub fn set_reg_in_opcode(&mut self, size: Size) -> Result<(), &'static str> {
        if self.modrm.is_some() {
            Err("Cannot set both ModR/M and +r_")
        } else if self.reg_in_opcode.is_none() {
            self.reg_in_opcode = Some(size);
            Ok(())
        } else {
            Err("Duplicate +r_")
        }
    }

    pub fn modrm(&self) -> Option<Option<u8>> {
        self.modrm
    }

    pub fn set_modrm(&mut self, reg_fixed: Option<u8>) -> Result<(), &'static str> {
        if self.reg_in_opcode.is_some() {
            Err("Cannot set both ModR/M and +r_")
        } else if self.modrm.is_none() {
            self.modrm = Some(reg_fixed);
            Ok(())
        } else {
            Err("Duplicate ModR/M")
        }
    }

    pub fn immediate(&self) -> Option<Size> {
        self.immediate
    }

    pub fn set_immediate(&mut self, size: Size) -> Result<(), &'static str> {
        if self.immediate.is_none() {
            self.immediate = Some(size);
            Ok(())
        } else {
            Err("Duplicate i_")
        }
    }

    pub fn code_offset(&self) -> Option<Size> {
        self.code_offset
    }

    pub fn set_code_offset(&mut self, size: Size) -> Result<(), &'static str> {
        if self.code_offset.is_none() {
            self.code_offset = Some(size);
            Ok(())
        } else {
            Err("Duplicate c_")
        }
    }

    pub fn components_count(&self) -> usize {
        self.prefixes.len()
            + self.rex.is_some() as usize
            + self.opcode.len()
            + self.reg_in_opcode.is_some() as usize
            + self.modrm.is_some() as usize
            + self.immediate.is_some() as usize
            + self.code_offset.is_some() as usize
    }

    pub fn components(&self) -> impl Iterator<Item = Component> + '_ {
        (self.prefixes.iter().copied().map(Const))
            .chain(self.rex.map(|rex| match rex {
                true => RexW,
                false => Rex,
            }))
            .chain(self.opcode.iter().copied().map(Const))
            .chain(self.reg_in_opcode.map(R))
            .chain(self.modrm.map(ModRM))
            .chain(self.immediate.map(Imm))
            .chain(self.code_offset.map(C))
    }

    pub fn from_components(
        components: impl IntoIterator<Item = Component>,
    ) -> Result<Self, String> {
        #[derive(Debug, Clone, Copy)]
        enum Phase {
            Prefixes,
            Opcode,
            Operands,
        }
        let mut phase = Phase::Prefixes;
        let mut encoding = Encoding::new();

        for c in components {
            match (c, phase) {
                (Const(byte @ (0xf2 | 0xf3 | 0x66)), Phase::Prefixes) => {
                    encoding.put_prefix(byte)?;
                }
                // The REX prefix must follow the Legacy prefixes and precede the Opcode.
                (Rex, Phase::Prefixes) => {
                    encoding.set_rex(false)?;
                    phase = Phase::Opcode;
                }
                (RexW, Phase::Prefixes) => {
                    encoding.set_rex(true)?;
                    phase = Phase::Opcode;
                }
                (Const(byte), Phase::Prefixes | Phase::Opcode) => {
                    encoding.put_opcode_part(byte)?;
                    phase = Phase::Opcode;
                }
                (ModRM(reg_fixed), Phase::Opcode) => {
                    encoding.set_modrm(reg_fixed)?;
                    phase = Phase::Operands;
                }
                (R(s), Phase::Opcode) => {
                    encoding.set_reg_in_opcode(s)?;
                    phase = Phase::Operands;
                }
                (Imm(s), Phase::Opcode | Phase::Operands) => {
                    encoding.set_immediate(s)?;
                    phase = Phase::Operands;
                }
                (C(s), Phase::Opcode) => {
                    encoding.set_code_offset(s)?;
                    phase = Phase::Operands;
                }
                (c, phase) => Err(format!("Unexpected component {} (phase: {:?})", c, phase))?,
            }
        }

        Ok(encoding)
    }

    pub fn operator(&self) -> impl Iterator<Item = Component> + '_ {
        // ModR/M with fixed reg field is also treated as part of operator
        self.components()
            .take_while(|c| matches!(c, Const(_) | Rex | RexW | ModRM(Some(_))))
    }
}

impl TryFrom<&'_ Spec> for Encoding {
    type Error = String;

    fn try_from(spec: &'_ Spec) -> Result<Self, Self::Error> {
        Self::from_components(
            spec.opcode
                .iter()
                .map(|unit| {
                    Component::try_from(unit.as_str())
                        .map_err(|_| format!("{}: Unknown component {}", spec.mnemonic, unit))
                })
                .collect::<Result<Vec<_>, _>>()?,
        )
    }
}

impl fmt::Display for Encoding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Displays the encoding components
        for (i, c) in self.components().enumerate() {
            if i != 0 {
                write!(f, " ")?;
            }
            write!(f, "{}", c)?;
        }
        Ok(())
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Reg {
    Operand(usize),
    PartOfOpcode(u8),
    Default,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Rm {
    Operand(usize),
    Default,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub struct RegInOpcode {
    pub operand: usize,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum RexSource {
    ModRM { w: bool },
    RegInOpcode { w: bool },
    Standalone { w: bool },
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn component_parse_display() {
        for (src, dest) in [
            ("cw", "cw"),
            ("+rd", "+rd"),
            ("ib", "ib"),
            ("/3", "/3"),
            ("/r", "/r"),
            ("66", "66"),
            ("REX+", "REX+"),
            ("REX.W+", "REX.W+"),
        ] {
            assert_eq!(
                Ok(dest.to_string()),
                Component::try_from(src).map(|o| o.to_string())
            );
        }
    }

    #[test]
    fn encoding_components() {
        let components = vec![Const(0x66), RexW, Const(0x0f), Const(0x6e), ModRM(None)];
        let encoding = Encoding::from_components(components.clone()).unwrap();
        assert_eq!(encoding.components().collect::<Vec<_>>(), components);
        assert_eq!(
            encoding.operator().collect::<Vec<_>>(),
            vec![Const(0x66), RexW, Const(0x0f), Const(0x6e)]
        );
    }

    #[test]
    fn encoding_from_spec() {
        assert_eq!(
            Encoding::try_from(&Spec {
                mnemonic: "movq".to_string(),
                operands: vec!["r64".to_string(), "r/m64".to_string()],
                opcode: vec!["REX.W+".to_string(), "8B".to_string(), "/r".to_string()],
                op_en: "RM".to_string(),
                description: "Move r/m64 to r64.".to_string(),
            })
            .unwrap()
            .components()
            .collect::<Vec<_>>(),
            vec![RexW, Const(0x8b), ModRM(None)]
        );
    }

    #[test]
    fn encoding_display() {
        assert_eq!(
            &Encoding::from_components(vec![Const(0x8b), ModRM(None)])
                .unwrap()
                .to_string(),
            "8B /r"
        );
    }
}
