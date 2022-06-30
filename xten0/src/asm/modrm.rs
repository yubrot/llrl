// https://www.intel.com/content/www/us/en/developer/articles/technical/intel-sdm.html

use super::rex;
use crate::bits::BitExt;
use heapless::Vec as HVec;

/// ModR/M and some subsequent bytes of x64 instruction encoding.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct ModRM {
    pub reg: Reg,
    pub rm: Rm,
}

impl ModRM {
    pub fn new(reg: impl Into<Reg>, rm: impl Into<Rm>) -> Self {
        Self {
            reg: reg.into(),
            rm: rm.into(),
        }
    }

    /// Obtain a REX prefix if required.
    pub fn rex_byte(&self, rex_w: bool) -> Option<u8> {
        rex::encode(rex_w, self.reg.rex_r, self.rm.rex_x, self.rm.rex_b)
    }

    /// Obtain a ModR/M byte.
    pub fn byte(&self) -> u8 {
        0u8.set_bits(self.rm.modrm_mod, 6..8)
            .set_bits(self.reg.modrm_reg, 3..6)
            .set_bits(self.rm.modrm_rm, 0..3)
    }

    /// Obtain a SIB byte, if required for this ModR/M.
    pub fn sib_byte(&self) -> Option<u8> {
        self.rm.sib_byte()
    }

    /// Obtain some Displacement bytes, if required for this ModR/M.
    pub fn disp_bytes(&self) -> Option<HVec<u8, 4>> {
        self.rm.disp_bytes()
    }
}

/// ModRM.reg part of x64 instruction encoding.
///
/// This type includes an optional REX.r, which is an extension of ModRM.reg.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct Reg {
    pub modrm_reg: u8, // 3 bits, extended by rex_r
    pub rex_r: bool,
}

impl Reg {
    pub fn new(reg: u8) -> Self {
        assert!(reg <= 0b1111);
        Self {
            modrm_reg: reg & 0b111,
            rex_r: (reg & 0b1000) != 0,
        }
    }
}

impl Default for Reg {
    fn default() -> Self {
        Self::new(0)
    }
}

/// ModRM.rm part of x64 instruction encoding.
///
/// This type includes an optional REX.b, an optional SIB, and an optional Displacement.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct Rm {
    pub modrm_mod: u8, // 2 bits
    pub modrm_rm: u8,  // 3 bits, extended by rex_b
    pub rex_x: bool,
    pub rex_b: bool,
    pub sib_scale: Option<Scale>,
    pub sib_index: Option<u8>, // 3 bits, extended by rex_x
    pub sib_base: Option<u8>,  // 3 bits, extended by rex_b
    pub disp: Option<Displacement>,
}

impl Rm {
    pub fn new(modrm_mod: u8, rm: u8) -> Self {
        assert!(modrm_mod <= 0b11);
        assert!(rm <= 0b1111);
        Self {
            modrm_mod,
            modrm_rm: rm & 0b111,
            rex_x: false,
            rex_b: (rm & 0b1000) != 0, // REX.b is used to extend ModRM.rm
            sib_scale: None,
            sib_index: None,
            sib_base: None,
            disp: None,
        }
    }

    pub fn sib_scale(self, scale: Scale) -> Self {
        assert!(self.sib_present());
        Self {
            sib_scale: Some(scale),
            ..self
        }
    }

    pub fn sib_index(self, index: u8) -> Self {
        assert!(self.sib_present());
        assert!(index <= 0b1111);
        Self {
            rex_x: (index & 0b1000) != 0,
            sib_index: Some(index & 0b111),
            ..self
        }
    }

    pub fn sib_base(self, base: u8) -> Self {
        assert!(self.sib_present());
        assert!(base <= 0b1111);
        Self {
            rex_b: (base & 0b1000) != 0, // Overwriting REX.b to extend SIB.base
            sib_base: Some(base & 0b111),
            ..self
        }
    }

    /// Whether the SIB byte is present or not, as determined by ModRM.mod and ModRM.rm.
    pub fn sib_present(&self) -> bool {
        self.modrm_mod != 0b11 && self.modrm_rm == 0b100
    }

    /// Obtain a SIB byte, if required.
    pub fn sib_byte(&self) -> Option<u8> {
        if self.sib_present() {
            Some(
                0u8.set_bits(self.sib_scale.map_or(0, |s| s.bits()), 6..8)
                    .set_bits(self.sib_index.unwrap_or(0), 3..6)
                    .set_bits(self.sib_base.unwrap_or(0), 0..3),
            )
        } else {
            None
        }
    }

    pub fn disp(self, disp: impl Into<Displacement>) -> Self {
        let disp = disp.into();
        assert!(Some(disp.size()) == self.disp_size());
        Self {
            disp: Some(disp),
            ..self
        }
    }

    /// Get the length of the Displacement, as determined by ModRM.mod, ModRM.rm, and SIB, in bytes.
    #[allow(clippy::if_same_then_else)]
    pub fn disp_size(&self) -> Option<u8> {
        if self.modrm_mod == 0b01 {
            // [GP + disp8]
            Some(1)
        } else if self.modrm_mod == 0b10 {
            // [GP + disp32]
            Some(4)
        } else if self.modrm_mod == 0b00 && self.modrm_rm == 0b101 {
            // [RIP + disp32] (RIP-relative addressing)
            Some(4)
        } else if self.sib_present()
            && self.modrm_mod == 0b00
            && !self.rex_b
            && self.sib_base == Some(0b101)
        {
            // [disp32 + ..]
            Some(4)
        } else {
            None
        }
    }

    /// Obtain some Displacement bytes, if required.
    pub fn disp_bytes(&self) -> Option<HVec<u8, 4>> {
        assert!(self.disp_size() == self.disp.map(|d| d.size()));
        self.disp.map(|d| d.bytes())
    }
}

impl Default for Rm {
    fn default() -> Self {
        Self::new(0, 0)
    }
}

/// Displacement part of x64 instruction encoding.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub enum Displacement {
    Disp8(i8),
    Disp32(i32),
}

impl Displacement {
    /// Get the appropriate ModRM.mod field value of this Displacement.
    ///
    /// In x64 instruction encoding, the use of Displacement is usually specified in the ModRM.mod field.
    /// See also: `Rm::disp_size`
    pub fn modrm_mod(&self) -> u8 {
        match self {
            Disp8(_) => 0b01,
            Disp32(_) => 0b10,
        }
    }

    /// Get the length of this Displacement in bytes.
    ///
    /// See also: `Rm::disp_size`
    pub fn size(&self) -> u8 {
        match self {
            Disp8(_) => 1,
            Disp32(_) => 4,
        }
    }

    pub fn bytes(&self) -> HVec<u8, 4> {
        match self {
            Disp8(disp) => HVec::from_slice(&disp.to_le_bytes()).unwrap(),
            Disp32(disp) => HVec::from_slice(&disp.to_le_bytes()).unwrap(),
        }
    }
}

impl From<i8> for Displacement {
    fn from(offset: i8) -> Self {
        Disp8(offset)
    }
}

impl From<i32> for Displacement {
    fn from(offset: i32) -> Self {
        Disp32(offset)
    }
}

pub use Displacement::*;

/// SIB.scale part of x64 instruction encoding.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub enum Scale {
    Scale1,
    Scale2,
    Scale4,
    Scale8,
}

impl Scale {
    pub fn bits(self) -> u8 {
        match self {
            Scale1 => 0b00,
            Scale2 => 0b01,
            Scale4 => 0b10,
            Scale8 => 0b11,
        }
    }
}

impl From<u8> for Scale {
    fn from(scale: u8) -> Self {
        match scale {
            1 => Scale1,
            2 => Scale2,
            4 => Scale4,
            8 => Scale8,
            i => panic!("Unsupported Scale: {}", i),
        }
    }
}

impl Default for Scale {
    fn default() -> Self {
        Scale1
    }
}

pub use Scale::*;

// Encoding correctness is tested in `super::operand` along with operand conversions.
