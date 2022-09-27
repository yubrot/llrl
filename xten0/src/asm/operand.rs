// https://www.intel.com/content/www/us/en/developer/articles/technical/intel-sdm.html

use super::encoding::{Displacement, Reg, Rm, Scale};
use derive_new::new;
use std::ops::{Add, Mul, Sub};

// 64-bit general-purpose (GP) register.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub enum Gpr64 {
    Rax,
    Rcx,
    Rdx,
    Rbx,
    Rsp,
    Rbp,
    Rsi,
    Rdi,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

impl Gpr64 {
    pub fn register_code(self) -> u8 {
        match self {
            Rax => 0,
            Rcx => 1,
            Rdx => 2,
            Rbx => 3,
            Rsp => 4,
            Rbp => 5,
            Rsi => 6,
            Rdi => 7,
            R8 => 8,
            R9 => 9,
            R10 => 10,
            R11 => 11,
            R12 => 12,
            R13 => 13,
            R14 => 14,
            R15 => 15,
        }
    }
}

impl From<Gpr64> for Reg {
    fn from(gpr: Gpr64) -> Self {
        Self::new(gpr.register_code())
    }
}

impl From<Gpr64> for Rm {
    fn from(gpr: Gpr64) -> Self {
        Self::new(0b11, gpr.register_code())
    }
}

pub use Gpr64::*;

// 32-bit general-purpose (GP) register.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub enum Gpr32 {
    Eax,
    Ecx,
    Edx,
    Ebx,
    Esp,
    Ebp,
    Esi,
    Edi,
    R8D,
    R9D,
    R10D,
    R11D,
    R12D,
    R13D,
    R14D,
    R15D,
}

impl Gpr32 {
    pub fn register_code(self) -> u8 {
        match self {
            Eax => 0,
            Ecx => 1,
            Edx => 2,
            Ebx => 3,
            Esp => 4,
            Ebp => 5,
            Esi => 6,
            Edi => 7,
            R8D => 8,
            R9D => 9,
            R10D => 10,
            R11D => 11,
            R12D => 12,
            R13D => 13,
            R14D => 14,
            R15D => 15,
        }
    }
}

impl From<Gpr32> for Reg {
    fn from(gpr: Gpr32) -> Self {
        Self::new(gpr.register_code())
    }
}

impl From<Gpr32> for Rm {
    fn from(gpr: Gpr32) -> Self {
        Self::new(0b11, gpr.register_code())
    }
}

pub use Gpr32::*;

// 16-bit general-purpose (GP) register.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub enum Gpr16 {
    Ax,
    Cx,
    Dx,
    Bx,
    Sp,
    Bp,
    Si,
    Di,
    R8W,
    R9W,
    R10W,
    R11W,
    R12W,
    R13W,
    R14W,
    R15W,
}

impl Gpr16 {
    pub fn register_code(self) -> u8 {
        match self {
            Ax => 0,
            Cx => 1,
            Dx => 2,
            Bx => 3,
            Sp => 4,
            Bp => 5,
            Si => 6,
            Di => 7,
            R8W => 8,
            R9W => 9,
            R10W => 10,
            R11W => 11,
            R12W => 12,
            R13W => 13,
            R14W => 14,
            R15W => 15,
        }
    }
}

impl From<Gpr16> for Reg {
    fn from(gpr: Gpr16) -> Self {
        Self::new(gpr.register_code())
    }
}

impl From<Gpr16> for Rm {
    fn from(gpr: Gpr16) -> Self {
        Self::new(0b11, gpr.register_code())
    }
}

pub use Gpr16::*;

// 8-bit general-purpose (GP) register.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub enum Gpr8 {
    Al,
    Cl,
    Dl,
    Bl,
    // Ah, Ch, Dh, Bh are unsupported
    Spl,
    Bpl,
    Sil,
    Dil,
    R8B,
    R9B,
    R10B,
    R11B,
    R12B,
    R13B,
    R14B,
    R15B,
}

impl Gpr8 {
    pub fn register_code(self) -> u8 {
        match self {
            Al => 0,
            Cl => 1,
            Dl => 2,
            Bl => 3,
            Spl => 4,
            Bpl => 5,
            Sil => 6,
            Dil => 7,
            R8B => 8,
            R9B => 9,
            R10B => 10,
            R11B => 11,
            R12B => 12,
            R13B => 13,
            R14B => 14,
            R15B => 15,
        }
    }

    pub fn requires_rex(self) -> bool {
        matches!(self, Spl | Bpl | Sil | Dil)
    }
}

impl From<Gpr8> for Reg {
    fn from(gpr: Gpr8) -> Self {
        Self::new(gpr.register_code()).force_rex_prefix(gpr.requires_rex())
    }
}

impl From<Gpr8> for Rm {
    fn from(gpr: Gpr8) -> Self {
        Self::new(0b11, gpr.register_code()).force_rex_prefix(gpr.requires_rex())
    }
}

pub use Gpr8::*;

// 64-bit instruction pointer register.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct Rip;

// XMM register.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub enum Xmm {
    Xmm0,
    Xmm1,
    Xmm2,
    Xmm3,
    Xmm4,
    Xmm5,
    Xmm6,
    Xmm7,
    Xmm8,
    Xmm9,
    Xmm10,
    Xmm11,
    Xmm12,
    Xmm13,
    Xmm14,
    Xmm15,
}

impl Xmm {
    pub fn encoding_index(self) -> u8 {
        match self {
            Xmm0 => 0,
            Xmm1 => 1,
            Xmm2 => 2,
            Xmm3 => 3,
            Xmm4 => 4,
            Xmm5 => 5,
            Xmm6 => 6,
            Xmm7 => 7,
            Xmm8 => 8,
            Xmm9 => 9,
            Xmm10 => 10,
            Xmm11 => 11,
            Xmm12 => 12,
            Xmm13 => 13,
            Xmm14 => 14,
            Xmm15 => 15,
        }
    }
}

impl From<Xmm> for Reg {
    fn from(xmm: Xmm) -> Self {
        Self::new(xmm.encoding_index())
    }
}

impl From<Xmm> for Rm {
    fn from(xmm: Xmm) -> Self {
        Self::new(0b11, xmm.encoding_index())
    }
}

pub use Xmm::*;

/// Pair of SIB.scale and SIB.index.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct IndexScale {
    pub index: Gpr64,
    pub scale: Scale,
}

impl Mul<u8> for Gpr64 {
    type Output = IndexScale;

    fn mul(self, scale: u8) -> Self::Output {
        self * Scale::from(scale)
    }
}

impl Mul<Scale> for Gpr64 {
    type Output = IndexScale;

    fn mul(self, scale: Scale) -> Self::Output {
        // This implies that `idxs.index.register_code()` will never be 0b100.
        assert!(self != Rsp, "Cannot use RSP as an index register");
        IndexScale { index: self, scale }
    }
}

/// Memory address. `Base + Disp + Idxs`
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, new)]
pub struct Address<Base, Disp, Idxs> {
    pub base: Base,
    pub disp: Disp,
    pub idxs: Idxs,
}

impl From<i32> for Address<(), i32, ()> {
    fn from(disp: i32) -> Self {
        Self::new((), disp, ())
    }
}

impl From<Gpr64> for Address<Gpr64, (), ()> {
    fn from(base: Gpr64) -> Self {
        Self::new(base, (), ())
    }
}

impl From<Rip> for Address<Rip, (), ()> {
    fn from(base: Rip) -> Self {
        Self::new(base, (), ())
    }
}

macro_rules! impl_add_sub_disp {
    // Address<$base, (), _> + $disp
    // Address<$base, (), _> - $disp
    // Address<$base, $disp, _> + $disp
    // Address<$base, $disp, _> - $disp
    // $base + $disp
    // $base - $disp
    ($base:ty, $disp:ty) => {
        impl<Idxs> Add<$disp> for Address<$base, (), Idxs> {
            type Output = Address<$base, $disp, Idxs>;

            fn add(self, disp: $disp) -> Self::Output {
                Address::new(self.base, disp, self.idxs)
            }
        }

        impl<Idxs> Sub<$disp> for Address<$base, (), Idxs> {
            type Output = Address<$base, $disp, Idxs>;

            fn sub(self, disp: $disp) -> Self::Output {
                Address::new(self.base, -disp, self.idxs)
            }
        }

        impl<Idxs> Add<$disp> for Address<$base, $disp, Idxs> {
            type Output = Address<$base, $disp, Idxs>;

            fn add(self, disp: $disp) -> Self::Output {
                Address::new(self.base, self.disp + disp, self.idxs)
            }
        }

        impl<Idxs> Sub<$disp> for Address<$base, $disp, Idxs> {
            type Output = Address<$base, $disp, Idxs>;

            fn sub(self, disp: $disp) -> Self::Output {
                Address::new(self.base, self.disp - disp, self.idxs)
            }
        }

        impl Add<$disp> for $base {
            type Output = Address<$base, $disp, ()>;

            fn add(self, disp: $disp) -> Self::Output {
                Address::from(self) + disp
            }
        }

        impl Sub<$disp> for $base {
            type Output = Address<$base, $disp, ()>;

            fn sub(self, disp: $disp) -> Self::Output {
                Address::from(self) - disp
            }
        }
    };
}

impl_add_sub_disp!(Gpr64, i8); // GP + disp8
impl_add_sub_disp!(Gpr64, i32); // GP + disp32
impl_add_sub_disp!(Rip, i32); // RIP + disp32 (RIP-relative)

macro_rules! impl_add_idxs {
    // Address<$base, $disp, ()> + IndexScale
    // Address<$base, $disp, ()> + GP
    ($base:ty, $disp:ty) => {
        impl Add<IndexScale> for Address<$base, $disp, ()> {
            type Output = Address<$base, $disp, IndexScale>;

            fn add(self, idxs: IndexScale) -> Self::Output {
                Address::new(self.base, self.disp, idxs)
            }
        }

        impl Add<Gpr64> for Address<$base, $disp, ()> {
            type Output = Address<$base, $disp, IndexScale>;

            #[allow(clippy::suspicious_arithmetic_impl)]
            fn add(self, idxs: Gpr64) -> Self::Output {
                Address::new(self.base, self.disp, idxs * Scale::Scale1)
            }
        }
    };

    // $src + IndexScale
    // $src + GP
    //   where Address<$base, $disp, ()>: From<$src>
    ($src:ty, $base:ty, $disp:ty) => {
        impl Add<IndexScale> for $src {
            type Output = Address<$base, $disp, IndexScale>;

            fn add(self, idxs: IndexScale) -> Self::Output {
                Address::<$base, $disp, ()>::from(self) + idxs
            }
        }

        impl Add<Gpr64> for $src {
            type Output = Address<$base, $disp, IndexScale>;

            fn add(self, idxs: Gpr64) -> Self::Output {
                Address::<$base, $disp, ()>::from(self) + idxs
            }
        }
    };
}

impl_add_idxs!(i32, (), i32); // disp32 + IndexScale
impl_add_idxs!((), i32);
impl_add_idxs!(Gpr64, Gpr64, ()); // GP + IndexScale
impl_add_idxs!(Gpr64, ());
impl_add_idxs!(Gpr64, i8); // GP + disp8 + IndexScale
impl_add_idxs!(Gpr64, i32); // GP + disp32 + IndexScale

/// A memory operand. Semantically equivalent to `[..]` in Intel assembler syntax.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct Memory<T = Rm>(pub T);

impl From<Memory> for Rm {
    fn from(Memory(rm): Memory) -> Self {
        rm
    }
}

// i32 -> Address<(), i32, ()>
impl From<Memory<i32>> for Rm {
    fn from(Memory(disp): Memory<i32>) -> Self {
        Self::from(Memory(Address::from(disp)))
    }
}

// Gpr64 -> Address<Gpr64, (), ()>
impl From<Memory<Gpr64>> for Rm {
    fn from(Memory(gpr): Memory<Gpr64>) -> Self {
        Self::from(Memory(Address::from(gpr)))
    }
}

// Rip -> Address<Rip, (), ()>
impl From<Memory<Rip>> for Rm {
    fn from(Memory(rip): Memory<Rip>) -> Self {
        Self::from(Memory(Address::from(rip)))
    }
}

// [disp32]
impl From<Memory<Address<(), i32, ()>>> for Rm {
    fn from(Memory(Address { disp, .. }): Memory<Address<(), i32, ()>>) -> Self {
        assert!(0 <= disp);
        Self::new(0b00, 0b100) // Use SIB
            .sib_base(0b0101) // No base register is encoded and use disp32
            .sib_index(0b0100) // No index register is encoded
            .disp(disp)
    }
}

// [disp32 + IndexScale]
impl From<Memory<Address<(), i32, IndexScale>>> for Rm {
    fn from(Memory(Address { disp, idxs, .. }): Memory<Address<(), i32, IndexScale>>) -> Self {
        Self::new(0b00, 0b100) // Use SIB
            .sib_base(0b0101) // No base register is encoded and use disp32
            .sib_index(idxs.index.register_code())
            .sib_scale(idxs.scale)
            .disp(disp)
    }
}

// [RIP]
impl From<Memory<Address<Rip, (), ()>>> for Rm {
    fn from(Memory(addr): Memory<Address<Rip, (), ()>>) -> Self {
        Self::from(Memory(addr + 0))
    }
}

// [RIP + disp32]
impl From<Memory<Address<Rip, i32, ()>>> for Rm {
    fn from(Memory(Address { disp, .. }): Memory<Address<Rip, i32, ()>>) -> Self {
        Self::new(0b00, 0b0101) // Use RIP-relative addressing
            .disp(disp)
    }
}

// [GP64]
impl From<Memory<Address<Gpr64, (), ()>>> for Rm {
    fn from(Memory(Address { base, .. }): Memory<Address<Gpr64, (), ()>>) -> Self {
        match base {
            // SIB-byte required for RSP-based or R12-based addressing.
            Rsp | R12 => {
                Self::new(0b00, 0b100) // Use SIB
                    .sib_base(base.register_code())
                    .sib_index(0b0100) // No index register is encoded
            }
            // Using RBP or R13 without displacement must be done using mod=01 with a displacement of 0.
            Rbp | R13 => {
                Self::new(0b01, base.register_code()) // Use disp8
                    .disp(0i8)
            }
            _ => Self::new(0b00, base.register_code()),
        }
    }
}

// [GP64 + disp]
impl<Disp> From<Memory<Address<Gpr64, Disp, ()>>> for Rm
where
    Disp: Into<Displacement>,
{
    fn from(Memory(Address { base, disp, .. }): Memory<Address<Gpr64, Disp, ()>>) -> Self {
        let disp = disp.into();
        match base {
            // SIB-byte required for RSP-based or R12-based addressing.
            Rsp | R12 => {
                Self::new(disp.modrm_mod(), 0b100) // Use SIB and use dispN
                    .sib_base(base.register_code())
                    .sib_index(0b0100) // No index register is encoded
                    .disp(disp)
            }
            _ => Self::new(disp.modrm_mod(), base.register_code()).disp(disp),
        }
    }
}

// [GP64 + IndexScale]
impl From<Memory<Address<Gpr64, (), IndexScale>>> for Rm {
    fn from(Memory(Address { base, idxs, .. }): Memory<Address<Gpr64, (), IndexScale>>) -> Self {
        match base {
            // Explicit displacement is required to be used with RBP or R13.
            Rbp | R13 => {
                Self::new(0b01, 0b100) // Use SIB and disp8
                    .sib_base(base.register_code())
                    .sib_index(idxs.index.register_code())
                    .sib_scale(idxs.scale)
                    .disp(0i8)
            }
            _ => Self::new(0b00, 0b100) // Use SIB
                .sib_base(base.register_code())
                .sib_index(idxs.index.register_code())
                .sib_scale(idxs.scale),
        }
    }
}

// [GP64 + disp + IndexScale]
impl<Disp> From<Memory<Address<Gpr64, Disp, IndexScale>>> for Rm
where
    Disp: Into<Displacement>,
{
    fn from(
        Memory(Address { base, disp, idxs }): Memory<Address<Gpr64, Disp, IndexScale>>,
    ) -> Self {
        let disp = disp.into();
        Self::new(disp.modrm_mod(), 0b100) // Use SIB and dispN
            .sib_base(base.register_code())
            .sib_index(idxs.index.register_code())
            .sib_scale(idxs.scale)
            .disp(disp)
    }
}

/// A type-erased memory operand.
pub fn memory<A: MemoryAddress>(address: A) -> Memory {
    A::common_rep(Memory(address))
}

pub trait MemoryAddress: Sized {
    fn common_rep(m: Memory<Self>) -> Memory;
}

impl<T> MemoryAddress for T
where
    Rm: From<Memory<T>>,
{
    fn common_rep(m: Memory<Self>) -> Memory {
        Memory(m.into())
    }
}

// Some fixed operands:
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct _Al;
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct _Ax;
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct _Eax;
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct _Rax;
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct _Cl;
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct _Dx;
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct _Xmm0;
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct _1;
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct _3;

#[cfg(test)]
mod tests {
    use super::super::encoding::{ModRM, PartOfOpcode, RegInOpcode};
    use super::*;

    // movq r/m64 r64: Move r64 to r/m64.
    fn movq(dest: impl Into<Rm>, src: impl Into<Reg>) -> Vec<u8> {
        // REX.W+ 89 /r
        let modrm = ModRM::new(src, dest);
        std::iter::empty()
            .chain(modrm.rex_byte(true)) // REX prefix
            .chain([0x89, modrm.byte()]) // Opcode, ModR/M
            .chain(modrm.sib_byte()) // SIB
            .chain(modrm.disp_bytes().into_iter().flatten()) // Displacement
            .collect()
    }

    // negl r/m32: Two's complement negate r/m32.
    fn negl(operand: impl Into<Rm>) -> Vec<u8> {
        // F7 /3
        let modrm = ModRM::new(PartOfOpcode(3), operand);
        std::iter::empty()
            .chain(modrm.rex_byte(false)) // REX prefix
            .chain([0xF7, modrm.byte()]) // Opcode, ModR/M
            .chain(modrm.sib_byte()) // SIB
            .chain(modrm.disp_bytes().into_iter().flatten()) // Displacement
            .collect()
    }

    // movw r16, imm16: Move imm16 to r16.
    fn movw(dest: impl Into<RegInOpcode>, src: i16) -> Vec<u8> {
        // B8 +rw +iw
        let rio = dest.into();
        std::iter::empty()
            .chain([0x66]) // Mandatory prefix
            .chain(rio.rex_byte(false)) // REX prefix
            .chain([0xB8 + rio.byte_added_to_opcode]) // Opcode, ModR/M
            .chain(src.to_le_bytes()) // Immediate
            .collect()
    }

    // movb r/m8 r8: Move r8 to r/m8.
    fn movb(dest: impl Into<Rm>, src: impl Into<Reg>) -> Vec<u8> {
        // 88 /r
        let modrm = ModRM::new(src, dest);
        std::iter::empty()
            .chain(modrm.rex_byte(false)) // REX prefix
            .chain([0x88, modrm.byte()]) // Opcode, ModR/M
            .chain(modrm.sib_byte()) // SIB
            .chain(modrm.disp_bytes().into_iter().flatten()) // Displacement
            .collect()
    }

    // movsd xmm1, xmm2/m64: Move scalar double-precision floating-point value from xmm2/m64 to xmm1 register.
    fn movsd(dest: impl Into<Reg>, src: impl Into<Rm>) -> Vec<u8> {
        // F2 0F 10 /r
        let modrm = ModRM::new(dest, src);
        std::iter::empty()
            .chain([0xf2]) // Mandatory prefix
            .chain(modrm.rex_byte(false)) // REX prefix
            .chain([0x0f, 0x10, modrm.byte()]) // Opcode, ModR/M
            .chain(modrm.sib_byte()) // SIB
            .chain(modrm.disp_bytes().into_iter().flatten()) // Displacement
            .collect()
    }

    fn general_purpose_registers() -> Vec<(Gpr64, &'static str)> {
        vec![
            (Rax, "rax"),
            (Rbx, "rbx"),
            (Rsp, "rsp"), // special case 1
            (Rbp, "rbp"), // special case 2
            (R12, "r12"), // special case 1
            (R13, "r13"), // special case 2
            (R14, "r14"), // An extension bit is necessary
            (R15, "r15"), // An extension bit is necessary
        ]
    }

    #[test]
    fn gpr64() {
        for (ra, sa) in general_purpose_registers() {
            for (rb, sb) in general_purpose_registers() {
                if ra == rb {
                    continue;
                }
                assert_as!(movq(ra, rb), "movq {}, {}", sa, sb);
            }
        }
    }

    #[test]
    fn gpr32() {
        // It also tests with PartOfOpcode
        assert_as!(negl(Eax), "neg eax");
        assert_as!(negl(Ecx), "neg ecx");
        assert_as!(negl(R13D), "neg r13d");
    }

    #[test]
    fn gpr16() {
        // It also tests with RegInOpcode
        assert_as!(movw(Ax, 30000), "movw ax, 30000");
        assert_as!(movw(R8W, -1234), "movw r8w, -1234");
    }

    #[test]
    fn gpr8() {
        // It also tests force_rex_prefix
        assert_as!(movb(Al, Cl), "movb al, cl");
        assert_as!(movb(memory(Rax + Rcx * 2), Dl), "movb [rax + rcx * 2], dl");
        assert_as!(movb(Spl, Bl), "movb spl, bl");
        assert_as!(movb(Al, Dil), "movb al, dil");
    }

    #[test]
    fn xmm() {
        assert_as!(movsd(Xmm1, Xmm3), "movsd xmm1, xmm3");
        assert_as!(movsd(Xmm10, memory(Rdx)), "movsd xmm10, [rdx]");
        assert_as!(
            movsd(Xmm4, memory(Rax + Rcx * 8 - 8i8)),
            "movsd xmm4, [rax + rcx * 8 - 8]"
        );
    }

    #[test]
    fn memory_address() {
        // absolute addressing
        assert_as!(movq(memory(124), Rax), "mov [124], rax");
        assert_as!(movq(memory(124 + Rcx * 2), Rax), "mov [124 + rcx * 2], rax");
        assert_as!(
            movq(memory(1024 + Rdx * 4), Rax),
            "mov [1024 + rdx * 4], rax"
        );
        assert_as!(
            movq(memory(4096 + R14 * 8), Rax),
            "mov [4096 + r14 * 8], rax"
        );

        // RIP-relative addressing
        assert_as!(movq(memory(Rip), Rcx), "mov [rip], rcx");
        assert_as!(movq(memory(Rip + 16), Rcx), "mov [rip + 16], rcx");
        assert_as!(movq(memory(Rip - 64), Rcx), "mov [rip - 64], rcx");

        // $disp + $disp
        assert_as!(movq(memory(Rip - 64 + 32), Rcx), "mov [rip - 32], rcx");
        assert_as!(movq(memory(Rip + 64 - 32), Rcx), "mov [rip + 32], rcx");

        for (ra, sa) in general_purpose_registers() {
            for (rb, sb) in general_purpose_registers() {
                if ra == rb {
                    continue;
                }

                // [Base], [Base + disp8], [Base + disp32]
                assert_as!(
                    [
                        movq(memory(ra), rb),
                        movq(memory(ra + 12i8), rb),
                        movq(memory(ra + 1024), rb),
                    ]
                    .into_iter()
                    .flatten()
                    .collect::<Vec<_>>(),
                    r#"
                        mov [{}], {}
                        mov [{} + 12], {}
                        mov [{} + 1024], {}
                    "#,
                    sa,
                    sb,
                    sa,
                    sb,
                    sa,
                    sb
                );

                // Cannot use RSP as an index register
                if rb != Rsp {
                    // [Base + Index] *2, [Base + disp8 + Index], [Base + disp32 + Index]
                    assert_as!(
                        [
                            movq(memory(ra + rb), Rcx),
                            movq(memory(ra + rb * 4), Rcx),
                            movq(memory(ra - 8i8 + rb), Rcx),
                            movq(memory(ra - 512 + rb * 4), Rcx),
                        ]
                        .into_iter()
                        .flatten()
                        .collect::<Vec<_>>(),
                        r#"
                            mov [{} + {}], rcx
                            mov [{} + {} * 4], rcx
                            mov [{} - 8 + {}], rcx
                            mov [{} - 512 + {} * 4], rcx
                        "#,
                        sa,
                        sb,
                        sa,
                        sb,
                        sa,
                        sb,
                        sa,
                        sb
                    );
                }
            }
        }
    }
}
