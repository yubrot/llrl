// https://www.intel.com/content/www/us/en/developer/articles/technical/intel-sdm.html

use super::modrm::{Displacement, Reg, Rm, Scale};
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
    pub fn encoding_index(self) -> u8 {
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
        Self::new(gpr.encoding_index())
    }
}

impl From<Gpr64> for Rm {
    fn from(gpr: Gpr64) -> Self {
        Self::new(0b11, gpr.encoding_index())
    }
}

pub use Gpr64::*;

// 64-bit instruction pointer register.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct Rip;

/// For `/digit` opcode.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct PartOfOpcode(pub u8);

impl From<PartOfOpcode> for Reg {
    fn from(PartOfOpcode(value): PartOfOpcode) -> Self {
        assert!(value <= 0b111); // /0../7
        Self::new(value)
    }
}

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
        // This implies that `idxs.index.encoded_index()` will never be 0b100.
        assert!(self != Rsp, "Cannot use RSP as an index register");
        IndexScale { index: self, scale }
    }
}

/// Memory address. `Base + Disp + Idxs`
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct Address<Base, Disp, Idxs> {
    pub base: Base,
    pub disp: Disp,
    pub idxs: Idxs,
}

impl<Base, Disp, Idxs> Address<Base, Disp, Idxs> {
    pub fn new(base: Base, disp: Disp, idxs: Idxs) -> Self {
        Self { base, disp, idxs }
    }
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
pub struct Memory<T>(pub T);

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
            .sib_index(idxs.index.encoding_index())
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

// [GP]
impl From<Memory<Address<Gpr64, (), ()>>> for Rm {
    fn from(Memory(Address { base, .. }): Memory<Address<Gpr64, (), ()>>) -> Self {
        match base {
            // SIB-byte required for RSP-based or R12-based addressing.
            Rsp | R12 => {
                Self::new(0b00, 0b100) // Use SIB
                    .sib_base(base.encoding_index())
                    .sib_index(0b0100) // No index register is encoded
            }
            // Using RBP or R13 without displacement must be done using mod=01 with a displacement of 0.
            Rbp | R13 => {
                Self::new(0b01, base.encoding_index()) // Use disp8
                    .disp(0i8)
            }
            _ => Self::new(0b00, base.encoding_index()),
        }
    }
}

// [GP + disp]
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
                    .sib_base(base.encoding_index())
                    .sib_index(0b0100) // No index register is encoded
                    .disp(disp)
            }
            _ => Self::new(disp.modrm_mod(), base.encoding_index()).disp(disp),
        }
    }
}

// [GP + IndexScale]
impl From<Memory<Address<Gpr64, (), IndexScale>>> for Rm {
    fn from(Memory(Address { base, idxs, .. }): Memory<Address<Gpr64, (), IndexScale>>) -> Self {
        match base {
            // Explicit displacement is required to be used with RBP or R13.
            Rbp | R13 => {
                Self::new(0b01, 0b100) // Use SIB and disp8
                    .sib_base(base.encoding_index())
                    .sib_index(idxs.index.encoding_index())
                    .sib_scale(idxs.scale)
                    .disp(0i8)
            }
            _ => Self::new(0b00, 0b100) // Use SIB
                .sib_base(base.encoding_index())
                .sib_index(idxs.index.encoding_index())
                .sib_scale(idxs.scale),
        }
    }
}

// [GP + disp + IndexScale]
impl<Disp> From<Memory<Address<Gpr64, Disp, IndexScale>>> for Rm
where
    Disp: Into<Displacement>,
{
    fn from(
        Memory(Address { base, disp, idxs }): Memory<Address<Gpr64, Disp, IndexScale>>,
    ) -> Self {
        let disp = disp.into();
        Self::new(disp.modrm_mod(), 0b100) // Use SIB and dispN
            .sib_base(base.encoding_index())
            .sib_index(idxs.index.encoding_index())
            .sib_scale(idxs.scale)
            .disp(disp)
    }
}

#[cfg(test)]
mod tests {
    use super::super::modrm::ModRM;
    use super::*;
    use std::fs::File;
    use std::io::{Read, Write};
    use std::path::Path;
    use std::process::Command;
    use tempfile::tempdir;

    fn exec(dir: impl AsRef<Path>, program: &str, args: &[&str]) {
        let status = Command::new(program)
            .current_dir(dir)
            .args(args)
            .status()
            .unwrap();
        assert!(status.success(), "{} failed with: {}", program, status);
    }

    fn dump(asm: &str) -> Vec<u8> {
        let dir = tempdir().unwrap();

        // Write to asm.S
        let mut h = File::create(dir.path().join("asm.S")).unwrap();
        writeln!(&mut h, ".intel_syntax noprefix").unwrap();
        writeln!(&mut h, "{}", asm).unwrap();
        // Assemble asm.S to asm.o
        exec(dir.path(), "as", &["-o", "asm.o", "asm.S"]);
        // Extract .text section into mc.bin
        exec(
            dir.path(),
            "objcopy",
            &["--dump-section", ".text=mc.bin", "asm.o"],
        );
        // Read the machine-code from mc.bin
        let mut out = Vec::new();
        File::open(dir.path().join("mc.bin"))
            .unwrap()
            .read_to_end(&mut out)
            .unwrap();
        out
    }

    macro_rules! assert_dump {
        ($e:expr, $( $t:tt )*) => {
            let asm = format!($( $t )*);
            assert_eq!(dump(&asm), $e, "{}", asm);
        };
    }

    // MOV r/m64 r64: Move r64 to r/m64.
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

    // NEG r/m64: Two's complement negate r/m64.
    fn negq(operand: impl Into<Rm>) -> Vec<u8> {
        // REX.W+ F7 /3
        let modrm = ModRM::new(PartOfOpcode(3), operand);
        std::iter::empty()
            .chain(modrm.rex_byte(true)) // REX prefix
            .chain([0xF7, modrm.byte()]) // Opcode, ModR/M
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
                assert_dump!(movq(ra, rb), "movq {}, {}", sa, sb);
            }
        }
    }

    #[test]
    fn part_of_opcode() {
        assert_dump!(negq(Rax), "negq rax");
        assert_dump!(negq(Rcx), "negq rcx");
    }

    #[test]
    fn memory() {
        // absolute addressing
        assert_dump!(movq(Memory(124), Rax), "mov [124], rax");
        assert_dump!(movq(Memory(124 + Rcx * 2), Rax), "mov [124 + rcx * 2], rax");
        assert_dump!(
            movq(Memory(1024 + Rdx * 4), Rax),
            "mov [1024 + rdx * 4], rax"
        );
        assert_dump!(
            movq(Memory(4096 + R14 * 8), Rax),
            "mov [4096 + r14 * 8], rax"
        );

        // RIP-relative addressing
        assert_dump!(movq(Memory(Rip), Rcx), "mov [rip], rcx");
        assert_dump!(movq(Memory(Rip + 16), Rcx), "mov [rip + 16], rcx");
        assert_dump!(movq(Memory(Rip - 64), Rcx), "mov [rip - 64], rcx");

        for (ra, sa) in general_purpose_registers() {
            for (rb, sb) in general_purpose_registers() {
                if ra == rb {
                    continue;
                }

                // [Base]
                assert_dump!(movq(Memory(ra), rb), "mov [{}], {}", sa, sb);
                // [Base + disp8]
                assert_dump!(movq(Memory(ra + 12i8), rb), "mov [{} + 12], {}", sa, sb);
                // [Base + disp32]
                assert_dump!(movq(Memory(ra + 1024), rb), "mov [{} + 1024], {}", sa, sb);

                // Cannot use RSP as an index register
                if rb != Rsp {
                    // [Base + Index]
                    assert_dump!(movq(Memory(ra + rb), Rcx), "mov [{} + {}], rcx", sa, sb);
                    assert_dump!(
                        movq(Memory(ra + rb * 4), Rcx),
                        "mov [{} + {} * 4], rcx",
                        sa,
                        sb
                    );
                    // [Base + disp8 + Index]
                    assert_dump!(
                        movq(Memory(ra - 8i8 + rb), Rcx),
                        "mov [{} - 8 + {}], rcx",
                        sa,
                        sb
                    );
                    // [Base + disp32 + Index]
                    assert_dump!(
                        movq(Memory(ra - 512 + rb * 4), Rcx),
                        "mov [{} - 512 + {} * 4], rcx",
                        sa,
                        sb
                    );
                }
            }
        }
    }
}
