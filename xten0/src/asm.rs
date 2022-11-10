//! x64 assembler.

mod encoding;
mod inst;
mod obj;
mod operand;
mod writer;

pub use inst::{WriteInst, WriteInstExt};
pub use obj::*;
pub use operand::*;
pub use writer::{AddressTable, Label, Short, WriteSection, Writer};

#[cfg(test)]
mod tests {
    use super::*;
    use std::io;

    fn asm(f: impl FnOnce(&mut Vec<u8>) -> io::Result<()>) -> Vec<u8> {
        let mut buf = Vec::new();
        f(&mut buf).unwrap();
        buf
    }

    #[test]
    fn test_inst() {
        assert_as!(
            asm(|w| {
                w.adcb(memory(Rax), Bl)?;
                w.adcb(R13B, memory(R9))?;
                w.adcb(Spl, 13i8)?;
                w.adcb(memory(Rax + Rcx * 4), -7i8)?;
                Ok(())
            }),
            r#"
                adcb [rax], bl
                adcb r13b, [r9]
                adcb spl, 13
                adcb [rax + rcx * 4], -7
            "#
        );
        assert_as!(
            asm(|w| {
                w.adcw(Cx, Bx)?;
                w.adcw(memory(11), Bx)?;
                w.adcw(Cx, memory(Rip + 5))?;
                w.adcw(R11W, 1234i16)?;
                Ok(())
            }),
            r#"
                adcw cx, bx
                adcw [11], bx
                adcw cx, [rip + 5]
                adcw r11w, 1234
            "#
        );
        assert_as!(
            asm(|w| {
                w.adcl(_Eax, 4444)?;
                w.adcl(memory(Rbx + 124i8), Esi)?;
                w.adcl(Ebx, 32i8)?;
                w.adcl(Ecx, 100000i32)?;
                Ok(())
            }),
            r#"
                adcd eax, 4444
                adcd [rbx + 124], esi
                adcd ebx, 32
                adcd ecx, 100000
            "#
        );
        assert_as!(
            asm(|w| {
                w.adcq(R13, memory(Rax))?;
                w.adcq(memory(R10), 124680i32)?;
                Ok(())
            }),
            r#"
                adcq r13, [rax]
                adcq [r10], 124680
            "#
        );
        assert_as!(
            asm(|w| {
                w.addpd(Xmm7, Xmm1)?;
                w.addps(Xmm10, memory(Rdi))?;
                w.addss(Xmm4, memory(Rax))?;
                w.andpd(Xmm10, Xmm15)?;
                Ok(())
            }),
            r#"
                addpd xmm7, xmm1
                addps xmm10, [rdi]
                addss xmm4, [rax]
                andpd xmm10, xmm15
            "#
        );
        assert_as!(
            asm(|w| {
                w.blendpd(Xmm4, Xmm5, 6i8)?;
                w.blendpd(Xmm4, memory(Rax + 4i8), 6i8)?;
                w.blendvpd(Xmm2, Xmm3, _Xmm0)?;
                Ok(())
            }),
            r#"
                blendpd xmm4, xmm5, 6
                blendpd xmm4, [rax + 4], 6
                blendvpd xmm2, xmm3, xmm0
            "#
        );
        assert_as!(
            asm(|w| {
                w.bswap(Ebx)?;
                w.bswap(Rsp)?;
                w.btq(memory(Rax + Rbx * 8 + 1024), 5i8)?;
                w.cli()?;
                w.extractps(Rcx, Xmm4, 4i8)?;
                w.setb(Dl)?;
                Ok(())
            }),
            r#"
                bswap ebx
                bswap rsp
                btq [rax + rbx * 8 + 1024], 5
                cli
                extractps rcx, xmm4, 4
                setb dl
            "#
        );
        assert_as!(
            asm(|w| {
                w.callq(2)?; // call rdi :: F2 /r (2 bytes)
                w.callq(Rdi)?;
                w.callq(memory(Rax + R10))?;
                w.jae(0i8)?;
                w.retq()?;
                w.verw(Ax)?;
                Ok(())
            }),
            r#"
                call hoge
                call rdi
                hoge:
                call [rax + r10]
                jae fuga
                fuga:
                retq
                verw ax
            "#
        );
        assert_as!(
            asm(|w| {
                w.crc32b(R10D, Dl)?;
                w.crc32b(Rbx, Dl)?;
                w.crc32w(Eax, Cx)?;
                w.crc32l(Ebx, Edx)?;
                Ok(())
            }),
            r#"
                crc32b r10d, dl
                crc32b rbx, dl
                crc32w eax, cx
                crc32d ebx, edx
            "#
        );
        assert_as!(
            asm(|w| {
                w.divb(memory(Rax))?;
                w.divw(Ax)?;
                w.divl(Ecx)?;
                w.divq(R10)?;
                Ok(())
            }),
            r#"
                divb [rax]
                divw ax
                divd ecx
                divq r10
            "#
        );
        assert_as!(
            asm(|w| {
                w.movl(Ebx, 1234i32)?;
                w.movl(R10D, 1234i32)?;
                w.movq(Rdx, 1234i32)?;
                w.movq(R11, 1234i32)?;
                Ok(())
            }),
            r#"
                mov ebx, 1234
                mov r10d, 1234
                mov rdx, 1234
                mov r11, 1234
            "#
        );
        assert_as!(
            asm(|w| {
                w.pushq(5i8)?;
                // w.pushq(1000i16)?; // NOTE: GAS encodes this as `68 id`, not `66 68 iw`
                w.pushq(70000i32)?;
                w.pushq(memory(Rbx))?;
                w.pushq(Rcx)?;
                w.pushw(Ax)?;
                w.popq(Rbx)?;
                w.popw(memory(Rax))?;
                Ok(())
            }),
            r#"
                pushq 5
                /* pushq 1000 */
                pushq 70000
                pushq [rbx]
                pushq rcx
                pushw ax
                popq rbx
                popw [rax]
            "#
        );
    }
}
