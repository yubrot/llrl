//! x64 assembler.

#[cfg(test)]
#[macro_use]
mod gas;

mod encoding;
mod instr;
mod operand;

pub use instr::WriteInstExt;
pub use operand::*;

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
    fn test_instr() {
        assert_as!(
            asm(|w| {
                w.adcb(memory(Rax), Bl)?;
                w.adcb(R13B, memory(R9))?;
                w.adcb(Spl, 13i8)?;
                w.adcb(memory(Rax + Rcx * 4), -7i8)?;

                // w.adcw(Cx, Bx)?; // NOTE: This can be encoded by both `13 /r` and `11 /r`
                w.adcw(memory(11), Bx)?;
                w.adcw(Cx, memory(Rip + 5))?;
                w.adcw(R11W, 1234i16)?;

                w.adcl(_Eax, 4444)?;
                w.adcl(memory(Rbx + 124i8), Esi)?;
                w.adcl(Ebx, 32i8)?;
                w.adcl(Ecx, 100000i32)?;

                w.adcq(R13, memory(Rax))?;
                w.adcq(memory(R10), 124680i32)?;
                Ok(())
            }),
            r#"
                adcb [rax], bl
                adcb r13b, [r9]
                adcb spl, 13
                adcb [rax + rcx * 4], -7

                /* adcw cx, bx */
                adcw [11], bx
                adcw cx, [rip + 5]
                adcw r11w, 1234

                adcd eax, 4444
                adcd [rbx + 124], esi
                adcd ebx, 32
                adcd ecx, 100000

                adcq r13, [rax]
                adcq [r10], 124680
            "#
        );
    }
}
