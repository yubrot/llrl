use super::{RegAssign, StackFrame};
use crate::backend::native::mem_layout::{Class, Layout};
use xten::asm::*;

/// A llrl call frame consists of the following on the stack:
/// * The arguments passed through the stack:
///   Which arguments are passed on the stack depends on the calling convention.
///   This module has no responsibility for this.
/// * 16-byte alignment padding (if necessary)
/// * Destination to write the return value to (if ret.class == Class::Memory)
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct CallFrame {
    stack_args_eightbytes: usize,
    call_alignment_padding: usize,
    ret_eightbytes: usize,
}

impl CallFrame {
    pub fn new(
        args: impl IntoIterator<Item = CallArg>,
        ret: CallRet,
        stack_frame: &StackFrame,
    ) -> Self {
        let stack_args_eightbytes = args.into_iter().fold(0, |sum, arg| match arg {
            CallArg::Stack(n) | CallArg::StackRev(n) => sum + n,
            _ => sum,
        });
        let ret_eightbytes = match ret {
            CallRet::Reg(_, _) => 0,
            CallRet::Stack(n) => n,
        };

        // 16-byte alignment is required
        let call_alignment_padding =
            (stack_frame.depth + ret_eightbytes + stack_args_eightbytes) % 2;

        Self {
            stack_args_eightbytes,
            call_alignment_padding,
            ret_eightbytes,
        }
    }

    pub fn padding_before_stack_args(&self) -> Layout {
        let num_eightbytes = self.ret_eightbytes + self.call_alignment_padding;
        Layout::memory(num_eightbytes * 8, 8)
    }

    pub fn offset_to_ret_destination(&self) -> Option<usize> {
        (self.ret_eightbytes != 0)
            .then_some((self.stack_args_eightbytes + self.call_alignment_padding) * 8)
    }

    pub fn remnants_after_call(&self) -> Layout {
        let num_eightbytes = self.stack_args_eightbytes + self.call_alignment_padding;
        Layout::memory(num_eightbytes * 8, 8)
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum CallArg {
    Reg(RegAssign, Option<RegAssign>),
    Stack(usize),    // num eightbytes
    StackRev(usize), // num eightbytes
}

impl CallArg {
    pub fn default_args(args: impl IntoIterator<Item = Layout>) -> Vec<Self> {
        // Every arguments are passed through the stack
        args.into_iter()
            .map(|arg| Self::Stack(arg.num_eightbytes()))
            .collect()
    }

    pub fn c_args(args: impl IntoIterator<Item = Layout>, ret: &Layout) -> Vec<Self> {
        static GPS: [Gpr64; 6] = [Rdi, Rsi, Rdx, Rcx, R8, R9];
        static FPS: [Xmm; 8] = [Xmm0, Xmm1, Xmm2, Xmm3, Xmm4, Xmm5, Xmm6, Xmm7];

        let mut gps = GPS.as_slice();
        let mut fps = FPS.as_slice();

        if ret.class == Class::Memory {
            // The memory address to be written to is given by the rdi register
            gps = &gps[1..];
        }

        args.into_iter()
            .map(|arg| match RegAssign::build(&arg, &mut gps, &mut fps) {
                Ok(assigns) => Self::Reg(*assigns.get(0).unwrap(), assigns.get(1).copied()),
                Err(()) => Self::StackRev(arg.num_eightbytes()),
            })
            .collect()
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum CallRet {
    Reg(RegAssign, Option<RegAssign>),
    Stack(usize), // num eightbytes
}

impl CallRet {
    pub fn default(layout: &Layout) -> Self {
        // same as C
        Self::c(layout)
    }

    pub fn c(layout: &Layout) -> Self {
        // This is compatible with the "last value" of stack_op.
        static GPS: [Gpr64; 2] = [Rax, Rdx];
        static FPS: [Xmm; 2] = [Xmm0, Xmm1];

        match RegAssign::build(layout, &mut GPS.as_slice(), &mut FPS.as_slice()) {
            Ok(assigns) => Self::Reg(*assigns.get(0).unwrap(), assigns.get(1).copied()),
            Err(()) => Self::Stack(layout.num_eightbytes()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn call_arg() {
        assert_eq!(CallArg::default_args([]), Vec::new());
        assert_eq!(
            CallArg::default_args([Layout::integer(4), Layout::integer(4)]),
            vec![CallArg::Stack(1), CallArg::Stack(1)]
        );
        assert_eq!(
            CallArg::c_args(
                [
                    Layout::integer(4),
                    Layout::integer(4),
                    Layout::floating_point(4)
                ],
                &Layout::integer(4)
            ),
            vec![
                CallArg::Reg(RegAssign::new(0, 4, Rdi), None),
                CallArg::Reg(RegAssign::new(0, 4, Rsi), None),
                CallArg::Reg(RegAssign::new(0, 4, Xmm0), None)
            ]
        );
        assert_eq!(
            CallArg::c_args(
                [
                    Layout::product(&[Layout::integer(8), Layout::integer(8), Layout::integer(8)]),
                    Layout::product(&[Layout::integer(8), Layout::integer(8)]),
                ],
                &Layout::memory(24, 8)
            ),
            vec![
                CallArg::StackRev(3),
                CallArg::Reg(RegAssign::new(0, 8, Rsi), Some(RegAssign::new(8, 8, Rdx))),
            ]
        );
    }

    #[test]
    fn call_ret() {
        assert_eq!(CallRet::c(&Layout::memory(24, 8)), CallRet::Stack(3));
        assert_eq!(
            CallRet::c(&Layout::new(16, 8, Class::Integer)),
            CallRet::Reg(RegAssign::new(0, 8, Rax), Some(RegAssign::new(8, 8, Rdx)))
        );
        assert_eq!(
            CallRet::c(&Layout::product(&[
                Layout::integer(8),
                Layout::floating_point(4)
            ])),
            CallRet::Reg(RegAssign::new(0, 8, Rax), Some(RegAssign::new(8, 8, Xmm0)))
        );
    }
}
