use super::{AssignedReg, RegAssign};
use crate::backend::native::mem_layout::{Class, Layout};
use std::io;
use xten::asm::*;

// In this implementation, we reserve R10 and R11 as temporary registers. This means that there is
// no guarantee that these registers will be preserved throughout the operation of this module.
// These registers are chosen because they are caller-save (volatile) and are not used for function
// arguments.
static TMP_GP1: Gpr64 = Gpr64::R10; // used for sized load/store
static TMP_GP2: Gpr64 = Gpr64::R11; // used for XMM-to-GP-to-mem and mem-to-mem

static LAST_VALUE_GPS: [Gpr64; 2] = [Rax, Rdx];
static LAST_VALUE_FPS: [Xmm; 2] = [Xmm0, Xmm1];

/// Writer for stack aware instructions.
pub trait StackAwareInstWriter {
    fn w(&mut self) -> &mut Writer;

    /// The depth of the current stack frame in eightbytes.
    /// This value is computed during stack-related operations to satisfy 16-byte alignment
    /// requirement in `call_frame`.
    fn depth(&mut self) -> &mut usize;

    fn extend_stack(&mut self, layout: &Layout) -> io::Result<()> {
        if layout.size != 0 {
            self.w().subq(Rsp, layout.num_eightbytes() as i32 * 8)?;
            *self.depth() += layout.num_eightbytes();
        }
        Ok(())
    }

    fn shrink_stack(&mut self, layout: &Layout) -> io::Result<()> {
        if layout.size != 0 {
            self.w().addq(Rsp, layout.num_eightbytes() as i32 * 8)?;
            *self.depth() -= layout.num_eightbytes();
        }
        Ok(())
    }

    fn slide_in_stack(&mut self, src: usize, dst: usize, layout: &Layout) -> io::Result<()> {
        match src.cmp(&dst) {
            std::cmp::Ordering::Equal => {}
            std::cmp::Ordering::Less => {
                // right to left
                for i in (0..layout.num_eightbytes()).rev() {
                    let src = src + i * 8;
                    let dst = dst + i * 8;
                    let size = (layout.size - i * 8).min(8);
                    self.load_eightbyte(TMP_GP2, Rsp + src as i32, size)?;
                    self.store_eightbyte(Rsp + dst as i32, TMP_GP2, size)?;
                }
            }
            std::cmp::Ordering::Greater => {
                // left to right
                for i in 0..layout.num_eightbytes() {
                    let src = src + i * 8;
                    let dst = dst + i * 8;
                    let size = (layout.size - i * 8).min(8);
                    self.load_eightbyte(TMP_GP2, Rsp + src as i32, size)?;
                    self.store_eightbyte(Rsp + dst as i32, TMP_GP2, size)?;
                }
            }
        }
        Ok(())
    }

    fn push_eightbyte(&mut self, reg: impl EightbyteReg) -> io::Result<()> {
        let num = reg.push(self.w())?;
        *self.depth() += num;
        Ok(())
    }

    fn pop_eightbyte(&mut self, reg: impl EightbyteReg) -> io::Result<()> {
        let num = reg.pop(self.w())?;
        *self.depth() -= num;
        Ok(())
    }

    fn load_eightbyte(
        &mut self,
        dst: impl EightbyteReg,
        src_addr: impl LoadStoreAddress,
        size: usize,
    ) -> io::Result<()> {
        dst.load(self.w(), src_addr, size)?;
        Ok(())
    }

    fn store_eightbyte(
        &mut self,
        dst_addr: impl LoadStoreAddress,
        src: impl EightbyteReg,
        size: usize,
    ) -> io::Result<()> {
        src.store(self.w(), dst_addr, size)?;
        Ok(())
    }

    /// Discard the last value.
    fn discard(&mut self, layout: &Layout) -> io::Result<()> {
        if layout.class == Class::Memory {
            // The stack area is used for values classified as Class::Memory
            self.shrink_stack(layout)?;
        } else {
            // The last value is in the specified registers
        }
        Ok(())
    }

    /// Push the last value onto the stack.
    fn push(&mut self, layout: &Layout) -> io::Result<()> {
        if let Ok(assigns) = RegAssign::build(
            layout,
            &mut LAST_VALUE_GPS.as_slice(),
            &mut LAST_VALUE_FPS.as_slice(),
        ) {
            for assign in assigns.into_iter().rev() {
                self.push_eightbyte(assign.reg)?;
            }
        } else {
            // There is nothing to push, or the stack area has already been used for the last value
        }
        Ok(())
    }

    /// Pop the top of the stack value into the specified registers.
    fn pop(&mut self, layout: &Layout) -> io::Result<()> {
        if let Ok(assigns) = RegAssign::build(
            layout,
            &mut LAST_VALUE_GPS.as_slice(),
            &mut LAST_VALUE_FPS.as_slice(),
        ) {
            for assign in assigns {
                self.pop_eightbyte(assign.reg)?;
            }
        } else {
            // There is nothing to pop, or the stack area has already been used for the last value
        }
        Ok(())
    }

    /// Load a value from the specified memory area.
    fn load<A, Ao>(&mut self, addr: A, layout: &Layout) -> io::Result<()>
    where
        A: Copy + std::ops::Add<i32, Output = Ao> + Into<EncodedAddress>,
        Ao: Copy + std::ops::Add<i32, Output = Ao> + Into<EncodedAddress>,
    {
        if let Ok(assigns) = RegAssign::build(
            layout,
            &mut LAST_VALUE_GPS.as_slice(),
            &mut LAST_VALUE_FPS.as_slice(),
        ) {
            for assign in assigns.into_iter() {
                self.load_eightbyte(assign.reg, addr + assign.offset as i32, assign.size)?;
            }
        } else if layout.class == Class::Memory {
            for e in layout.eightbytes().rev() {
                self.load_eightbyte(TMP_GP2, addr + e.offset as i32, e.size)?;
                self.push_eightbyte(TMP_GP2)?;
            }
        }
        Ok(())
    }

    /// Store the last value in the specified memory area.
    fn store<A, Ao>(&mut self, addr: A, layout: &Layout) -> io::Result<()>
    where
        A: Copy + std::ops::Add<i32, Output = Ao> + Into<EncodedAddress>,
        Ao: Copy + std::ops::Add<i32, Output = Ao> + Into<EncodedAddress>,
    {
        if let Ok(assigns) = RegAssign::build(
            layout,
            &mut LAST_VALUE_GPS.as_slice(),
            &mut LAST_VALUE_FPS.as_slice(),
        ) {
            for assign in assigns.into_iter() {
                self.store_eightbyte(addr + assign.offset as i32, assign.reg, assign.size)?;
            }
        } else if layout.class == Class::Memory {
            for e in layout.eightbytes() {
                self.pop_eightbyte(TMP_GP2)?;
                self.store_eightbyte(addr + e.offset as i32, TMP_GP2, e.size)?;
            }
        }
        Ok(())
    }
}

pub trait EightbyteReg {
    /// Push a value of this register onto the stack.
    fn push(&self, w: &mut Writer) -> io::Result<usize>;

    /// Pop a value from the stack into this register.
    fn pop(&self, w: &mut Writer) -> io::Result<usize>;

    /// Load a value from `src_addr` into this register.
    fn load(&self, w: &mut Writer, src_addr: impl LoadStoreAddress, size: usize) -> io::Result<()>;

    /// Store a value of this register to `dst_addr`.
    fn store(&self, w: &mut Writer, dst_addr: impl LoadStoreAddress, size: usize)
        -> io::Result<()>;
}

impl EightbyteReg for Gpr64 {
    fn push(&self, w: &mut Writer) -> io::Result<usize> {
        w.pushq(*self)?;
        Ok(1)
    }

    fn pop(&self, w: &mut Writer) -> io::Result<usize> {
        w.popq(*self)?;
        Ok(1)
    }

    fn load(&self, w: &mut Writer, src_addr: impl LoadStoreAddress, size: usize) -> io::Result<()> {
        match size {
            0 => Ok(()),
            1 => w.movb(self.b(), memory(src_addr)),
            2 => w.movw(self.w(), memory(src_addr)),
            3 => {
                w.movzwq(self.q(), memory(src_addr))?;
                w.movzbq(TMP_GP1, memory(src_addr.offset(2)))?;
                w.shlq(TMP_GP1, 16i8)?;
                w.orq(self.q(), TMP_GP1)
            }
            4 => w.movl(self.l(), memory(src_addr)),
            5 => {
                w.movl(self.l(), memory(src_addr))?;
                w.movzbq(TMP_GP1, memory(src_addr.offset(4)))?;
                w.shlq(TMP_GP1, 32i8)?;
                w.orq(self.q(), TMP_GP1)
            }
            6 => {
                w.movl(self.l(), memory(src_addr))?;
                w.movzwq(TMP_GP1, memory(src_addr.offset(4)))?;
                w.shlq(TMP_GP1, 32i8)?;
                w.orq(self.q(), TMP_GP1)
            }
            7 => {
                w.movl(self.l(), memory(src_addr))?;
                w.movzwq(TMP_GP1, memory(src_addr.offset(4)))?;
                w.shlq(TMP_GP1, 32i8)?;
                w.orq(self.q(), TMP_GP1)?;
                w.movzbq(TMP_GP1, memory(src_addr.offset(6)))?;
                w.shlq(TMP_GP1, 48i8)?;
                w.orq(self.q(), TMP_GP1)
            }
            8 => w.movq(self.q(), memory(src_addr)),
            _ => panic!(),
        }
    }

    fn store(
        &self,
        w: &mut Writer,
        dst_addr: impl LoadStoreAddress,
        size: usize,
    ) -> io::Result<()> {
        match size {
            0 => Ok(()),
            1 => w.movb(memory(dst_addr), self.b()),
            2 => w.movw(memory(dst_addr), self.w()),
            3 => {
                w.movw(memory(dst_addr), self.w())?;
                w.shrl(self.l(), 16i8)?;
                w.movb(memory(dst_addr.offset(2)), self.b())
            }
            4 => w.movl(memory(dst_addr), self.l()),
            5 => {
                w.movl(memory(dst_addr), self.l())?;
                w.shrq(self.q(), 32i8)?;
                w.movb(memory(dst_addr.offset(4)), self.b())
            }
            6 => {
                w.movl(memory(dst_addr), self.l())?;
                w.shrq(self.q(), 32i8)?;
                w.movw(memory(dst_addr.offset(4)), self.w())
            }
            7 => {
                w.movl(memory(dst_addr), self.l())?;
                w.shrq(self.q(), 32i8)?;
                w.movw(memory(dst_addr.offset(4)), self.w())?;
                w.shrq(self.q(), 16i8)?;
                w.movb(memory(dst_addr.offset(6)), self.b())
            }
            8 => w.movq(memory(dst_addr), self.q()),
            _ => panic!(),
        }
    }
}

impl EightbyteReg for Xmm {
    fn push(&self, w: &mut Writer) -> io::Result<usize> {
        w.movq(TMP_GP2, *self)?;
        w.pushq(TMP_GP2)?;
        Ok(1)
    }

    fn pop(&self, w: &mut Writer) -> io::Result<usize> {
        w.popq(TMP_GP2)?;
        w.movq(*self, TMP_GP2)?;
        Ok(1)
    }

    fn load(&self, w: &mut Writer, src_addr: impl LoadStoreAddress, size: usize) -> io::Result<()> {
        TMP_GP2.load(w, src_addr, size)?;
        w.movq(*self, TMP_GP2)
    }

    fn store(
        &self,
        w: &mut Writer,
        dst_addr: impl LoadStoreAddress,
        size: usize,
    ) -> io::Result<()> {
        w.movq(TMP_GP2, *self)?;
        TMP_GP2.store(w, dst_addr, size)
    }
}

impl EightbyteReg for AssignedReg {
    fn push(&self, w: &mut Writer) -> io::Result<usize> {
        match self {
            AssignedReg::Gpr(r) => r.push(w),
            AssignedReg::Xmm(r) => r.push(w),
            AssignedReg::Void => Ok(0),
        }
    }

    fn pop(&self, w: &mut Writer) -> io::Result<usize> {
        match self {
            AssignedReg::Gpr(r) => r.pop(w),
            AssignedReg::Xmm(r) => r.pop(w),
            AssignedReg::Void => Ok(0),
        }
    }

    fn load(&self, w: &mut Writer, src_addr: impl LoadStoreAddress, size: usize) -> io::Result<()> {
        match self {
            AssignedReg::Gpr(r) => r.load(w, src_addr, size),
            AssignedReg::Xmm(r) => r.load(w, src_addr, size),
            AssignedReg::Void => Ok(()),
        }
    }

    fn store(
        &self,
        w: &mut Writer,
        dst_addr: impl LoadStoreAddress,
        size: usize,
    ) -> io::Result<()> {
        match self {
            AssignedReg::Gpr(r) => r.store(w, dst_addr, size),
            AssignedReg::Xmm(r) => r.store(w, dst_addr, size),
            AssignedReg::Void => Ok(()),
        }
    }
}

/// Shorthand for addresses that can be offset by integers.
pub trait LoadStoreAddress: Copy + Into<EncodedAddress> {
    type OffsetAddress: Into<EncodedAddress>;

    fn offset(self, offset: i32) -> Self::OffsetAddress;
}

impl<A, Ao> LoadStoreAddress for A
where
    A: Copy + std::ops::Add<i32, Output = Ao> + Into<EncodedAddress>,
    Ao: Into<EncodedAddress>,
{
    type OffsetAddress = Ao;

    fn offset(self, offset: i32) -> Self::OffsetAddress {
        self + offset
    }
}
