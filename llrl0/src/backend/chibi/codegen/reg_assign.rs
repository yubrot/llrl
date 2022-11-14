use crate::backend::native::mem_layout::{Class, Layout};
use derive_new::new;
use xten::asm::*;

/// Correspondence information between a part of data and a register.
#[derive(PartialEq, Eq, Debug, Clone, Copy, new)]
pub struct RegAssign {
    pub offset: usize,
    pub size: usize,
    pub reg: AssignedReg,
}

impl RegAssign {
    #[allow(dead_code)]
    pub fn gpr(offset: usize, size: usize, reg: Gpr64) -> Self {
        Self::new(offset, size, AssignedReg::Gpr(reg))
    }

    #[allow(dead_code)]
    pub fn xmm(offset: usize, size: usize, reg: Xmm) -> Self {
        Self::new(offset, size, AssignedReg::Xmm(reg))
    }

    #[allow(dead_code)]
    pub fn void(offset: usize, size: usize) -> Self {
        Self::new(offset, size, AssignedReg::Void)
    }

    pub fn build(layout: &Layout, gps: &mut &[Gpr64], fps: &mut &[Xmm]) -> Result<Vec<Self>, ()> {
        if !matches!(layout.class, Class::Integer | Class::FloatingPoint) {
            return Err(());
        }

        let mut gps_index = 0;
        let mut fps_index = 0;
        let result = layout
            .eightbytes()
            .map(|eightbyte| {
                let reg = match eightbyte.class {
                    Class::Void => AssignedReg::Void,
                    Class::Integer if gps_index < gps.len() => {
                        let assign = AssignedReg::Gpr(gps[gps_index]);
                        gps_index += 1;
                        assign
                    }
                    Class::FloatingPoint if fps_index < fps.len() => {
                        let assign = AssignedReg::Xmm(fps[fps_index]);
                        fps_index += 1;
                        assign
                    }
                    _ => return Err(()),
                };
                Ok(RegAssign::new(eightbyte.offset, eightbyte.size, reg))
            })
            .collect::<Result<_, _>>()?;
        *gps = &gps[gps_index..];
        *fps = &fps[fps_index..];
        Ok(result)
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum AssignedReg {
    Gpr(Gpr64),
    Xmm(Xmm),
    Void,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn build() {
        static GPS: [Gpr64; 2] = [Rax, Rdx];
        static FPS: [Xmm; 2] = [Xmm0, Xmm1];
        let mut gps = GPS.as_slice();
        let mut fps = FPS.as_slice();

        let layout_a = Layout::product(&[
            Layout::integer(4),
            Layout::floating_point(4),
            Layout::floating_point(4),
        ]);
        assert_eq!(
            RegAssign::build(&layout_a, &mut gps, &mut fps),
            Ok(vec![RegAssign::gpr(0, 8, Rax), RegAssign::xmm(8, 4, Xmm0)])
        );
        assert_eq!(gps, &[Rdx]);
        assert_eq!(fps, &[Xmm1]);

        let layout_b = Layout::new(16, 8, Class::Integer);
        assert_eq!(RegAssign::build(&layout_b, &mut gps, &mut fps), Err(()));
        assert_eq!(gps, &[Rdx]);
        assert_eq!(fps, &[Xmm1]);

        let layout_c = Layout::floating_point(8);
        assert_eq!(
            RegAssign::build(&layout_c, &mut gps, &mut fps),
            Ok(vec![RegAssign::xmm(0, 8, Xmm1)])
        );
        assert_eq!(gps, &[Rdx]);
        assert_eq!(fps, &[]);
    }
}
