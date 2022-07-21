use super::data::{NativeArray, NativeCapturedUse, NativeChar, NativeString, NativeSyntax};
use crate::lowering::ir::*;
use derive_new::new;
use std::collections::HashMap;
use std::mem::{align_of, size_of};
use std::sync::Arc;

/// Resolve size and alignment of data types that may contain cross-references.
#[derive(Debug, Clone)]
pub struct SizeAlignResolver {
    map: HashMap<CtId, Option<SizeAlign>>,
}

impl SizeAlignResolver {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn get(&mut self, ty: &Ct, defs: &HashMap<CtId, Arc<CtDef>>) -> SizeAlign {
        match ty {
            Ct::Id(id) => match self.map.get(id) {
                Some(None) => panic!("Unsized type: {}", id),
                Some(Some(size)) => *size,
                None => {
                    self.map.insert(*id, None);
                    let sas = match defs.get(id).map(|def| &**def) {
                        Some(CtDef::Struct(ty)) => SizeAlign::tuple(self.get_all(&ty.fields, defs)),
                        Some(CtDef::Union(ty)) => SizeAlign::union(self.get_all(&ty.tys, defs)),
                        Some(_) => panic!("Not a type: {}", id),
                        None => panic!("Unknown type: {}", id),
                    };
                    self.map.insert(*id, Some(sas));
                    sas
                }
            },
            Ct::GenericInst(_) => panic!("Found Ct::GenericInst on SizeAlignResolver"),
            Ct::TableGet(_) => panic!("Found Ct::GenericInst on SizeAlignResolver"),
            Ct::Ptr(_) => SizeAlign::pointer(),
            Ct::Clos(_) => {
                let ptr = SizeAlign::pointer();
                SizeAlign::tuple(vec![ptr, ptr])
            }
            Ct::S(s) | Ct::U(s) => {
                let size = ((*s + 7) / 8).next_power_of_two();
                SizeAlign::new(size, size)
            }
            Ct::F32 => SizeAlign::new(4, 4),
            Ct::F64 => SizeAlign::new(8, 8),
            Ct::String => SizeAlign::of::<NativeString>(),
            Ct::Char => SizeAlign::of::<NativeChar>(),
            Ct::Array(_) => SizeAlign::of::<NativeArray<u8>>(),
            Ct::CapturedUse => SizeAlign::of::<NativeCapturedUse>(),
            Ct::Unit => SizeAlign::new(0, 0),
            Ct::Env => SizeAlign::pointer(),
            Ct::Syntax(_) => SizeAlign::of::<NativeSyntax<u8>>(),
            Ct::Hole => panic!("Found Ct::Hole on SizeAlignResolver"),
        }
    }

    pub fn get_all(&mut self, tys: &[Ct], defs: &HashMap<CtId, Arc<CtDef>>) -> Vec<SizeAlign> {
        tys.iter().map(|ty| self.get(ty, defs)).collect()
    }
}

/// A pair of the data size and alignment.
#[derive(Debug, Clone, Copy, new)]
pub struct SizeAlign {
    pub size: usize,
    pub align: usize,
}

impl SizeAlign {
    pub fn of<T>() -> Self {
        Self::new(size_of::<T>(), align_of::<T>())
    }

    pub fn pointer() -> Self {
        Self::new(std::mem::size_of::<usize>(), std::mem::align_of::<usize>())
    }

    pub fn tuple(tss: Vec<Self>) -> Self {
        let align = tss.iter().map(|a| a.align).max().unwrap_or(0);
        let mut size = 0;
        for ts in tss {
            if ts.align != 0 {
                size += (ts.align - (size % ts.align)) % ts.align;
            }
            size += ts.size;
        }
        if align != 0 {
            size += (align - (size % align)) % align;
        }
        Self::new(size, align)
    }

    pub fn union(tss: Vec<Self>) -> Self {
        let align = tss.iter().map(|a| a.align).max().unwrap_or(0);
        let mut size = tss.iter().map(|a| a.size).max().unwrap_or(0);
        if align != 0 {
            size += (align - (size % align)) % align;
        }
        Self::new(size, align)
    }
}
