use super::data::{NativeArray, NativeCapturedUse, NativeChar, NativeString, NativeSyntax};
use crate::lowering::ir::*;
use derive_new::new;
use std::borrow::Cow;
use std::collections::HashMap;
use std::mem::{align_of, size_of};
use std::sync::Arc;

/// Information on how data is arranged in memory.
#[derive(PartialEq, Eq, Debug, Clone, new)]
pub struct Layout {
    pub size: usize,
    pub align: usize,
    pub components: Vec<LayoutComponent>,
}

impl Layout {
    pub fn terminal(size: usize, align: usize) -> Self {
        Self::new(size, align, Vec::new())
    }

    pub fn of<T>() -> Self {
        Self::terminal(size_of::<T>(), align_of::<T>())
    }

    pub fn pointer() -> Self {
        Self::terminal(std::mem::size_of::<usize>(), std::mem::align_of::<usize>())
    }

    fn product(components: Vec<(usize, usize, Ct)>) -> Self {
        let align = components.iter().map(|(_, a, _)| *a).max().unwrap_or(0);
        let mut offset = 0;
        let components = components
            .into_iter()
            .map(|(s, a, ty)| {
                if a != 0 {
                    offset += (a - (offset % a)) % a;
                }
                let component = LayoutComponent::new(offset, ty);
                offset += s;
                component
            })
            .collect();
        if align != 0 {
            offset += (align - (offset % align)) % align;
        }
        Self::new(offset, align, components)
    }

    fn sum(tys: Vec<(usize, usize)>) -> Self {
        let align = tys.iter().map(|(_, a)| *a).max().unwrap_or(0);
        let mut size = tys.iter().map(|(s, _)| *s).max().unwrap_or(0);
        if align != 0 {
            size += (align - (size % align)) % align;
        }
        Self::terminal(size, align)
    }
}

#[derive(PartialEq, Eq, Debug, Clone, new)]
pub struct LayoutComponent {
    pub offset: usize,
    pub ty: Ct,
}

/// Resolve layout of data types that may contain cross-references.
#[derive(Debug, Clone)]
pub struct LayoutResolver {
    map: HashMap<CtId, Option<Layout>>,
}

impl LayoutResolver {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn register(&mut self, defs: &HashMap<CtId, Arc<CtDef>>) {
        for (id, def) in defs {
            if matches!(**def, CtDef::Struct(_) | CtDef::Union(_)) {
                self.register_visit(&Ct::Id(*id), defs);
            }
        }
    }

    fn register_visit(&mut self, ty: &Ct, defs: &HashMap<CtId, Arc<CtDef>>) -> (usize, usize) {
        let id = match ty {
            Ct::Id(id) => match self.map.get(id) {
                Some(None) => panic!("Unsized type: {}", id),
                Some(Some(layout)) => return (layout.size, layout.align),
                None => *id,
            },
            _ => {
                let layout = self.get(ty);
                return (layout.size, layout.align);
            }
        };

        self.map.insert(id, None);
        let layout = match defs.get(&id).map(|def| &**def) {
            Some(CtDef::Struct(ty)) => Layout::product(
                ty.fields
                    .iter()
                    .map(|f| {
                        let (size, align) = self.register_visit(f, defs);
                        (size, align, f.clone())
                    })
                    .collect(),
            ),
            Some(CtDef::Union(ty)) => Layout::sum(
                ty.tys
                    .iter()
                    .map(|f| self.register_visit(f, defs))
                    .collect(),
            ),
            Some(_) => panic!("Not a type: {}", id),
            None => panic!("Unknown type: {}", id),
        };
        let result = (layout.size, layout.align);
        self.map.insert(id, Some(layout));
        result
    }

    pub fn get(&self, ty: &Ct) -> Cow<Layout> {
        Cow::Owned(match ty {
            Ct::Id(id) => match self.map.get(id) {
                Some(Some(layout)) => return Cow::Borrowed(layout),
                _ => panic!("Unregistered type: {}", id),
            },
            Ct::GenericInst(_) => panic!("Found Ct::GenericInst on LayoutResolver"),
            Ct::TableGet(_) => panic!("Found Ct::GenericInst on LayoutResolver"),
            Ct::Ptr(_) => Layout::pointer(),
            Ct::Clos(_) => {
                let ptr = Layout::pointer();
                Layout::terminal(ptr.size * 2, ptr.align)
            }
            Ct::S(s) | Ct::U(s) => {
                let size = ((*s + 7) / 8).next_power_of_two();
                Layout::terminal(size, size)
            }
            Ct::F32 => Layout::terminal(4, 4),
            Ct::F64 => Layout::terminal(8, 8),
            Ct::String => Layout::of::<NativeString>(),
            Ct::Char => Layout::of::<NativeChar>(),
            Ct::Array(_) => Layout::of::<NativeArray<u8>>(),
            Ct::CapturedUse => Layout::of::<NativeCapturedUse>(),
            Ct::Unit => Layout::terminal(0, 0),
            Ct::Env => Layout::pointer(),
            Ct::Syntax(_) => Layout::of::<NativeSyntax<u8>>(),
            Ct::Hole => panic!("Found Ct::Hole on LayoutResolver"),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn layout_product() {
        assert_eq!(
            Layout::product(vec![
                (4, 4, Ct::S(32)),
                (1, 1, Ct::U(8)),
                (2, 2, Ct::U(16)),
                (8, 8, Ct::S(64)),
            ]),
            Layout::new(
                16,
                8,
                vec![
                    LayoutComponent::new(0, Ct::S(32)),
                    LayoutComponent::new(4, Ct::U(8)),
                    LayoutComponent::new(6, Ct::U(16)),
                    LayoutComponent::new(8, Ct::S(64)),
                ]
            ),
        );
    }

    #[test]
    fn layout_sum() {
        assert_eq!(
            Layout::sum(vec![(12, 4), (20, 2), (8, 8)]),
            Layout::terminal(24, 8),
        );
    }
}
