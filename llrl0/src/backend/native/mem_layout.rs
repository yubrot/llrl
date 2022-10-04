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
    pub class: Class,
    pub components: Vec<LayoutComponent>,
}

impl Layout {
    pub fn terminal(size: usize, align: usize, class: Class) -> Self {
        Self::new(size, align, class, Vec::new())
    }

    pub fn of<T>(class: Class) -> Self {
        assert!(size_of::<T>() <= 16 || class == Class::Memory);
        Self::terminal(size_of::<T>(), align_of::<T>(), class)
    }

    pub fn pointer() -> Self {
        Self::terminal(
            std::mem::size_of::<usize>(),
            std::mem::align_of::<usize>(),
            Class::Integer,
        )
    }

    pub fn clos() -> Self {
        Self::terminal(
            std::mem::size_of::<usize>() * 2,
            std::mem::align_of::<usize>(),
            Class::Integer,
        )
    }

    pub fn outline(&self) -> Self {
        Self::new(self.size, self.align, self.class, Vec::new())
    }

    fn product(components: Vec<(usize, usize, Class, Ct)>) -> Self {
        let align = components.iter().map(|(_, a, _, _)| *a).max().unwrap_or(0);
        let mut class = components
            .iter()
            .fold(Class::Void, |a, (_, _, b, _)| a.merge(*b));
        let mut offset = 0;
        let components = components
            .into_iter()
            .map(|(s, a, _, ty)| {
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
        let size = offset;
        if size > 16 {
            class = Class::Memory;
        }
        Self::new(size, align, class, components)
    }

    fn sum(tys: Vec<(usize, usize, Class)>) -> Self {
        let align = tys.iter().map(|(_, a, _)| *a).max().unwrap_or(0);
        let mut class = tys
            .iter()
            .fold(Class::Integer, |a, component| a.merge(component.2));
        let mut size = tys.iter().map(|(s, _, _)| *s).max().unwrap_or(0);
        if align != 0 {
            size += (align - (size % align)) % align;
        }
        if size > 16 {
            class = Class::Memory;
        }
        Self::terminal(size, align, class)
    }
}

impl Default for Layout {
    fn default() -> Self {
        Self::terminal(0, 0, Class::Void)
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

    pub fn register(&mut self, defs: &HashMap<CtId, Arc<Def>>) {
        for (id, def) in defs {
            if matches!(**def, Def::Struct(_) | Def::Union(_)) {
                self.register_visit(&Ct::Id(*id), defs);
            }
        }
    }

    fn register_visit(&mut self, ty: &Ct, defs: &HashMap<CtId, Arc<Def>>) -> (usize, usize, Class) {
        let id = match ty {
            Ct::Id(id) => match self.map.get(id) {
                Some(None) => panic!("Unsized type: {}", id),
                Some(Some(layout)) => return (layout.size, layout.align, layout.class),
                None => *id,
            },
            _ => {
                let layout = self.get(ty);
                return (layout.size, layout.align, layout.class);
            }
        };

        self.map.insert(id, None);
        let layout = match defs.get(&id).map(|def| &**def) {
            Some(Def::Struct(ty)) => Layout::product(
                ty.fields
                    .iter()
                    .map(|f| {
                        let (size, align, class) = self.register_visit(f, defs);
                        (size, align, class, f.clone())
                    })
                    .collect(),
            ),
            Some(Def::Union(ty)) => Layout::sum(
                ty.tys
                    .iter()
                    .map(|f| self.register_visit(f, defs))
                    .collect(),
            ),
            Some(_) => panic!("Not a type: {}", id),
            None => panic!("Unknown type: {}", id),
        };
        let result = (layout.size, layout.align, layout.class);
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
            Ct::Clos(_) => Layout::clos(),
            Ct::S(s) | Ct::U(s) => {
                let size = ((*s + 7) / 8).next_power_of_two();
                Layout::terminal(size, size, Class::Integer)
            }
            Ct::F32 => Layout::terminal(4, 4, Class::FloatingPoint),
            Ct::F64 => Layout::terminal(8, 8, Class::FloatingPoint),
            Ct::String => Layout::of::<NativeString>(Class::Integer),
            Ct::Char => Layout::of::<NativeChar>(Class::Integer),
            Ct::Array(_) => Layout::of::<NativeArray<u8>>(Class::Integer),
            Ct::CapturedUse => Layout::of::<NativeCapturedUse>(Class::Integer),
            Ct::Unit => Layout::default(),
            Ct::Env => Layout::pointer(),
            Ct::Syntax(_) => Layout::of::<NativeSyntax<u8>>(Class::Integer),
        })
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub enum Class {
    Void,
    Integer,
    FloatingPoint,
    Memory,
}

impl Class {
    pub fn merge(self, other: Self) -> Self {
        use Class::*;
        match (self, other) {
            (Memory, _) | (_, Memory) => Memory,
            (Integer, _) | (_, Integer) => Integer,
            (FloatingPoint, _) | (_, FloatingPoint) => FloatingPoint,
            (Void, _) => Void,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn class_merge() {
        use Class::*;
        assert_eq!(Void.merge(Void), Void);
        assert_eq!(Void.merge(FloatingPoint), FloatingPoint);
        assert_eq!(FloatingPoint.merge(FloatingPoint), FloatingPoint);
        assert_eq!(Integer.merge(FloatingPoint), Integer);
        assert_eq!(Integer.merge(Memory), Memory);
        assert_eq!(Memory.merge(FloatingPoint), Memory);
    }

    #[test]
    fn layout_product() {
        assert_eq!(
            Layout::product(vec![
                (4, 4, Class::Integer, Ct::S(32)),
                (1, 1, Class::Integer, Ct::U(8)),
                (2, 2, Class::Integer, Ct::U(16)),
                (8, 8, Class::Integer, Ct::S(64)),
            ]),
            Layout::new(
                16,
                8,
                Class::Integer,
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
            Layout::sum(vec![
                (12, 4, Class::Integer),
                (20, 2, Class::Memory),
                (8, 8, Class::Integer),
            ]),
            Layout::terminal(24, 8, Class::Memory),
        );
    }
}
