use super::data::{NativeArray, NativeCapturedUse, NativeChar, NativeString, NativeSyntax};
use crate::lowering::ir::*;
use derive_new::new;
use std::collections::HashMap;
use std::mem::{align_of, size_of};
use std::sync::Arc;

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

/// Information on how data is arranged in memory.
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Layout {
    pub size: usize,
    pub align: usize,
    pub class: Class,
    pub composite: Option<Arc<CompositeLayout>>,
}

impl Layout {
    pub fn new(mut size: usize, align: usize, mut class: Class) -> Self {
        if align != 0 {
            size += (align - (size % align)) % align;
        }
        if size == 0 {
            class = Class::Void;
        } else if size > 16 {
            class = Class::Memory;
        }

        Self {
            size,
            align,
            class,
            composite: None,
        }
    }

    pub fn composite(mut self, composite: CompositeLayout) -> Self {
        assert!(self.composite.replace(Arc::new(composite)).is_none());
        self
    }

    pub fn unit() -> Self {
        Self::new(0, 0, Class::Void)
    }

    pub fn integer(size: usize) -> Self {
        Self::new(size, size, Class::Integer)
    }

    pub fn floating_point(size: usize) -> Self {
        Self::new(size, size, Class::FloatingPoint)
    }

    pub fn memory(size: usize, align: usize) -> Self {
        Self::new(size, align, Class::Memory)
    }

    pub fn pointer() -> Self {
        Self::new(8, 8, Class::Integer)
    }

    pub fn clos() -> Self {
        Self::new(16, 8, Class::Integer)
    }

    pub fn env() -> Self {
        Self::pointer()
    }

    pub fn native<T>(class: Class) -> Self {
        Self::new(size_of::<T>(), align_of::<T>(), class)
    }

    pub fn string() -> Self {
        Self::native::<NativeString>(Class::Integer)
    }

    pub fn char() -> Self {
        Self::native::<NativeChar>(Class::Integer)
    }

    pub fn array() -> Self {
        Self::native::<NativeArray<u8>>(Class::Integer)
    }

    pub fn captured_use() -> Self {
        Self::native::<NativeCapturedUse>(Class::Integer)
    }

    pub fn syntax() -> Self {
        Self::native::<NativeSyntax<u8>>(Class::Integer)
    }

    pub fn product<'a>(ls: impl IntoIterator<Item = &'a Self>) -> Self {
        let mut align = 0;
        let mut class = Class::Void;
        let mut size = 0;
        let mut elems = Vec::new();
        let mut components = Vec::new();
        for (o, l) in Self::iter_with_offset(ls) {
            align = l.align.max(align);
            class = l.class.merge(class);
            size = o + l.size;
            elems.push((o, l.clone()));
            if let Some(c) = l.composite.as_ref() {
                components.extend(c.components.iter().map(|(p, l)| (o + *p, l.clone())));
            } else {
                components.push((o, l.clone()));
            }
        }
        Self::new(size, align, class).composite(CompositeLayout::new(elems, components))
    }

    pub fn sum<'a>(ls: impl IntoIterator<Item = &'a Self>) -> Self {
        let mut align = 0;
        let mut class = Class::Integer;
        let mut size = 0;
        for l in ls {
            align = l.align.max(align);
            class = l.class.merge(class);
            size = l.size.max(size);
        }
        Self::new(size, align, class)
    }

    /// Get the number of eightbytes to be required for this layout.
    pub fn num_eightbytes(&self) -> usize {
        (self.size + 7) / 8
    }

    pub fn eightbytes(&self) -> impl DoubleEndedIterator<Item = Eightbyte> + '_ {
        let cs = self.composite.as_ref().map(|c| c.components.as_slice());
        (0..self.num_eightbytes()).map(move |index| {
            let offset = index * 8;
            let size = (self.size - offset).min(8);
            let class = match cs {
                Some(cs) => cs
                    .iter()
                    .filter(|(o, l)| offset < l.size + *o && *o < offset + 8)
                    .fold(Class::Void, |c, (_, l)| c.merge(l.class)),
                None => self.class,
            };
            Eightbyte::new(offset, size, class)
        })
    }

    pub fn size_in_stack(&self, last_value: bool) -> usize {
        if last_value && self.class != Class::Memory {
            0
        } else {
            self.num_eightbytes() * 8
        }
    }

    /// Align this layout to eightbytes.
    pub fn in_stack(mut self) -> Self {
        self.size = self.size_in_stack(false);
        self
    }

    fn iter_with_offset<'a>(
        ls: impl IntoIterator<Item = &'a Self>,
    ) -> impl Iterator<Item = (usize, &'a Self)> {
        let mut offset = 0;
        ls.into_iter().map(move |l| {
            if l.align != 0 {
                offset += (l.align - (offset % l.align)) % l.align;
            }
            let o = offset;
            offset += l.size;
            (o, l)
        })
    }
}

/// Layout information for composite data types.
#[derive(PartialEq, Eq, Debug, Clone, new)]
pub struct CompositeLayout {
    pub elems: Vec<(usize, Layout)>,
    pub components: Vec<(usize, Layout)>,
}

/// Information for every certain 8 bytes in the layout.
#[derive(PartialEq, Eq, Debug, Clone, new)]
pub struct Eightbyte {
    pub offset: usize,
    pub size: usize,
    pub class: Class,
}

/// Resolve layout of data types that may contain cross-references.
#[derive(Debug, Clone)]
pub struct LayoutResolver {
    layouts: HashMap<CtId, Option<Layout>>,
}

impl LayoutResolver {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            layouts: HashMap::new(),
        }
    }

    pub fn register(&mut self, defs: &HashMap<CtId, Arc<Def>>) {
        for (id, def) in defs {
            if matches!(**def, Def::Struct(_) | Def::Union(_)) {
                self.register_visit(&Ct::Id(*id), defs);
            }
        }
    }

    fn register_visit(&mut self, ty: &Ct, defs: &HashMap<CtId, Arc<Def>>) -> Layout {
        let id = match ty {
            Ct::Id(id) => match self.layouts.get(id) {
                Some(None) => panic!("Unsized type: {}", id),
                Some(Some(layout)) => return layout.clone(),
                None => *id,
            },
            _ => return self.get(ty),
        };

        self.layouts.insert(id, None);
        let layout = match defs.get(&id).map(|def| &**def) {
            Some(Def::Struct(ty)) => {
                let ls = ty
                    .fields
                    .iter()
                    .map(|f| self.register_visit(f, defs))
                    .collect::<Vec<_>>();
                Layout::product(&ls)
            }
            Some(Def::Union(ty)) => {
                let ls = ty
                    .tys
                    .iter()
                    .map(|f| self.register_visit(f, defs))
                    .collect::<Vec<_>>();
                Layout::sum(&ls)
            }
            Some(_) => panic!("Not a type: {}", id),
            None => panic!("Unknown type: {}", id),
        };
        self.layouts.insert(id, Some(layout.clone()));
        layout
    }

    pub fn get(&self, ty: &Ct) -> Layout {
        match ty {
            Ct::Id(id) => match self.layouts.get(id) {
                Some(Some(layout)) => layout.clone(),
                _ => panic!("Unregistered type: {}", id),
            },
            Ct::GenericInst(_) => panic!("Found Ct::GenericInst on LayoutResolver"),
            Ct::TableGet(_) => panic!("Found Ct::GenericInst on LayoutResolver"),
            Ct::Ptr(_) => Layout::pointer(),
            Ct::Clos(_) => Layout::clos(),
            Ct::S(s) | Ct::U(s) => Layout::integer(((*s + 7) / 8).next_power_of_two()),
            Ct::F32 => Layout::floating_point(4),
            Ct::F64 => Layout::floating_point(8),
            Ct::String => Layout::string(),
            Ct::Char => Layout::char(),
            Ct::Array(_) => Layout::array(),
            Ct::CapturedUse => Layout::captured_use(),
            Ct::Unit => Layout::unit(),
            Ct::Env => Layout::pointer(),
            Ct::Syntax(_) => Layout::syntax(),
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
    fn layout_num_eightbytes() {
        assert_eq!(Layout::memory(1, 1).num_eightbytes(), 1);
        assert_eq!(Layout::memory(12, 4).num_eightbytes(), 2);
    }

    #[test]
    fn layout_eightbytes() {
        assert_eq!(
            Layout::integer(4).eightbytes().collect::<Vec<_>>(),
            vec![Eightbyte::new(0, 4, Class::Integer)]
        );
        assert_eq!(
            Layout::memory(12, 4).eightbytes().collect::<Vec<_>>(),
            vec![
                Eightbyte::new(0, 8, Class::Memory),
                Eightbyte::new(8, 4, Class::Memory),
            ]
        );
        assert_eq!(
            Layout::product(&[
                Layout::integer(4),
                Layout::product(&[
                    Layout::floating_point(4),
                    Layout::floating_point(4),
                    Layout::floating_point(4),
                ]),
            ])
            .eightbytes()
            .collect::<Vec<_>>(),
            vec![
                Eightbyte::new(0, 8, Class::Integer),
                Eightbyte::new(8, 8, Class::FloatingPoint),
            ]
        );
    }

    #[test]
    fn layout_size_in_stack() {
        assert_eq!(Layout::integer(1).size_in_stack(true), 0);
        assert_eq!(Layout::integer(1).size_in_stack(false), 8);
        assert_eq!(Layout::memory(1, 1).size_in_stack(true), 8);
        assert_eq!(Layout::memory(12, 4).size_in_stack(false), 16);
    }

    #[test]
    fn layout_in_stack() {
        assert_eq!(
            Layout::integer(1).in_stack(),
            Layout::new(8, 1, Class::Integer)
        );
        assert_eq!(
            Layout::memory(12, 4).in_stack(),
            Layout::new(16, 4, Class::Memory)
        );
    }

    #[test]
    fn layout_with_offset_iter() {
        assert_eq!(
            Layout::iter_with_offset(&[
                Layout::memory(4, 4),
                Layout::memory(1, 1),
                Layout::memory(2, 2),
                Layout::memory(8, 8),
            ])
            .collect::<Vec<_>>(),
            vec![
                (0, &Layout::memory(4, 4)),
                (4, &Layout::memory(1, 1)),
                (6, &Layout::memory(2, 2)),
                (8, &Layout::memory(8, 8)),
            ]
        );
    }

    #[test]
    fn layout_product() {
        assert_eq!(
            Layout::product(&[
                Layout::memory(4, 4),
                Layout::memory(1, 1),
                Layout::memory(2, 2),
                Layout::memory(8, 8),
            ]),
            Layout::new(16, 8, Class::Memory).composite(CompositeLayout::new(
                vec![
                    (0, Layout::memory(4, 4)),
                    (4, Layout::memory(1, 1)),
                    (6, Layout::memory(2, 2)),
                    (8, Layout::memory(8, 8)),
                ],
                vec![
                    (0, Layout::memory(4, 4)),
                    (4, Layout::memory(1, 1)),
                    (6, Layout::memory(2, 2)),
                    (8, Layout::memory(8, 8)),
                ],
            )),
        );
    }

    #[test]
    fn layout_sum() {
        assert_eq!(
            Layout::sum(&[
                Layout::new(12, 4, Class::Integer),
                Layout::new(20, 2, Class::Memory),
                Layout::new(8, 8, Class::Integer),
            ]),
            Layout::new(24, 8, Class::Memory),
        );
    }

    #[test]
    fn layout_resolver() {
        let mut id_gen = CtIdGen::new();
        let a = id_gen.next();
        let b = id_gen.next();
        let c = id_gen.next();
        let mut r = LayoutResolver::new();
        r.register(
            &vec![
                (
                    a,
                    Arc::new(Def::Struct(Struct::new(
                        StructRepr::Standard,
                        vec![Ct::Id(b), Ct::Id(b), Ct::Id(b)],
                    ))),
                ),
                (
                    b,
                    Arc::new(Def::Struct(Struct::new(
                        StructRepr::Standard,
                        vec![Ct::S(16), Ct::S(8), Ct::S(16)],
                    ))),
                ),
                (
                    c,
                    Arc::new(Def::Union(Union::new(vec![Ct::Id(a), Ct::Env]))),
                ),
            ]
            .into_iter()
            .collect(),
        );

        let b_composite = CompositeLayout::new(
            vec![
                (0, Layout::integer(2)),
                (2, Layout::integer(1)),
                (4, Layout::integer(2)),
            ],
            vec![
                (0, Layout::integer(2)),
                (2, Layout::integer(1)),
                (4, Layout::integer(2)),
            ],
        );
        let a_composite = CompositeLayout::new(
            vec![
                (
                    0,
                    Layout::new(6, 2, Class::Integer).composite(b_composite.clone()),
                ),
                (
                    6,
                    Layout::new(6, 2, Class::Integer).composite(b_composite.clone()),
                ),
                (
                    12,
                    Layout::new(6, 2, Class::Integer).composite(b_composite.clone()),
                ),
            ],
            vec![
                (0, Layout::integer(2)),
                (2, Layout::integer(1)),
                (4, Layout::integer(2)),
                (6, Layout::integer(2)),
                (8, Layout::integer(1)),
                (10, Layout::integer(2)),
                (12, Layout::integer(2)),
                (14, Layout::integer(1)),
                (16, Layout::integer(2)),
            ],
        );

        assert_eq!(
            r.get(&Ct::Id(a)),
            Layout::new(18, 2, Class::Memory).composite(a_composite)
        );
        assert_eq!(
            r.get(&Ct::Id(b)),
            Layout::new(6, 2, Class::Integer).composite(b_composite)
        );
        assert_eq!(r.get(&Ct::Id(c)), Layout::new(24, 8, Class::Memory));
    }
}
