//! Expands data types and pattern matching to lower level representations.

use super::{ir::*, rewriter, traverser};
use crate::topological_sort;
use derive_new::new;
use if_chain::if_chain;
use itertools::Itertools;
use std::collections::HashMap;

pub trait Env {
    fn add_def(&mut self, def: CtDef) -> CtId;

    fn data_expansions(&mut self) -> &mut HashMap<CtId, DataExpansion>;
}

pub fn compute<'a>(data_types: impl IntoIterator<Item = (CtId, &'a Data)>, env: &mut impl Env) {
    for expanding_data_group in topological_sort::run(
        data_types
            .into_iter()
            .map(|(id, data)| (id, ExpandingData::new(id, data))),
    ) {
        let should_be_named =
            expanding_data_group.len() >= 2 || expanding_data_group[0].has_recursion();

        for expanding_data in expanding_data_group {
            debug_assert!(!env.data_expansions().contains_key(&expanding_data.id));
            let data_expansion = expanding_data.compute_data_expansion(should_be_named, env);
            env.data_expansions()
                .insert(expanding_data.id, data_expansion);
        }
    }
}

pub fn expand(src: &mut impl rewriter::Rewrite, data_expansions: &HashMap<CtId, DataExpansion>) {
    let _ = rewriter::rewrite(src, &mut DataExpander::new(data_expansions));
}

#[derive(Debug, new)]
struct DataExpander<'e> {
    data_expansions: &'e HashMap<CtId, DataExpansion>,
}

impl<'e> rewriter::Rewriter for DataExpander<'e> {
    type Error = ();

    fn before_ct(&mut self, ct: &mut Ct) -> Result<bool, Self::Error> {
        if_chain! {
            if let Ct::Id(id) = ct;
            if let Some(expansion) = self.data_expansions.get(id);
            then {
                *ct = expansion.to_ct();
                self.before_ct(ct)
            } else {
                Ok(true)
            }
        }
    }

    fn before_rt(&mut self, rt: &mut Rt) -> Result<bool, Self::Error> {
        if_chain! {
            if let Rt::ConstructData(construct) = rt;
            if let (Ct::Id(id), ref mut index, ref mut args) = **construct;
            if let Some(expansion) = self.data_expansions.get(&id);
            let args = std::mem::take(args);
            then {
                *rt = expansion.to_rt(*index, args);
                self.before_rt(rt)
            } else {
                Ok(true)
            }
        }
    }

    fn before_rt_pat(&mut self, pat: &mut RtPat) -> Result<bool, Self::Error> {
        if_chain! {
            if let RtPat::Data(Ct::Id(id), index, args) = pat;
            if let Some(expansion) = self.data_expansions.get(id);
            let args = std::mem::take(args);
            then {
                *pat = expansion.to_pat(*index, args);
                self.before_rt_pat(pat)
            } else {
                Ok(true)
            }
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash)]
pub enum DataExpansion {
    Unit,
    Enum(usize),
    Alias(Ct),
    BoxedStruct(CtId),
    Struct(CtId),
    BoxedTagged(CtId, usize, TaggedDataBody),
    Tagged(CtId, usize, TaggedDataBody),
}

impl DataExpansion {
    pub fn boxed_struct_on(env: &mut impl Env, fields: Vec<Ct>) -> Self {
        Self::BoxedStruct(env.add_def(CtDef::Struct(Struct::new(StructRepr::Standard, fields))))
    }

    pub fn struct_on(env: &mut impl Env, fields: Vec<Ct>) -> Self {
        Self::Struct(env.add_def(CtDef::Struct(Struct::new(StructRepr::Standard, fields))))
    }

    pub fn c_struct_on(env: &mut impl Env, fields: Vec<Ct>) -> Self {
        Self::Struct(env.add_def(CtDef::Struct(Struct::new(StructRepr::C, fields))))
    }

    pub fn boxed_tagged_on(env: &mut impl Env, size: usize, tagged_body: TaggedDataBody) -> Self {
        Self::BoxedTagged(
            env.add_def(CtDef::Struct(Struct::new(
                StructRepr::Standard,
                vec![DataExpansion::enum_ct(size), Ct::ptr(tagged_body.to_ct())],
            ))),
            size,
            tagged_body,
        )
    }

    pub fn tagged_on(env: &mut impl Env, size: usize, tagged_body: TaggedDataBody) -> Self {
        Self::Tagged(
            env.add_def(CtDef::Struct(Struct::new(
                StructRepr::Standard,
                vec![DataExpansion::enum_ct(size), tagged_body.to_ct()],
            ))),
            size,
            tagged_body,
        )
    }

    pub fn to_ct(&self) -> Ct {
        match self {
            // unit
            Self::Unit => Ct::Unit,
            // iN
            Self::Enum(n) => Self::enum_ct(*n),
            // ptr(#id) where #id = struct(..)
            Self::BoxedStruct(id) => Ct::ptr(Ct::Id(*id)),
            // #id where #id = struct(..)
            Self::Struct(id) => Ct::Id(*id),
            // T
            Self::Alias(ty) => ty.clone(),
            // #id where #id = struct(iN, ptr(<tagged>))
            Self::BoxedTagged(id, _n, _tagged) => Ct::Id(*id),
            // #id where #id = struct(iN, <tagged>)
            Self::Tagged(id, _n, _tagged) => Ct::Id(*id),
        }
    }

    pub fn to_rt(&self, index: usize, mut args: Vec<Rt>) -> Rt {
        match self {
            // unit
            Self::Unit => {
                debug_assert_eq!(args.len(), 0);
                Rt::Const(Const::Unit)
            }
            // <index>
            Self::Enum(n) => {
                debug_assert_eq!(args.len(), 0);
                Self::enum_rt(*n, index)
            }
            // alloc(#id(<args>))
            Self::BoxedStruct(id) => {
                debug_assert_eq!(index, 0);
                Rt::alloc(Location::Heap, Rt::construct_struct(Ct::Id(*id), args))
            }
            // #id(<args>)
            Self::Struct(id) => {
                debug_assert_eq!(index, 0);
                Rt::construct_struct(Ct::Id(*id), args)
            }
            // <args[0]>
            Self::Alias(_) => {
                debug_assert_eq!(args.len(), 1);
                args.pop().unwrap()
            }
            // #id(<index>, alloc(<tagged>))
            Self::BoxedTagged(id, n, tagged) => Rt::construct_struct(
                Ct::Id(*id),
                vec![Self::enum_rt(*n, index), tagged.to_rt_boxed(index, args)],
            ),
            // #id(<index>, <tagged>)
            Self::Tagged(id, n, tagged) => Rt::construct_struct(
                Ct::Id(*id),
                vec![Self::enum_rt(*n, index), tagged.to_rt(index, args)],
            ),
        }
    }

    pub fn to_pat(&self, index: usize, mut args: Vec<RtPat>) -> RtPat {
        match self {
            // unit
            Self::Unit => {
                debug_assert_eq!(args.len(), 0);
                RtPat::Const(Const::Unit)
            }
            // <index>
            Self::Enum(n) => {
                debug_assert_eq!(args.len(), 0);
                Self::enum_pat(*n, index)
            }
            // ptr(#id(<args>))
            Self::BoxedStruct(id) => {
                debug_assert_eq!(index, 0);
                RtPat::deref(RtPat::Struct(Ct::Id(*id), args))
            }
            // #id(<args>)
            Self::Struct(id) => {
                debug_assert_eq!(index, 0);
                RtPat::Struct(Ct::Id(*id), args)
            }
            // <args[0]>
            Self::Alias(_) => {
                debug_assert_eq!(args.len(), 1);
                args.pop().unwrap()
            }
            // #id(<index>, ptr(<tagged>))
            Self::BoxedTagged(id, n, tagged) => RtPat::Struct(
                Ct::Id(*id),
                vec![Self::enum_pat(*n, index), tagged.to_pat_boxed(index, args)],
            ),
            // #id(<index>, <tagged>)
            Self::Tagged(id, n, tagged) => RtPat::Struct(
                Ct::Id(*id),
                vec![Self::enum_pat(*n, index), tagged.to_pat(index, args)],
            ),
        }
    }

    fn enum_ct(size: usize) -> Ct {
        match size {
            0 | 1 => Ct::Unit,
            n => Ct::U((n as f64).log2().ceil() as usize),
        }
    }

    fn enum_rt(size: usize, index: usize) -> Rt {
        Rt::Const(Self::enum_const(size, index))
    }

    fn enum_pat(size: usize, index: usize) -> RtPat {
        RtPat::Const(Self::enum_const(size, index))
    }

    fn enum_const(size: usize, index: usize) -> Const {
        match size {
            0 | 1 => Const::Unit,
            _ => Const::Integer(Self::enum_ct(size), false, index as u64),
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash)]
pub enum TaggedDataBody {
    Alias(usize, Ct),
    Struct(usize, CtId),
    Union(CtId, Vec<CtId>),
}

impl TaggedDataBody {
    pub fn struct_on(env: &mut impl Env, i: usize, fields: Vec<Ct>) -> Self {
        Self::Struct(
            i,
            env.add_def(CtDef::Struct(Struct::new(StructRepr::Standard, fields))),
        )
    }

    pub fn union_on(env: &mut impl Env, cons: impl IntoIterator<Item = Vec<Ct>>) -> Self {
        let cons = cons
            .into_iter()
            .map(|fields| env.add_def(CtDef::Struct(Struct::new(StructRepr::Standard, fields))))
            .collect::<Vec<_>>();

        TaggedDataBody::Union(
            env.add_def(CtDef::Union(Union::new(
                cons.iter().map(|id| Ct::Id(*id)).collect(),
            ))),
            cons,
        )
    }

    pub fn to_ct(&self) -> Ct {
        match self {
            // T
            Self::Alias(_, ty) => ty.clone(),
            // #id where #id = Struct(..)
            Self::Struct(_, id) => Ct::Id(*id),
            // #id where #id = Union(..)
            Self::Union(id, _) => Ct::Id(*id),
        }
    }

    pub fn to_rt(&self, index: usize, mut args: Vec<Rt>) -> Rt {
        match self {
            // <args[0]>
            Self::Alias(i, _) if index == *i => {
                debug_assert_eq!(args.len(), 1);
                args.pop().unwrap()
            }
            // uninitialized
            Self::Alias(_, ty) => {
                debug_assert_eq!(args.len(), 0);
                Rt::nullary(Nullary::Uninitialized(ty.clone()))
            }
            // #id(<args>)
            Self::Struct(i, id) if index == *i => Rt::construct_struct(Ct::Id(*id), args),
            // uninitialized
            Self::Struct(_, id) => {
                debug_assert_eq!(args.len(), 0);
                Rt::nullary(Nullary::Uninitialized(Ct::Id(*id)))
            }
            // reinterpret[#con -> #id](#con(<args>))
            Self::Union(id, structs) => Rt::unary(
                Unary::Reinterpret(Ct::Id(structs[index]), Ct::Id(*id)),
                Rt::construct_struct(Ct::Id(structs[index]), args),
            ),
        }
    }

    pub fn to_rt_boxed(&self, index: usize, args: Vec<Rt>) -> Rt {
        match self {
            Self::Alias(i, ty) if index != *i => {
                Rt::nullary(Nullary::Uninitialized(Ct::ptr(ty.clone())))
            }
            Self::Struct(i, id) if index != *i => {
                Rt::nullary(Nullary::Uninitialized(Ct::ptr(Ct::Id(*id))))
            }
            _ => Rt::alloc(Location::Heap, self.to_rt(index, args)),
        }
    }

    pub fn to_pat(&self, index: usize, mut args: Vec<RtPat>) -> RtPat {
        match self {
            // <args[0]>
            Self::Alias(i, _) if index == *i => {
                debug_assert_eq!(args.len(), 1);
                args.pop().unwrap()
            }
            // uninitialized
            Self::Alias(_, _) => {
                debug_assert_eq!(args.len(), 0);
                RtPat::Wildcard
            }
            // #id(<args>)
            Self::Struct(i, id) if index == *i => RtPat::Struct(Ct::Id(*id), args),
            // uninitialized
            Self::Struct(_, _) => {
                debug_assert_eq!(args.len(), 0);
                RtPat::Wildcard
            }
            // reinterpret[#id -> #con](#con(<args>))
            Self::Union(id, structs) => RtPat::Reinterpret(
                Ct::Id(*id),
                Ct::Id(structs[index]),
                Box::new(RtPat::Struct(Ct::Id(structs[index]), args)),
            ),
        }
    }

    pub fn to_pat_boxed(&self, index: usize, args: Vec<RtPat>) -> RtPat {
        match self {
            Self::Alias(i, _) if index != *i => RtPat::Wildcard,
            Self::Struct(i, _) if index != *i => RtPat::Wildcard,
            _ => RtPat::deref(self.to_pat(index, args)),
        }
    }
}

#[derive(Debug, new)]
struct ExpandingData<'a> {
    id: CtId,
    data: &'a Data,
}

impl<'a> ExpandingData<'a> {
    fn has_recursion(&self) -> bool {
        let mut finder = DataRecursionChecker(self.id);
        traverser::traverse(self.data, &mut finder).is_err()
    }

    fn compute_data_expansion(&self, should_be_named: bool, env: &mut impl Env) -> DataExpansion {
        match self.data.cons.as_slice() {
            // TODO: Should this be an expansion like Never?
            [] => DataExpansion::Unit,
            [fields] => match fields.as_slice() {
                // c-data is not expanded to Unit
                [] if self.data.repr != DataRepr::C => DataExpansion::Unit,
                // A single field value-data can be an alias
                [field] if self.data.repr == DataRepr::Value && !should_be_named => {
                    DataExpansion::Alias(field.clone())
                }
                fields => {
                    let fields = fields.into_iter().cloned().collect();
                    match self.data.repr {
                        DataRepr::Boxed => DataExpansion::boxed_struct_on(env, fields),
                        DataRepr::Value => DataExpansion::struct_on(env, fields),
                        DataRepr::C => DataExpansion::c_struct_on(env, fields),
                    }
                }
            },
            _cons if self.data.repr == DataRepr::C => {
                panic!("c-data does not match C-compatible structure")
            }
            cons => {
                let size = cons.len();
                let tagged_body = match cons.iter().filter(|fields| !fields.is_empty()).count() {
                    0 => return DataExpansion::Enum(size),
                    1 => {
                        let (index, fields) = cons
                            .iter()
                            .find_position(|fields| !fields.is_empty())
                            .unwrap();
                        match fields.as_slice() {
                            [field] => TaggedDataBody::Alias(index, field.clone()),
                            fields => {
                                let fields = fields.into_iter().cloned().collect();
                                TaggedDataBody::struct_on(env, index, fields)
                            }
                        }
                    }
                    _ => {
                        let cons = cons
                            .into_iter()
                            .map(|fields| fields.into_iter().cloned().collect());
                        TaggedDataBody::union_on(env, cons)
                    }
                };
                match self.data.repr {
                    DataRepr::Boxed => DataExpansion::boxed_tagged_on(env, size, tagged_body),
                    _ => DataExpansion::tagged_on(env, size, tagged_body),
                }
            }
        }
    }
}

impl<'a> topological_sort::DependencyList<CtId> for ExpandingData<'a> {
    fn traverse_dependencies(&self, f: &mut impl FnMut(&CtId)) {
        traverser::visit_ct_uses(self.data, f);
    }
}

#[derive(Debug)]
struct DataRecursionChecker(CtId);

impl traverser::Traverser for DataRecursionChecker {
    type Error = ();

    fn after_ct(&mut self, ct: &Ct) -> Result<(), ()> {
        match ct {
            Ct::Id(id) if *id == self.0 => Err(()),
            _ => Ok(()),
        }
    }
}
