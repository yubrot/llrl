use super::{branch_expander, data_expander, heap2stack, ir::*, normalizer, translator};
use crate::ast;
use crate::module::ModuleSet;
use derive_new::new;
use if_chain::if_chain;
use std::collections::{hash_map, BTreeSet, HashMap};
use std::sync::Arc;

#[derive(Debug, Clone)]
pub struct Context {
    ct_id_gen: CtIdGen,
    rt_id_gen: RtIdGen,
    ct_mapping: HashMap<CtKey, CtId>,
    rt_mapping: HashMap<RtKey, RtId>,
    defs: HashMap<CtId, ProcessingDef>,
    data_expansions: HashMap<CtId, data_expander::DataExpansion>,
    next_generation: Generation,
}

impl Context {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            ct_id_gen: CtIdGen::new(),
            rt_id_gen: RtIdGen::new(),
            ct_mapping: HashMap::new(),
            rt_mapping: HashMap::new(),
            defs: HashMap::new(),
            data_expansions: HashMap::new(),
            next_generation: Generation(0),
        }
    }

    pub fn def(&self, id: CtId) -> Option<&Arc<Def>> {
        self.defs
            .get(&id)
            .filter(|cdef| cdef.is_populated())
            .map(|cdef| &cdef.def)
    }

    pub fn defs(&self) -> impl Iterator<Item = (CtId, &Arc<Def>)> {
        self.defs
            .iter()
            .filter(|(_, cdef)| cdef.is_populated())
            .map(|(id, cdef)| (*id, &cdef.def))
    }

    pub fn populate<T>(
        &mut self,
        src: &T,
        set: &impl ModuleSet,
    ) -> (T::Dest, impl Iterator<Item = (CtId, &Arc<Def>)>)
    where
        T: translator::Translate,
        T::Dest: traverser::Traverse + rewriter::Rewrite,
    {
        let mut dest = translator::translate(src, &mut TranslatorContext::new(self, set));
        normalizer::normalize(&mut dest, self);

        let generation = self.next_generation;
        self.next_generation.0 += 1;

        // Assign generation information to each Def generated in this populate pass.
        let mut generation_defs = GenerationCollector::collect(&dest, self);
        for id in generation_defs.iter() {
            self.defs.get_mut(id).unwrap().generation = Some(generation);
        }

        DataExpansionComputetor::compute(generation, &mut generation_defs, self);

        let mut target = CanonicalizeTarget::new(&generation_defs, &mut dest, &mut self.defs);
        data_expander::expand(&mut target, &self.data_expansions);
        branch_expander::expand(&mut target, &mut MatchExpander::new(&mut self.rt_id_gen));
        heap2stack::run(&mut target);

        // Possible optimizations that are not implemented:
        // * Closure inlining: we can inline closure immediate calls like $<f>{<env>}(..)
        // * More escape analysis to promote heap allocations to stack allocations

        let defs = &self.defs;
        let related_defs =
            generation_defs
                .into_iter()
                .filter_map(move |id| match defs.get(&id).unwrap() {
                    cdef if cdef.is_populated() => Some((id, &cdef.def)),
                    _ => None,
                });
        (dest, related_defs)
    }

    fn bind_ct(&mut self, key: CtKey, build: impl FnOnce(&mut Self) -> Option<Def>) -> CtId {
        match self.ct_mapping.entry(key) {
            hash_map::Entry::Occupied(e) => *e.get(),
            hash_map::Entry::Vacant(e) => {
                let id = self.ct_id_gen.next();
                e.insert(id);
                if let Some(def) = build(self) {
                    let phase = match def {
                        Def::Generic(_, _) => Phase::Generalized,
                        _ => Phase::Instantiated,
                    };
                    self.defs
                        .insert(id, ProcessingDef::new(phase, None, Arc::new(def)));
                }
                id
            }
        }
    }
}

impl normalizer::Env for Context {
    fn instantiate(&mut self, id: CtId, args: Vec<Ct>) -> CtId {
        self.bind_ct(CtKey::Inst(id, args.clone()), move |self_| {
            let def = match self_.defs.get(&id) {
                Some(cdef) => &cdef.def,
                None => panic!("Attempt to instantiate {}: which is not a definition", id),
            };
            match def.as_ref() {
                Def::Generic(params, ct) => {
                    assert_eq!(params.len(), args.len());
                    let mut ct = ct.as_ref().clone();
                    rewriter::replace_ct(&mut ct, params.iter().copied().zip(args).collect());
                    Some(ct)
                }
                _ => panic!(
                    "Attempt to instantiate {}: which is not a generic definition",
                    id
                ),
            }
        })
    }

    fn get_processing_def(&mut self, id: CtId) -> Option<normalizer::ProcessingDef> {
        let cdef = self.defs.get(&id)?;
        let mut def = Arc::clone(&cdef.def);
        let is_normalized = match cdef.phase {
            Phase::Generalized => false,
            Phase::Instantiated => {
                self.defs.get_mut(&id).unwrap().phase = Phase::Normalizing;
                def = Arc::new({
                    let mut def = def.as_ref().clone();
                    normalizer::normalize(&mut def, self);
                    def
                });
                self.defs.get_mut(&id).unwrap().phase = Phase::Normalized;
                self.defs.get_mut(&id).unwrap().def = Arc::clone(&def);
                true
            }
            Phase::Normalizing => false,
            Phase::Normalized => true,
        };
        Some(normalizer::ProcessingDef { is_normalized, def })
    }

    fn alloc_ct(&mut self) -> CtId {
        self.ct_id_gen.next()
    }

    fn define_ct(&mut self, id: CtId, def: Def) {
        let phase = match def {
            Def::Generic(_, _) => Phase::Generalized,
            _ => Phase::Instantiated,
        };
        let def = ProcessingDef::new(phase, None, Arc::new(def));
        if self.defs.insert(id, def).is_some() {
            panic!("Duplicate definition of {}", id);
        }
    }

    fn alloc_rt(&mut self) -> RtId {
        self.rt_id_gen.next()
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash)]
enum CtKey {
    Construct(ast::Construct),
    Inst(CtId, Vec<Ct>),
}

type RtKey = ast::Construct;

#[derive(Debug, Clone, new)]
struct ProcessingDef {
    phase: Phase,
    generation: Option<Generation>,
    def: Arc<Def>,
}

impl ProcessingDef {
    fn is_populated(&self) -> bool {
        self.phase == Phase::Normalized
            && self.generation.is_some()
            && !matches!(*self.def, Def::Data(_))
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
enum Phase {
    Generalized,
    Instantiated,
    Normalizing,
    Normalized,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
struct Generation(usize);

#[derive(Debug, new)]
struct TranslatorContext<'a, 'm, M> {
    context: &'a mut Context,
    module_set: &'m M,
}

impl<'a, 'm, M: ModuleSet> translator::Env<'m> for TranslatorContext<'a, 'm, M> {
    type ModuleSet = M;

    fn module_set(&self) -> &'m Self::ModuleSet {
        self.module_set
    }

    fn alloc_ct(&mut self) -> CtId {
        self.context.ct_id_gen.next()
    }

    fn issue_ct(&mut self, construct: impl Into<ast::Construct>) -> CtId {
        let module_set = self.module_set;
        let construct = construct.into();
        self.context.bind_ct(CtKey::Construct(construct), |ctx| {
            TranslatorContext::new(ctx, module_set).translate_def(construct)
        })
    }

    fn alloc_rt(&mut self) -> RtId {
        self.context.rt_id_gen.next()
    }

    fn issue_rt(&mut self, construct: impl Into<ast::Construct>) -> RtId {
        match self.context.rt_mapping.entry(construct.into()) {
            hash_map::Entry::Occupied(e) => *e.get(),
            hash_map::Entry::Vacant(e) => {
                let id = self.context.rt_id_gen.next();
                e.insert(id);
                id
            }
        }
    }
}

#[derive(Debug, new)]
struct DataExpansionComputetor<'a> {
    ct_id_gen: &'a mut CtIdGen,
    data_expansions: &'a mut HashMap<CtId, data_expander::DataExpansion>,
    #[new(default)]
    defs: HashMap<CtId, Def>,
}

impl<'a> DataExpansionComputetor<'a> {
    fn compute(generation: Generation, generation_defs: &mut BTreeSet<CtId>, ctx: &'a mut Context) {
        let mut env = Self::new(&mut ctx.ct_id_gen, &mut ctx.data_expansions);

        data_expander::compute(
            {
                let defs = &ctx.defs;
                generation_defs
                    .iter()
                    .filter_map(move |id| match *defs.get(id).unwrap().def {
                        Def::Data(ref data) => Some((*id, data)),
                        _ => None,
                    })
            },
            &mut env,
        );

        for (id, def) in env.defs {
            let cdef = ProcessingDef::new(Phase::Normalized, Some(generation), Arc::new(def));
            ctx.defs.insert(id, cdef);
            generation_defs.insert(id);
        }
    }
}

impl<'a> data_expander::Env for DataExpansionComputetor<'a> {
    fn add_def(&mut self, def: Def) -> CtId {
        let id = self.ct_id_gen.next();
        self.defs.insert(id, def);
        id
    }

    fn data_expansions(&mut self) -> &mut HashMap<CtId, data_expander::DataExpansion> {
        self.data_expansions
    }
}

#[derive(Debug, new)]
struct MatchExpander<'a> {
    rt_id_gen: &'a mut RtIdGen,
}

impl<'a> branch_expander::Env for MatchExpander<'a> {
    fn alloc_rt(&mut self) -> RtId {
        self.rt_id_gen.next()
    }
}

#[derive(Debug, new)]
struct GenerationCollector<'a> {
    generation_defs: BTreeSet<CtId>,
    context: &'a Context,
}

impl<'a> GenerationCollector<'a> {
    fn collect(dest: &impl traverser::Traverse, ctx: &'a Context) -> BTreeSet<CtId> {
        let mut collector = Self::new(BTreeSet::new(), ctx);
        let _ = traverser::traverse(dest, &mut collector);
        collector.generation_defs
    }
}

impl traverser::Traverser for GenerationCollector<'_> {
    type Error = ();

    fn after_ct(&mut self, ct: &Ct) -> Result<(), ()> {
        if_chain! {
            if let Ct::Id(id) = ct;
            if let Some(cdef) = self.context.defs.get(id);
            if cdef.generation.is_none() && self.generation_defs.insert(*id);
            then {
                    self.traverse(cdef.def.as_ref())?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, new)]
struct CanonicalizeTarget<'a, D> {
    generation_defs: &'a BTreeSet<CtId>,
    dest: &'a mut D,
    defs: &'a mut HashMap<CtId, ProcessingDef>,
}

impl<'a, D: rewriter::Rewrite> rewriter::Rewrite for CanonicalizeTarget<'a, D> {
    fn rewrite<T: rewriter::Rewriter>(&mut self, rewriter: &mut T) -> Result<(), T::Error> {
        rewriter.rewrite(self.dest)?;
        for id in self.generation_defs {
            let def = self.defs.get_mut(id).unwrap();
            rewriter.rewrite(Arc::make_mut(&mut def.def))?;
        }
        Ok(())
    }
}
