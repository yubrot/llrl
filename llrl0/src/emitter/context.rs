use super::{
    branch_expander, data_expander, heap2stack, ir::*, normalizer, rewriter, simplifier, traverser,
};
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
    ct_defs: HashMap<CtId, ContextCtDef>,
    data_expansions: HashMap<CtId, data_expander::DataExpansion>,
    next_generation: Generation,
}

impl Context {
    pub fn new() -> Self {
        Self {
            ct_id_gen: CtIdGen::new(),
            rt_id_gen: RtIdGen::new(),
            ct_mapping: HashMap::new(),
            rt_mapping: HashMap::new(),
            ct_defs: HashMap::new(),
            data_expansions: HashMap::new(),
            next_generation: Generation(0),
        }
    }

    pub fn def(&self, id: CtId) -> Option<&Arc<CtDef>> {
        self.ct_defs
            .get(&id)
            .filter(|cdef| cdef.is_populated())
            .map(|cdef| &cdef.def)
    }

    pub fn defs(&self) -> impl Iterator<Item = (CtId, &Arc<CtDef>)> {
        self.ct_defs
            .iter()
            .filter(|(_, cdef)| cdef.is_populated())
            .map(|(id, cdef)| (*id, &cdef.def))
    }

    pub fn populate<T>(
        &mut self,
        src: &T,
        set: &impl ModuleSet,
    ) -> (T::Dest, impl Iterator<Item = (CtId, &Arc<CtDef>)>)
    where
        T: simplifier::Simplify,
        T::Dest: traverser::Traverse + rewriter::Rewrite,
    {
        let mut dest = simplifier::simplify(src, &mut SimplifierContext::new(self, set));
        normalizer::normalize(&mut dest, self);

        let generation = self.next_generation;
        self.next_generation.0 += 1;

        // Assign generation information to each CtDef generated in this populate pass.
        let mut generation_defs = GenerationCollector::collect(&dest, self);
        for id in generation_defs.iter() {
            self.ct_defs.get_mut(id).unwrap().generation = Some(generation);
        }

        DataExpansionComputetor::compute(generation, &mut generation_defs, self);

        let mut target = CanonicalizeTarget::new(&generation_defs, &mut dest, &mut self.ct_defs);
        data_expander::expand(&mut target, &self.data_expansions);
        branch_expander::expand(
            &mut target,
            &mut MatchExpander::new(&mut self.rt_id_gen, &self.data_expansions),
        );
        heap2stack::run(&mut target);

        // Possible optimizations that are not implemented:
        // * Closure inlining: we can inline closure immediate calls like $<f>{<env>}(..)
        // * More escape analysis to promote heap allocations to stack allocations

        let ct_defs = &self.ct_defs;
        let related_defs =
            generation_defs
                .into_iter()
                .filter_map(move |id| match ct_defs.get(&id).unwrap() {
                    cdef if cdef.is_populated() => Some((id, &cdef.def)),
                    _ => None,
                });
        (dest, related_defs)
    }

    fn bind_ct(&mut self, key: CtKey, build: impl FnOnce(&mut Self) -> Option<CtDef>) -> CtId {
        match self.ct_mapping.entry(key) {
            hash_map::Entry::Occupied(e) => *e.get(),
            hash_map::Entry::Vacant(e) => {
                let id = self.ct_id_gen.next();
                e.insert(id);
                if let Some(def) = build(self) {
                    let phase = match def {
                        CtDef::Generic(_, _) => Phase::Generalized,
                        _ => Phase::Instantiated,
                    };
                    self.ct_defs
                        .insert(id, ContextCtDef::new(phase, None, Arc::new(def)));
                }
                id
            }
        }
    }
}

impl normalizer::Env for Context {
    fn instantiate(&mut self, id: CtId, args: Vec<Ct>) -> CtId {
        self.bind_ct(CtKey::Inst(id, args.clone()), move |self_| {
            let def = match self_.ct_defs.get(&id) {
                Some(cdef) => &cdef.def,
                None => panic!("Attempt to instantiate {}: which is not a definition", id),
            };
            match def.as_ref() {
                CtDef::Generic(params, ct) => {
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

    fn get_ct(&mut self, id: CtId) -> Option<normalizer::GetCt> {
        let cdef = self.ct_defs.get(&id)?;
        let mut def = Arc::clone(&cdef.def);
        let is_normalized = match cdef.phase {
            Phase::Generalized => false,
            Phase::Instantiated => {
                self.ct_defs.get_mut(&id).unwrap().phase = Phase::Normalizing;
                def = Arc::new({
                    let mut def = def.as_ref().clone();
                    normalizer::normalize(&mut def, self);
                    def
                });
                self.ct_defs.get_mut(&id).unwrap().phase = Phase::Normalized;
                self.ct_defs.get_mut(&id).unwrap().def = Arc::clone(&def);
                true
            }
            Phase::Normalizing => false,
            Phase::Normalized => true,
        };
        Some(normalizer::GetCt { is_normalized, def })
    }

    fn alloc_ct(&mut self) -> CtId {
        self.ct_id_gen.next()
    }

    fn define_ct(&mut self, id: CtId, def: CtDef) -> CtId {
        let phase = match def {
            CtDef::Generic(_, _) => Phase::Generalized,
            _ => Phase::Instantiated,
        };
        let def = ContextCtDef::new(phase, None, Arc::new(def));
        if self.ct_defs.insert(id, def).is_some() {
            panic!("Duplicate definition of {}", id);
        }
        id
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
struct ContextCtDef {
    phase: Phase,
    generation: Option<Generation>,
    def: Arc<CtDef>,
}

impl ContextCtDef {
    fn is_populated(&self) -> bool {
        self.phase == Phase::Normalized
            && self.generation.is_some()
            && !matches!(*self.def, CtDef::Data(_))
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
struct SimplifierContext<'a, 'm, M> {
    context: &'a mut Context,
    module_set: &'m M,
}

impl<'a, 'm, M: ModuleSet> simplifier::Env<'m> for SimplifierContext<'a, 'm, M> {
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
            SimplifierContext::new(ctx, module_set).simplify_def(construct)
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
    defs: HashMap<CtId, CtDef>,
}

impl<'a> DataExpansionComputetor<'a> {
    fn compute(generation: Generation, generation_defs: &mut BTreeSet<CtId>, ctx: &'a mut Context) {
        let mut env = Self::new(&mut ctx.ct_id_gen, &mut ctx.data_expansions);

        data_expander::compute(
            {
                let ct_defs = &ctx.ct_defs;
                generation_defs
                    .iter()
                    .filter_map(move |id| match *ct_defs.get(id).unwrap().def {
                        CtDef::Data(ref data) => Some((*id, data)),
                        _ => None,
                    })
            },
            &mut env,
        );

        for (id, def) in env.defs {
            let cdef = ContextCtDef::new(Phase::Normalized, Some(generation), Arc::new(def));
            ctx.ct_defs.insert(id, cdef);
            generation_defs.insert(id);
        }
    }
}

impl<'a> data_expander::Env for DataExpansionComputetor<'a> {
    fn add_def(&mut self, def: CtDef) -> CtId {
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
    data_expansions: &'a HashMap<CtId, data_expander::DataExpansion>,
}

impl<'a> branch_expander::Env for MatchExpander<'a> {
    fn alloc_rt(&mut self) -> RtId {
        self.rt_id_gen.next()
    }

    fn data_expansions(&self) -> &HashMap<CtId, data_expander::DataExpansion> {
        self.data_expansions
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
            if let Some(cdef) = self.context.ct_defs.get(id);
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
    ct_defs: &'a mut HashMap<CtId, ContextCtDef>,
}

impl<'a, D: rewriter::Rewrite> rewriter::Rewrite for CanonicalizeTarget<'a, D> {
    fn rewrite<T: rewriter::Rewriter>(&mut self, rewriter: &mut T) -> Result<(), T::Error> {
        rewriter.rewrite(self.dest)?;
        for id in self.generation_defs {
            let cdef = self.ct_defs.get_mut(id).unwrap();
            rewriter.rewrite(Arc::make_mut(&mut cdef.def))?;
        }
        Ok(())
    }
}
