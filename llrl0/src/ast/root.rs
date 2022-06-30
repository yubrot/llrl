use super::*;

#[derive(Debug, Clone)]
pub struct Root {
    pub id: NodeId<Root>,
    pub functions: HashMap<NodeId<Function>, Function>,
    pub c_functions: HashMap<NodeId<CFunction>, CFunction>,
    pub builtin_ops: HashMap<NodeId<BuiltinOp>, BuiltinOp>,
    pub macros: HashMap<NodeId<Macro>, Macro>,
    pub data_type_cons: HashMap<NodeId<DataTypeCon>, DataTypeCon>,
    pub data_value_cons: HashMap<NodeId<DataValueCon>, DataValueCon>,
    pub builtin_type_cons: HashMap<NodeId<BuiltinTypeCon>, BuiltinTypeCon>,
    pub builtin_value_cons: HashMap<NodeId<BuiltinValueCon>, BuiltinValueCon>,
    pub class_cons: HashMap<NodeId<ClassCon>, ClassCon>,
    pub class_methods: HashMap<NodeId<ClassMethod>, ClassMethod>,
    pub instance_cons: HashMap<NodeId<InstanceCon>, InstanceCon>,
    pub instance_methods: HashMap<NodeId<InstanceMethod>, InstanceMethod>,
    pub init_expressions: Vec<InitExpr>,
}

impl Root {
    pub fn new(id: NodeId<Root>) -> Self {
        Self {
            id,
            functions: HashMap::new(),
            c_functions: HashMap::new(),
            builtin_ops: HashMap::new(),
            macros: HashMap::new(),
            data_type_cons: HashMap::new(),
            data_value_cons: HashMap::new(),
            builtin_type_cons: HashMap::new(),
            builtin_value_cons: HashMap::new(),
            class_cons: HashMap::new(),
            class_methods: HashMap::new(),
            instance_cons: HashMap::new(),
            instance_methods: HashMap::new(),
            init_expressions: Vec::new(),
        }
    }

    pub fn verify_relations(&self) -> Result<(), Construct> {
        fn ensure(cond: bool, construct: impl Into<Construct>) -> Result<(), Construct> {
            if cond {
                Ok(())
            } else {
                Err(construct.into())
            }
        }

        for ty in self.data_type_cons.values() {
            for v in ty.value_cons.iter() {
                ensure(self.data_value_cons.contains_key(v), ty.id)?;
                ensure(self.data_value_cons[v].type_con == ty.id, ty.id)?;
            }
        }

        for v in self.data_value_cons.values() {
            ensure(self.data_type_cons.contains_key(&v.type_con), v.id)?;
            ensure(
                self.data_type_cons[&v.type_con].value_cons.contains(&v.id),
                v.id,
            )?;
        }

        for ty in self.builtin_type_cons.values() {
            for v in ty.value_cons.iter() {
                ensure(self.builtin_value_cons.contains_key(v), ty.id)?;
                ensure(self.builtin_value_cons[v].type_con == ty.id, ty.id)?;
            }
        }

        for v in self.builtin_value_cons.values() {
            ensure(self.builtin_type_cons.contains_key(&v.type_con), v.id)?;
            ensure(
                self.builtin_type_cons[&v.type_con]
                    .value_cons
                    .contains(&v.id),
                v.id,
            )?;
        }

        for cls in self.class_cons.values() {
            for m in cls.methods.iter() {
                ensure(self.class_methods.contains_key(m), cls.id)?;
                ensure(self.class_methods[m].class_con == cls.id, cls.id)?;
            }
        }

        for m in self.class_methods.values() {
            ensure(self.class_cons.contains_key(&m.class_con), m.id)?;
            ensure(self.class_cons[&m.class_con].methods.contains(&m.id), m.id)?;
        }

        for inst in self.instance_cons.values() {
            for m in inst.methods.iter() {
                ensure(self.instance_methods.contains_key(m), inst.id)?;
                ensure(self.instance_methods[m].instance_con == inst.id, inst.id)?;
            }
        }

        for m in self.instance_methods.values() {
            ensure(self.instance_cons.contains_key(&m.instance_con), m.id)?;
            ensure(
                self.instance_cons[&m.instance_con].methods.contains(&m.id),
                m.id,
            )?;
        }

        Ok(())
    }

    pub fn data_types(&self) -> impl Iterator<Item = DataType> {
        self.data_type_cons
            .values()
            .map(move |con| DataType { con, root: self })
    }

    pub fn builtin_types(&self) -> impl Iterator<Item = BuiltinType> {
        self.builtin_type_cons
            .values()
            .map(move |con| BuiltinType { con, root: self })
    }

    pub fn classes(&self) -> impl Iterator<Item = Class> {
        self.class_cons
            .values()
            .map(move |con| Class { con, root: self })
    }

    pub fn instances(&self) -> impl Iterator<Item = Instance> {
        self.instance_cons
            .values()
            .map(move |con| Instance { con, root: self })
    }

    pub fn expressions(&self) -> impl Iterator<Item = &Expr> {
        self.functions
            .values()
            .map(|f| &f.body)
            .chain(self.macros.values().map(|m| &m.body))
            .chain(
                self.class_methods
                    .values()
                    .filter_map(|m| m.default_body.as_ref()),
            )
            .chain(self.instance_methods.values().map(|m| &m.body))
            .chain(self.init_expressions.iter().filter_map(|e| e.expr()))
    }

    pub fn constructs(&self) -> impl Iterator<Item = Construct> + '_ {
        (self.functions.keys().copied().map(Construct::from))
            .chain(self.c_functions.keys().copied().map(Construct::from))
            .chain(self.builtin_ops.keys().copied().map(Construct::from))
            .chain(self.macros.keys().copied().map(Construct::from))
            .chain(self.data_type_cons.keys().copied().map(Construct::from))
            .chain(self.builtin_type_cons.keys().copied().map(Construct::from))
            .chain(self.class_cons.keys().copied().map(Construct::from))
            .chain(self.instance_cons.keys().copied().map(Construct::from))
    }

    pub fn get<'a, T: RootConstruct<'a>>(&'a self, id: NodeId<T>) -> Option<T::Dest> {
        T::get(self, id)
    }
}

pub trait RootConstruct<'a> {
    type Dest;

    fn get(root: &'a Root, id: NodeId<Self>) -> Option<Self::Dest>;
}

impl<'a> RootConstruct<'a> for Function {
    type Dest = &'a Function;

    fn get(root: &'a Root, id: NodeId<Self>) -> Option<Self::Dest> {
        root.functions.get(&id)
    }
}

impl<'a> RootConstruct<'a> for CFunction {
    type Dest = &'a CFunction;

    fn get(root: &'a Root, id: NodeId<Self>) -> Option<Self::Dest> {
        root.c_functions.get(&id)
    }
}

impl<'a> RootConstruct<'a> for BuiltinOp {
    type Dest = &'a BuiltinOp;

    fn get(root: &'a Root, id: NodeId<Self>) -> Option<Self::Dest> {
        root.builtin_ops.get(&id)
    }
}

impl<'a> RootConstruct<'a> for Macro {
    type Dest = &'a Macro;

    fn get(root: &'a Root, id: NodeId<Self>) -> Option<Self::Dest> {
        root.macros.get(&id)
    }
}

impl<'a> RootConstruct<'a> for DataTypeCon {
    type Dest = DataType<'a>;

    fn get(root: &'a Root, id: NodeId<Self>) -> Option<Self::Dest> {
        root.data_type_cons
            .get(&id)
            .map(move |con| DataType { con, root })
    }
}

impl<'a> RootConstruct<'a> for DataValueCon {
    type Dest = &'a DataValueCon;

    fn get(root: &'a Root, id: NodeId<Self>) -> Option<Self::Dest> {
        root.data_value_cons.get(&id)
    }
}

impl<'a> RootConstruct<'a> for BuiltinTypeCon {
    type Dest = BuiltinType<'a>;

    fn get(root: &'a Root, id: NodeId<Self>) -> Option<Self::Dest> {
        root.builtin_type_cons
            .get(&id)
            .map(move |con| BuiltinType { con, root })
    }
}

impl<'a> RootConstruct<'a> for BuiltinValueCon {
    type Dest = &'a BuiltinValueCon;

    fn get(root: &'a Root, id: NodeId<Self>) -> Option<Self::Dest> {
        root.builtin_value_cons.get(&id)
    }
}

impl<'a> RootConstruct<'a> for ClassCon {
    type Dest = Class<'a>;

    fn get(root: &'a Root, id: NodeId<Self>) -> Option<Self::Dest> {
        root.class_cons.get(&id).map(move |con| Class { con, root })
    }
}

impl<'a> RootConstruct<'a> for ClassMethod {
    type Dest = &'a ClassMethod;

    fn get(root: &'a Root, id: NodeId<Self>) -> Option<Self::Dest> {
        root.class_methods.get(&id)
    }
}

impl<'a> RootConstruct<'a> for InstanceCon {
    type Dest = Instance<'a>;

    fn get(root: &'a Root, id: NodeId<Self>) -> Option<Self::Dest> {
        root.instance_cons
            .get(&id)
            .map(move |con| Instance { con, root })
    }
}

impl<'a> RootConstruct<'a> for InstanceMethod {
    type Dest = &'a InstanceMethod;

    fn get(root: &'a Root, id: NodeId<Self>) -> Option<Self::Dest> {
        root.instance_methods.get(&id)
    }
}
