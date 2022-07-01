use super::*;
use crate::topological_sort;
use itertools::Itertools;

#[derive(Debug, Clone)]
pub struct Function {
    pub id: NodeId<Function>,
    pub transparent: bool,
    pub params: Option<Vec<Parameter>>,
    pub ann: Option<Annotation<Scheme>>,
    pub body: Expr,
}

impl topological_sort::DependencyList<NodeId<Function>> for Function {
    fn traverse_dependencies(&self, f: &mut impl FnMut(&NodeId<Function>)) {
        self.body.traverse_dependencies(f);
    }
}

#[derive(Debug, Clone)]
pub struct CFunction {
    pub id: NodeId<CFunction>,
    pub ann: Annotation<Type>,
    pub c_name: String,
}

#[derive(Debug, Clone)]
pub struct BuiltinOp {
    pub id: NodeId<BuiltinOp>,
    pub ann: Annotation<Scheme>,
    pub builtin_name: String,
}

#[derive(Debug, Clone)]
pub struct Macro {
    pub id: NodeId<Macro>,
    pub param: Parameter,
    pub body: Expr,
}

impl Macro {
    pub fn src_ty() -> Type {
        build_type!(Ast, ((con {TypeCon::SYNTAX}) (con {TypeCon::SEXP})))
    }

    pub fn dest_ty() -> Type {
        build_type!(Ast, (
            (con {TypeCon::RESULT})
            ((con {TypeCon::SYNTAX}) (con {TypeCon::SEXP}))
            (con {TypeCon::STRING})
        ))
    }
}

#[derive(Debug, Clone)]
pub struct DataTypeCon {
    pub id: NodeId<DataTypeCon>,
    pub repr: DataRepr,
    pub ty_params: Vec<TypeParameter>,
    pub value_cons: Vec<NodeId<DataValueCon>>,
}

impl DataTypeCon {
    pub fn index_of(&self, value_con_id: NodeId<DataValueCon>) -> Option<usize> {
        self.value_cons
            .iter()
            .find_position(|id| **id == value_con_id)
            .map(|(index, _)| index)
    }
}

impl Generic for DataTypeCon {
    fn generic_types(&self) -> &[TypeParameter] {
        self.ty_params.as_slice()
    }

    fn generic_constraints(&self) -> &[Constraint] {
        &[]
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
pub enum DataRepr {
    Default,
    Value,
    C,
}

#[derive(Debug, Clone)]
pub struct DataValueCon {
    pub id: NodeId<DataValueCon>,
    pub fields: Option<Vec<ValueConField>>,
    pub type_con: NodeId<DataTypeCon>,
}

impl DataValueCon {
    pub fn to_scheme(&self, type_con: &DataTypeCon) -> Scheme {
        debug_assert_eq!(self.type_con, type_con.id);
        let ty_params = type_con.ty_params.clone();

        let ret = {
            let params = type_con.ty_params.iter().map(|p| Type::Gen(p.id)).collect();
            build_type!(Ast, ((data {type_con.id}) ...{params}))
        };
        let body = match self.fields {
            Some(ref fields) => {
                let fields = fields.iter().map(|field| field.ty.clone()).collect();
                build_type!(Ast, (-> ...{fields} {ret}))
            }
            None => ret,
        };
        Scheme::new(ty_params, Vec::new(), body)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct DataType<'a> {
    pub con: &'a DataTypeCon,
    pub root: &'a Root,
}

impl<'a> DataType<'a> {
    pub fn value_cons(self) -> impl Iterator<Item = &'a DataValueCon> {
        self.con
            .value_cons
            .iter()
            .map(move |id| &self.root.data_value_cons[id])
    }

    pub fn fields_on_every_value_cons(self) -> impl Iterator<Item = &'a ValueConField> {
        self.value_cons()
            .flat_map(|value_con| value_con.fields.iter())
            .flat_map(|fields| fields.iter())
    }
}

impl<'a> topological_sort::DependencyList<NodeId<DataTypeCon>> for DataType<'a> {
    fn traverse_dependencies(&self, f: &mut impl FnMut(&NodeId<DataTypeCon>)) {
        for field in self.fields_on_every_value_cons() {
            field.ty.traverse_dependencies(f);
        }
    }
}

#[derive(Debug, Clone)]
pub struct BuiltinTypeCon {
    pub id: NodeId<BuiltinTypeCon>,
    pub ty_params: Vec<TypeParameter>,
    pub builtin_name: String,
    pub value_cons: Vec<NodeId<BuiltinValueCon>>,
}

impl Generic for BuiltinTypeCon {
    fn generic_types(&self) -> &[TypeParameter] {
        self.ty_params.as_slice()
    }

    fn generic_constraints(&self) -> &[Constraint] {
        &[]
    }
}

#[derive(Debug, Clone)]
pub struct BuiltinValueCon {
    pub id: NodeId<BuiltinValueCon>,
    pub builtin_name: String,
    pub fields: Option<Vec<ValueConField>>,
    pub type_con: NodeId<BuiltinTypeCon>,
}

impl BuiltinValueCon {
    pub fn to_scheme(&self, type_con: &BuiltinTypeCon) -> Scheme {
        debug_assert_eq!(self.type_con, type_con.id);
        let ty_params = type_con.ty_params.clone();

        let ret = {
            let params = type_con.ty_params.iter().map(|p| Type::Gen(p.id)).collect();
            build_type!(Ast, ((builtin {type_con.id}) ...{params}))
        };
        let body = match self.fields {
            Some(ref fields) => {
                let fields = fields.iter().map(|field| field.ty.clone()).collect();
                build_type!(Ast, (-> ...{fields} {ret}))
            }
            None => ret,
        };
        Scheme::new(ty_params, Vec::new(), body)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BuiltinType<'a> {
    pub con: &'a BuiltinTypeCon,
    pub root: &'a Root,
}

impl<'a> BuiltinType<'a> {
    pub fn value_cons(self) -> impl Iterator<Item = &'a BuiltinValueCon> {
        self.con
            .value_cons
            .iter()
            .map(move |id| &self.root.builtin_value_cons[id])
    }

    pub fn fields_on_every_value_cons(self) -> impl Iterator<Item = &'a ValueConField> {
        self.value_cons()
            .flat_map(|value_con| value_con.fields.iter())
            .flat_map(|fields| fields.iter())
    }
}

#[derive(Debug, Clone)]
pub struct ValueConField {
    pub id: NodeId<ValueConField>,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct ClassCon {
    pub id: NodeId<ClassCon>,
    pub constraint_id: NodeId<Constraint>,
    pub ty_params: Vec<TypeParameter>,
    pub superclasses: Vec<Constraint>,
    pub methods: Vec<NodeId<ClassMethod>>,
    pub is_sealed: bool,
}

impl ClassCon {
    pub fn constraint(&self) -> Constraint {
        Constraint::class(
            self.constraint_id,
            self.id,
            self.ty_params.iter().map(|p| Type::Gen(p.id)).collect(),
        )
    }
}

impl Generic for ClassCon {
    fn generic_types(&self) -> &[TypeParameter] {
        self.ty_params.as_slice()
    }

    fn generic_constraints(&self) -> &[Constraint] {
        &[]
    }
}

impl topological_sort::DependencyList<NodeId<ClassCon>> for ClassCon {
    fn traverse_dependencies(&self, f: &mut impl FnMut(&NodeId<ClassCon>)) {
        for c in self.superclasses.iter() {
            c.traverse_dependencies(f);
        }
    }
}

#[derive(Debug, Clone)]
pub struct ClassMethod {
    pub id: NodeId<ClassMethod>,
    pub ann: Annotation<Scheme>,
    pub params: Option<Vec<Parameter>>,
    pub default_body: Option<Expr>,
    pub class_con: NodeId<ClassCon>,
}

impl ClassMethod {
    pub fn arity(&self) -> Option<u32> {
        self.params.as_ref().map(|params| params.len() as u32)
    }

    pub fn to_external_scheme(&self, class: &ClassCon) -> Scheme {
        debug_assert_eq!(self.class_con, class.id);

        // [method_tp_1, method_tp2, .., class_tp_1, class_tp_2, ..]
        let ty_params = self
            .ann
            .body
            .ty_params
            .iter()
            .chain(class.ty_params.iter())
            .cloned()
            .collect();

        // [method_constraint_1, method_constraint_2, .., class_constraint]
        let s_params = self
            .ann
            .body
            .s_params
            .iter()
            .cloned()
            .chain(std::iter::once(class.constraint()))
            .collect();

        Scheme::new(ty_params, s_params, self.ann.body.body.clone())
    }

    pub fn expand_external_instantiation(
        &self,
        mut instantiation: Instantiation,
    ) -> (Instantiation, Instantiation) {
        let instance_ty_args = instantiation
            .ty_args
            .drain(self.ann.body.ty_params.len()..)
            .collect();
        let instance_s_args = instantiation
            .s_args
            .drain(self.ann.body.s_params.len()..)
            .collect();
        (
            Instantiation::new(instance_ty_args, instance_s_args),
            instantiation,
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Class<'a> {
    pub con: &'a ClassCon,
    pub root: &'a Root,
}

impl<'a> Class<'a> {
    pub fn methods(self) -> impl Iterator<Item = &'a ClassMethod> {
        self.con
            .methods
            .iter()
            .map(move |id| &self.root.class_methods[id])
    }

    pub fn constraints_on_interface(self) -> impl Iterator<Item = &'a Constraint> {
        self.con.superclasses.iter().chain(
            self.methods()
                .flat_map(|method| method.ann.body.s_params.iter()),
        )
    }
}

impl<'a> topological_sort::DependencyList<NodeId<ClassCon>> for Class<'a> {
    fn traverse_dependencies(&self, f: &mut impl FnMut(&NodeId<ClassCon>)) {
        for c in self.constraints_on_interface() {
            c.traverse_dependencies(f);
        }
    }
}

#[derive(Debug, Clone)]
pub struct InstanceCon {
    pub id: NodeId<InstanceCon>,
    pub ty_params: Vec<TypeParameter>,
    pub s_params: Vec<Constraint>,
    pub target: Constraint,
    pub methods: Vec<NodeId<InstanceMethod>>,
}

impl Generic for InstanceCon {
    fn generic_types(&self) -> &[TypeParameter] {
        self.ty_params.as_slice()
    }

    fn generic_constraints(&self) -> &[Constraint] {
        self.s_params.as_slice()
    }
}

#[derive(Debug, Clone)]
pub struct InstanceMethod {
    pub id: NodeId<InstanceMethod>,
    pub transparent: bool,
    pub params: Option<Vec<Parameter>>,
    pub ann: Option<Annotation<Scheme>>,
    pub body: Expr,
    pub class_method: Use<NodeId<ClassMethod>>,
    pub instance_con: NodeId<InstanceCon>,
}

impl InstanceMethod {
    pub fn arity(&self) -> Option<u32> {
        self.params.as_ref().map(|params| params.len() as u32)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Instance<'a> {
    pub con: &'a InstanceCon,
    pub root: &'a Root,
}

impl<'a> Instance<'a> {
    pub fn methods(self) -> impl Iterator<Item = &'a InstanceMethod> {
        self.con
            .methods
            .iter()
            .map(move |id| &self.root.instance_methods[id])
    }

    pub fn find_method(&self, id: NodeId<ClassMethod>) -> Option<&'a InstanceMethod> {
        self.methods()
            .find(|m| *m.class_method.get_resolved() == id)
    }
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub id: NodeId<Parameter>,
}
