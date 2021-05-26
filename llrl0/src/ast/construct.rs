use super::*;
use std::convert::TryFrom;
use std::fmt;

macro_rules! define_construct {
    ($( $name:tt($ty:ty), )*) => {
        /// A reference to the language construct on the AST.
        #[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
        pub enum Construct {
            $( $name(NodeId<$ty>), )*
        }

        impl Construct {
            pub fn module(self) -> ModuleId {
                match self {
                    $( Construct::$name(id) => id.module(), )*
                }
            }
        }

        impl fmt::Display for Construct {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    $( Construct::$name(id) => id.fmt(f), )*
                }
            }
        }

        $(
        impl From<NodeId<$ty>> for Construct {
            fn from(id: NodeId<$ty>) -> Self {
                Self::$name(id)
            }
        }

        impl TryFrom<Construct> for NodeId<$ty> {
            type Error = ();

            fn try_from(construct: Construct) -> Result<Self, Self::Error> {
                match construct {
                    Construct::$name(id) => Ok(id),
                    _ => Err(()),
                }
            }
        }
        )*
    };
}

define_construct! {
    // kind
    KindAnn(Annotation<Kind>),
    KindUse(Use<KindUse>),

    // types
    TypeAnn(Annotation<Type>),
    SchemeAnn(Annotation<Scheme>),
    TypeUse(Use<TypeUse>),
    TypeParameter(TypeParameter),
    Constraint(Constraint),
    ClassConUse(Use<NodeId<ClassCon>>),
    ClassMethodUse(Use<NodeId<ClassMethod>>),
    InstanceConUse(Use<NodeId<InstanceCon>>),

    // expr
    Expr(Expr),
    LocalVar(LocalVar),
    LocalFun(LocalFun),
    ValueUse(Use<Value>),
    Capture(Use<Construct>),

    // pattern
    Pattern(Pattern),
    PatternVar(PatternVar),
    ValueConUse(Use<ValueCon>),

    // decl
    Function(Function),
    CFunction(CFunction),
    BuiltinOp(BuiltinOp),
    Macro(Macro),
    DataTypeCon(DataTypeCon),
    DataValueCon(DataValueCon),
    BuiltinTypeCon(BuiltinTypeCon),
    BuiltinValueCon(BuiltinValueCon),
    ValueConField(ValueConField),
    ClassCon(ClassCon),
    ClassMethod(ClassMethod),
    InstanceCon(InstanceCon),
    InstanceMethod(InstanceMethod),
    Parameter(Parameter),
}

impl<T> From<Use<T>> for Construct
where
    Construct: From<T> + From<NodeId<Use<T>>>,
{
    fn from(use_: Use<T>) -> Self {
        match use_ {
            Use::Unresolved(id) => id.into(),
            Use::Resolved(id, _) => id.into(),
        }
    }
}

impl From<KindUse> for Construct {
    fn from(use_: KindUse) -> Self {
        match use_ {}
    }
}

impl From<TypeUse> for Construct {
    fn from(use_: TypeUse) -> Self {
        match use_ {}
    }
}
