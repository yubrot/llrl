use super::{Annotatable, Constraint, Forall, Name, Parameterizable, Scheme, TypeParameter, Where};
use crate::sexp::{self, matcher as m, MatchSlice as _, Sexp};
use crate::source_loc::SourceLocation;
use derive_new::new;
use either::*;
use std::borrow::Cow;

#[derive(Debug, Clone)]
pub enum Decl<'a> {
    NoImplicitStd,
    Import(Import<'a>),
    Export(Export<'a>),
    Function(Function<'a>),
    CFunction(CFunction<'a>),
    BuiltinOp(BuiltinOp<'a>),
    Macro(Macro<'a>),
    Data(Data<'a>),
    BuiltinType(BuiltinType<'a>),
    Class(Class<'a>),
    Instance(Instance<'a>),
    Begin(&'a [Sexp]),
    TopLevelExpr(&'a Sexp),
}

impl<'a> m::Match<'a> for Decl<'a> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<(m::Symbol, m::Rest<m::Any>)>() {
            Ok(("no-implicit-std", [])) => Ok(Decl::NoImplicitStd),
            Ok(("import", _)) => Ok(Decl::Import(s.matches::<Import>()?)),
            Ok(("export", _)) => Ok(Decl::Export(s.matches::<Export>()?)),
            Ok(("function", _)) => Ok(Decl::Function(s.matches::<Function>()?)),
            Ok(("transparent-function", _)) => Ok(Decl::Function(s.matches::<Function>()?)),
            Ok(("c-function", _)) => Ok(Decl::CFunction(s.matches::<CFunction>()?)),
            Ok(("builtin-op", _)) => Ok(Decl::BuiltinOp(s.matches::<BuiltinOp>()?)),
            Ok(("macro", _)) => Ok(Decl::Macro(s.matches::<Macro>()?)),
            Ok(("data", _)) => Ok(Decl::Data(s.matches::<Data>()?)),
            Ok(("value-data", _)) => Ok(Decl::Data(s.matches::<Data>()?)),
            Ok(("c-data", _)) => Ok(Decl::Data(s.matches::<Data>()?)),
            Ok(("builtin-type", _)) => Ok(Decl::BuiltinType(s.matches::<BuiltinType>()?)),
            Ok(("class", _)) => Ok(Decl::Class(s.matches::<Class>()?)),
            Ok(("sealed-class", _)) => Ok(Decl::Class(s.matches::<Class>()?)),
            Ok(("instance", _)) => Ok(Decl::Instance(s.matches::<Instance>()?)),
            Ok(("begin", decls)) => Ok(Decl::Begin(decls)),
            _ => Ok(Decl::TopLevelExpr(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        "<declaration>".into()
    }
}

impl_default_match_slice!(['a] MatchSlice<'a> for Decl<'a>);

#[derive(Debug, Clone)]
pub struct NoImplicitStd;

impl<'a> m::Match<'a> for NoImplicitStd {
    type Result = ();

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<(m::Symbol,)>() {
            Ok(("no-implicit-std",)) => Ok(()),
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        "(no-implicit-std)".into()
    }
}

#[derive(Debug, Clone, new)]
pub struct Import<'a, Target: m::MatchSlice<'a> = m::Any> {
    pub loc: SourceLocation,
    pub path: &'a str,
    pub targets: Target::SliceResult,
}

impl<'a, Target: m::MatchSlice<'a>> m::Match<'a> for Import<'a, Target> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<(m::Symbol, m::String, m::Rest<m::Any>)>() {
            Ok(("import", path, targets)) => {
                let targets = Target::match_sexp_slice(targets, s.loc)?;
                Ok(Self::new(s.loc, path, targets))
            }
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        "(import \"<path>\" <target> ...)".into()
    }
}

impl_default_match_slice!(['a] MatchSlice<'a> for Import<'a>);

#[derive(Debug, Clone, new)]
pub struct Export<'a, Target: m::MatchSlice<'a> = m::Any> {
    pub loc: SourceLocation,
    pub targets: Target::SliceResult,
}

impl<'a, Target: m::MatchSlice<'a>> m::Match<'a> for Export<'a, Target> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<(m::Symbol, m::Rest<m::Any>)>() {
            Ok(("export", targets)) => {
                let targets = Target::match_sexp_slice(targets, s.loc)?;
                Ok(Self::new(s.loc, targets))
            }
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        "(export <target> ...)".into()
    }
}

impl_default_match_slice!(['a] MatchSlice<'a> for Export<'a>);

#[derive(Debug, Clone, new)]
pub struct PortTarget<'a> {
    pub loc: SourceLocation,
    pub name: Option<Name<'a>>,
    pub target: Name<'a>,
}

impl<'a> m::Match<'a> for PortTarget<'a> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<Either<Name, (Name, Name)>>() {
            Ok(Left(target)) => Ok(Self::new(s.loc, None, target)),
            Ok(Right((name, target))) => Ok(Self::new(s.loc, Some(name), target)),
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        "<<target-name> or (<name> <target-name>)>".into()
    }
}

impl_default_match_slice!(['a] MatchSlice<'a> for PortTarget<'a>);

#[derive(Debug, Clone, new)]
pub struct Function<'a> {
    pub loc: SourceLocation,
    pub transparent: bool,
    pub name: Name<'a>,
    pub params: Option<Vec<Parameter<'a>>>,
    pub scheme: Option<Scheme<'a>>,
    pub body: &'a [Sexp],
}

impl<'a> m::Match<'a> for Function<'a> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        if let Ok((header, decl, body)) = s.matches::<(
            m::Symbol,
            Annotatable<Parameterizable<Name, Parameter>, Scheme>,
            m::Rest<m::Any>,
        )>() {
            let transparent = match header {
                "function" => false,
                "transparent-function" => true,
                _ => Err(Self::error(s))?,
            };
            Ok(Self::new(
                s.loc,
                transparent,
                decl.body.callee,
                decl.body.params,
                decl.annotation,
                body,
            ))
        } else {
            Err(Self::error(s))
        }
    }

    fn expect() -> Cow<'static, str> {
        format!(
            "(<function or transparent-function> <<name> or (<name> <param> ...)> {{{}}}? <body> ...)",
            Scheme::expect_slice()
        )
        .into()
    }
}

impl_default_match_slice!(['a] MatchSlice<'a> for Function<'a>);

#[derive(Debug, Clone, new)]
pub struct CFunction<'a> {
    pub loc: SourceLocation,
    pub name: Name<'a>,
    pub scheme: Scheme<'a>,
    pub c_name: &'a str,
}

impl<'a> m::Match<'a> for CFunction<'a> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<(m::Symbol, (m::Symbol, Name, m::Rest<Scheme>), m::String)>() {
            Ok(("c-function", (sexp::ANNOTATE, name, scheme), c_name)) => {
                Ok(Self::new(s.loc, name, scheme, c_name))
            }
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        format!(
            r#"(c-function <name> {{{}}} "<c-name>")"#,
            Scheme::expect_slice()
        )
        .into()
    }
}

impl_default_match_slice!(['a] MatchSlice<'a> for CFunction<'a>);

#[derive(Debug, Clone, new)]
pub struct BuiltinOp<'a> {
    pub loc: SourceLocation,
    pub name: Name<'a>,
    pub scheme: Scheme<'a>,
    pub builtin_name: &'a str,
}

impl<'a> m::Match<'a> for BuiltinOp<'a> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<(m::Symbol, (m::Symbol, Name, m::Rest<Scheme>), m::String)>() {
            Ok(("builtin-op", (sexp::ANNOTATE, name, scheme), builtin_name)) => {
                Ok(Self::new(s.loc, name, scheme, builtin_name))
            }
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        format!(
            r#"(builtin-op <name> {{{}}} "<builtin-name>")"#,
            Scheme::expect_slice()
        )
        .into()
    }
}

impl_default_match_slice!(['a] MatchSlice<'a> for BuiltinOp<'a>);

#[derive(Debug, Clone, new)]
pub struct Macro<'a> {
    pub loc: SourceLocation,
    pub name: Name<'a>,
    pub param: Parameter<'a>,
    pub body: &'a [Sexp],
}

impl<'a> m::Match<'a> for Macro<'a> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<(m::Symbol, (Name, Parameter), m::Rest<m::Any>)>() {
            Ok(("macro", (name, param), body)) => Ok(Self::new(s.loc, name, param, body)),
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        "(macro (<name> <param>) <body> ...)".into()
    }
}

impl_default_match_slice!(['a] MatchSlice<'a> for Macro<'a>);

#[derive(Debug, Clone, new)]
pub struct Data<'a, VCon: m::MatchSlice<'a> = m::Any> {
    pub loc: SourceLocation,
    pub repr: DataRepr,
    pub name: Name<'a>,
    pub ty_params: Option<Vec<TypeParameter<'a>>>,
    pub value_cons: VCon::SliceResult,
}

impl<'a, VCon: m::MatchSlice<'a>> m::Match<'a> for Data<'a, VCon> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<(
            m::Symbol,
            Parameterizable<Name, TypeParameter>,
            m::Rest<m::Any>,
        )>() {
            Ok((header, decl, value_cons)) => {
                let repr = match header {
                    "data" => DataRepr::Default,
                    "value-data" => DataRepr::Value,
                    "c-data" => DataRepr::C,
                    _ => return Err(Self::error(s)),
                };
                let value_cons = VCon::match_sexp_slice(value_cons, s.loc)?;
                Ok(Self::new(s.loc, repr, decl.callee, decl.params, value_cons))
            }
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        "(<data or value-data or c-data> <<ty-name> or (<ty-name> <ty-param> ...)> <value-con> ...)"
            .into()
    }
}

impl_default_match_slice!(['a] MatchSlice<'a> for Data<'a>);

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
pub enum DataRepr {
    Default,
    Value,
    C,
}

#[derive(Debug, Clone, new)]
pub struct DataValueConstructor<'a> {
    pub loc: SourceLocation,
    pub name: Name<'a>,
    pub fields: Option<&'a [Sexp]>,
}

impl<'a> m::Match<'a> for DataValueConstructor<'a> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<Parameterizable<Name, m::Any>>() {
            Ok(decl) => Ok(Self::new(s.loc, decl.callee, decl.params)),
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        "<<con-name> or (<con-name> <field-ty> ...)>".into()
    }
}

impl_default_match_slice!(['a] MatchSlice<'a> for DataValueConstructor<'a>);

#[derive(Debug, Clone, new)]
pub struct BuiltinType<'a, VCon: m::MatchSlice<'a> = m::Any> {
    pub loc: SourceLocation,
    pub name: Name<'a>,
    pub ty_params: Option<Vec<TypeParameter<'a>>>,
    pub builtin_name: &'a str,
    pub value_cons: VCon::SliceResult,
}

impl<'a, VCon: m::MatchSlice<'a>> m::Match<'a> for BuiltinType<'a, VCon> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<(
            m::Symbol,
            Parameterizable<Name, TypeParameter>,
            m::String,
            m::Rest<m::Any>,
        )>() {
            Ok(("builtin-type", decl, builtin_name, value_cons)) => {
                let value_cons = VCon::match_sexp_slice(value_cons, s.loc)?;
                Ok(Self::new(
                    s.loc,
                    decl.callee,
                    decl.params,
                    builtin_name,
                    value_cons,
                ))
            }
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        r#"(builtin-type <<ty-name> or (<ty-name> <ty-param> ...)> "<builtin-name>" <value-con> ...)"#
        .into()
    }
}

impl_default_match_slice!(['a] MatchSlice<'a> for BuiltinType<'a>);

#[derive(Debug, Clone, new)]
pub struct BuiltinValueConstructor<'a> {
    pub loc: SourceLocation,
    pub name: Name<'a>,
    pub fields: Option<&'a [Sexp]>,
    pub builtin_name: &'a str,
}

impl<'a> m::Match<'a> for BuiltinValueConstructor<'a> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<(Parameterizable<Name, m::Any>, m::String)>() {
            Ok((decl, builtin_name)) => {
                Ok(Self::new(s.loc, decl.callee, decl.params, builtin_name))
            }
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        r#"(<<con-name> or (<con-name> <field-ty> ...)> "<builtin-name>")"#.into()
    }
}

impl_default_match_slice!(['a] MatchSlice<'a> for BuiltinValueConstructor<'a>);

#[derive(Debug, Clone, new)]
pub struct Class<'a, Method: m::MatchSlice<'a> = m::Any> {
    pub loc: SourceLocation,
    pub name: Name<'a>,
    pub ty_params: Option<Vec<TypeParameter<'a>>>,
    pub superclasses: Vec<Constraint<'a>>,
    pub methods: Method::SliceResult,
    pub is_sealed: bool,
}

impl<'a, Method: m::MatchSlice<'a>> m::Match<'a> for Class<'a, Method> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<(
            m::Symbol,
            Parameterizable<Name, TypeParameter>,
            m::Rest<m::Any>,
        )>() {
            Ok((header, decl, ls)) => {
                let is_sealed = match header {
                    "class" => false,
                    "sealed-class" => true,
                    _ => return Err(Self::error(s)),
                };
                let (where_, ls) = Where::match_first(ls)?;
                let methods = Method::match_sexp_slice(ls, s.loc)?;
                Ok(Self::new(
                    s.loc,
                    decl.callee,
                    decl.params,
                    where_.map_or(Vec::new(), |w| w.constraints),
                    methods,
                    is_sealed,
                ))
            }
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        format!(
            "(class <<name> or (<name> <ty-param> ...)> {}? <method> ...)",
            Where::expect()
        )
        .into()
    }
}

impl_default_match_slice!(['a] MatchSlice<'a> for Class<'a>);

#[derive(Debug, Clone, new)]
pub struct Instance<'a, MethodImpl: m::MatchSlice<'a> = m::Any> {
    pub loc: SourceLocation,
    pub name: Name<'a>,
    pub ty_params: Vec<TypeParameter<'a>>,
    pub s_params: Vec<Constraint<'a>>,
    pub target: Constraint<'a>,
    pub method_impls: MethodImpl::SliceResult,
}

impl<'a, MethodImpl: m::MatchSlice<'a>> m::Match<'a> for Instance<'a, MethodImpl> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<(m::Symbol, Name, m::Rest<m::Any>)>() {
            Ok(("instance", name, ls)) => {
                let (forall, ls) = Forall::match_first(ls)?;
                let (target, ls) = Constraint::match_first(ls)?;
                let target = target.ok_or_else(|| Self::error(s))?;
                let (where_, ls) = Where::match_first(ls)?;
                let method_impls = MethodImpl::match_sexp_slice(ls, s.loc)?;
                Ok(Self::new(
                    s.loc,
                    name,
                    forall.map_or(Vec::new(), |f| f.params),
                    where_.map_or(Vec::new(), |w| w.constraints),
                    target,
                    method_impls,
                ))
            }
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        format!(
            "(instance <name> {}? {} {}? <method-impl> ...)",
            Forall::expect(),
            Constraint::expect(),
            Where::expect()
        )
        .into()
    }
}

#[derive(Debug, Clone, new)]
pub struct Parameter<'a> {
    pub loc: SourceLocation,
    pub name: Name<'a>,
}

impl<'a> m::Match<'a> for Parameter<'a> {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        match s.matches::<Name>() {
            Ok(name) => Ok(Self::new(s.loc, name)),
            _ => Err(Self::error(s)),
        }
    }

    fn expect() -> Cow<'static, str> {
        "<param-name>".into()
    }
}

impl_default_match_slice!(['a] MatchSlice<'a> for Parameter<'a>);
