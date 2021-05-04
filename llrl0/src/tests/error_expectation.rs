use crate::prelude::*;
use crate::sexp::matcher as m;
use std::borrow::Cow;
use std::collections::HashSet;

#[derive(Debug)]
pub(super) enum ErrorExpectation {
    Syntax,
    MacroExpansion(String),
    MultipleDeclarations(String),
    ConflictingExports(String),
    DuplicatedIdentifier(String),
    Unresolved(String, String),
    CannotDeclareSealedClassInstanceInAnotherModule,
    CannotUseMacroDefinedInTheSameModule,
    ClassMethodTypeSchemeUnspecified,
    CannotGeneralize,
    UnifyKind(UnificationError),
    UnsupportedKind,
    CyclicClasses(Vec<String>),
    OverlappingInstances(Vec<String>),
    NoMatchingInstances,
    RecursionLimitExceeded,
    CannotUseReturnInThisContext,
    UnifyType(UnificationError),
    CannotResolveAmbiguity,
    ArityMismatch,
    MethodTypeSchemeMismatch,
    UselessPattern(String),
    NonExhaustivePattern(Vec<String>),
}

impl ErrorExpectation {
    pub(super) fn matches(&self, error: &ModuleError) -> bool {
        let error = error.clone().drop_context_info();
        match self {
            Self::Syntax => matches!(error, ModuleError::Syntax(_)),
            Self::MacroExpansion(a) => {
                matches!(error, ModuleError::MacroExpansionFailed(_, ref b) if a == b)
            }
            Self::MultipleDeclarations(a) => {
                matches!(error, ModuleError::MultipleDeclarations(ref b, _, _) if a == b)
            }
            Self::ConflictingExports(a) => {
                matches!(error, ModuleError::ConflictingExports(ref b, _, _) if a == b)
            }
            Self::DuplicatedIdentifier(a) => {
                matches!(error, ModuleError::DuplicatedIdentifier(ref b, _, _) if a == b)
            }
            Self::Unresolved(kind, name) => {
                matches!(error, ModuleError::Unresolved(_, ref k, ref n) if kind == k && name == n)
            }
            Self::CannotDeclareSealedClassInstanceInAnotherModule => {
                matches!(
                    error,
                    ModuleError::CannotDeclareSealedClassInstanceInAnotherModule(_)
                )
            }
            Self::CannotUseMacroDefinedInTheSameModule => {
                matches!(error, ModuleError::CannotUseMacroDefinedInTheSameModule(_))
            }
            Self::ClassMethodTypeSchemeUnspecified => {
                matches!(error, ModuleError::ClassMethodTypeSchemeUnspecified(_))
            }
            Self::CannotGeneralize => {
                matches!(error, ModuleError::CannotGeneralize(_))
            }
            Self::UnifyKind(a) => {
                matches!(error, ModuleError::CannotUnifyKind(_, _, ref b) if a == b)
            }
            Self::UnsupportedKind => {
                matches!(error, ModuleError::UnsupportedKind(_))
            }
            Self::CyclicClasses(names) => {
                matches!(
                    error,
                    ModuleError::CyclicClasses(ref classes)
                        if names.iter().map(|s| s.to_string()).collect::<HashSet<_>>()
                        == classes.iter().map(|c| c.name.to_string()).collect()
                )
            }
            Self::OverlappingInstances(names) => {
                matches!(
                    error,
                    ModuleError::OverlappingInstances(ref instances)
                        if names.iter().map(|s| s.to_string()).collect::<HashSet<_>>()
                        == instances.iter().map(|i| i.name.to_string()).collect()
                )
            }
            Self::NoMatchingInstances => {
                matches!(error, ModuleError::NoMatchingInstances(_, _))
            }
            Self::RecursionLimitExceeded => {
                matches!(error, ModuleError::RecursionLimitExceeded(_))
            }
            Self::CannotUseReturnInThisContext => {
                matches!(error, ModuleError::CannotUseReturnInThisContext)
            }
            Self::UnifyType(a) => {
                matches!(error, ModuleError::CannotUnifyType(_, _, ref b) if a == b)
            }
            Self::CannotResolveAmbiguity => {
                matches!(error, ModuleError::CannotResolveAmbiguity(_, _))
            }
            Self::ArityMismatch => {
                matches!(error, ModuleError::ArityMismatch(_, _))
            }
            Self::MethodTypeSchemeMismatch => {
                matches!(error, ModuleError::MethodTypeSchemeMismatch(_, _))
            }
            Self::UselessPattern(pat) => {
                matches!(error, ModuleError::UselessPattern(ref p) if p == pat)
            }
            Self::NonExhaustivePattern(pats) => {
                matches!(error, ModuleError::NonExhaustivePattern(ref ps) if ps == pats)
            }
        }
    }
}

impl<'a> m::Match<'a> for ErrorExpectation {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        if let Ok("syntax") = s.matches::<m::Symbol>() {
            Ok(Self::Syntax)
        } else if let Ok(("macro-expansion", error)) = s.matches::<(m::Symbol, m::String)>() {
            Ok(Self::MacroExpansion(error.to_string()))
        } else if let Ok("unify-kind") = s.matches::<m::Symbol>() {
            Ok(Self::UnifyKind(UnificationError::Mismatch))
        } else if let Ok(("unify-kind", "occurs-check-failed")) =
            s.matches::<(m::Symbol, m::Symbol)>()
        {
            Ok(Self::UnifyKind(UnificationError::OccursCheckFailed))
        } else if let Ok("unsupported-kind") = s.matches::<m::Symbol>() {
            Ok(Self::UnsupportedKind)
        } else if let Ok(("multiple-declarations", name)) = s.matches::<(m::Symbol, m::Symbol)>() {
            Ok(Self::MultipleDeclarations(name.to_string()))
        } else if let Ok(("conflicting-exports", name)) = s.matches::<(m::Symbol, m::Symbol)>() {
            Ok(Self::ConflictingExports(name.to_string()))
        } else if let Ok(("duplicated-identifier", name)) = s.matches::<(m::Symbol, m::Symbol)>() {
            Ok(Self::DuplicatedIdentifier(name.to_string()))
        } else if let Ok("cannot-use-macro-defined-in-the-same-module") = s.matches::<m::Symbol>() {
            Ok(Self::CannotUseMacroDefinedInTheSameModule)
        } else if let Ok("class-method-type-scheme-unspecified") = s.matches::<m::Symbol>() {
            Ok(Self::ClassMethodTypeSchemeUnspecified)
        } else if let Ok("cannot-generalize") = s.matches::<m::Symbol>() {
            Ok(Self::CannotGeneralize)
        } else if let Ok(("unresolved", def_kind, name)) =
            s.matches::<(m::Symbol, m::Symbol, m::Symbol)>()
        {
            Ok(Self::Unresolved(def_kind.to_string(), name.to_string()))
        } else if let Ok("cannot-declare-sealed-class-instance-in-another-module") =
            s.matches::<m::Symbol>()
        {
            Ok(Self::CannotDeclareSealedClassInstanceInAnotherModule)
        } else if let Ok(("cyclic-classes", names)) = s.matches::<(m::Symbol, m::Rest<m::Symbol>)>()
        {
            Ok(Self::CyclicClasses(
                names.into_iter().map(|s| s.to_string()).collect(),
            ))
        } else if let Ok(("overlapping-instances", names)) =
            s.matches::<(m::Symbol, m::Rest<m::Symbol>)>()
        {
            Ok(Self::OverlappingInstances(
                names.into_iter().map(|s| s.to_string()).collect(),
            ))
        } else if let Ok("no-matching-instances") = s.matches::<m::Symbol>() {
            Ok(Self::NoMatchingInstances)
        } else if let Ok("recursion-limit-exceeded") = s.matches::<m::Symbol>() {
            Ok(Self::RecursionLimitExceeded)
        } else if let Ok("cannot-use-return-in-this-context") = s.matches::<m::Symbol>() {
            Ok(Self::CannotUseReturnInThisContext)
        } else if let Ok("unify-type") = s.matches::<m::Symbol>() {
            Ok(Self::UnifyType(UnificationError::Mismatch))
        } else if let Ok(("unify-type", "occurs-check-failed")) =
            s.matches::<(m::Symbol, m::Symbol)>()
        {
            Ok(Self::UnifyType(UnificationError::OccursCheckFailed))
        } else if let Ok("cannot-resolve-ambiguity") = s.matches::<m::Symbol>() {
            Ok(Self::CannotResolveAmbiguity)
        } else if let Ok("arity-mismatch") = s.matches::<m::Symbol>() {
            Ok(Self::ArityMismatch)
        } else if let Ok("method-type-scheme-mismatch") = s.matches::<m::Symbol>() {
            Ok(Self::MethodTypeSchemeMismatch)
        } else if let Ok(("useless", s)) = s.matches::<(m::Symbol, m::Any)>() {
            Ok(Self::UselessPattern(s.to_string()))
        } else if let Ok(("non-exhaustive", ss)) = s.matches::<(m::Symbol, m::Rest<m::Any>)>() {
            Ok(Self::NonExhaustivePattern(
                ss.into_iter().map(|s| s.to_string()).collect(),
            ))
        } else {
            Err(Self::error(s))
        }
    }

    fn expect() -> Cow<'static, str> {
        "<error-expectation>".into()
    }
}
