use super::{Formatter, LocatedConstruct, ModuleSet, Symbol};
use crate::ast;
use crate::formatting::ContextualDisplay;
use crate::pattern_matching;
use crate::sexp::matcher;
use crate::source_loc::{SourceLocation, SourceLocationTable};
use crate::unification;
use itertools::Itertools;
use std::fmt;

pub trait ErrorContext {
    type Def;
    type Use;
    type ClassUse;
    type InstanceUse;
    type Kind;
    type Type;
    type Constraint;
    type Scheme;
    type Pattern;
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
pub enum SemaErrorContext {}

impl ErrorContext for SemaErrorContext {
    type Def = LocatedConstruct;
    type Use = ast::Construct;
    type ClassUse = ast::NodeId<ast::ClassCon>;
    type InstanceUse = ast::NodeId<ast::InstanceCon>;
    type Kind = ast::Kind;
    type Type = ast::Type;
    type Constraint = ast::Constraint;
    type Scheme = ast::Scheme;
    type Pattern = pattern_matching::Pattern;
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
pub enum TextualErrorContext {}

impl ErrorContext for TextualErrorContext {
    type Def = SourceLocation;
    type Use = Symbol;
    type ClassUse = Symbol;
    type InstanceUse = Symbol;
    type Kind = String;
    type Type = String;
    type Constraint = String;
    type Scheme = String;
    type Pattern = String;
}

/// Error during module building.
#[derive(PartialEq, Debug, Clone)]
pub enum Error<Ctx: ErrorContext = TextualErrorContext> {
    DependentModuleBuildFailed,
    Syntax(matcher::Error),
    MultipleDeclarations(String, Ctx::Def, Ctx::Def),
    ConflictingExports(String, Ctx::Def, Ctx::Def),
    DuplicatedIdentifier(String, Ctx::Def, Ctx::Def),
    WildcardPortNameMustBeWildcard(SourceLocation),
    Unresolved(SourceLocation, String, String),
    CannotDeclareSealedClassInstanceInAnotherModule(SourceLocation),
    MacroExpansionFailed(SourceLocation, String),
    CannotUseMacroDefinedInTheSameModule(SourceLocation),
    ClassMethodTypeSchemeUnspecified(SourceLocation),
    CannotGeneralize(SourceLocation),
    CannotUnifyKind(Ctx::Kind, Ctx::Kind, unification::Error),
    UnsupportedKind(Ctx::Kind),
    CyclicClasses(Vec<Ctx::ClassUse>),
    OverlappingInstances(Vec<Ctx::InstanceUse>),
    NoMatchingInstances(Ctx::Constraint, Vec<Ctx::Constraint>),
    RecursionLimitExceeded(Ctx::Constraint),
    CannotUseReturnInThisContext,
    CannotUnifyType(Ctx::Type, Ctx::Type, unification::Error),
    CannotResolveAmbiguity(Ctx::Type, Vec<Ctx::Constraint>),
    ArityMismatch(Option<u32>, Option<u32>),
    MethodTypeSchemeMismatch(Ctx::Scheme, Ctx::Scheme),
    BuildSatisfactionFailedAtThisScope,
    UselessPattern(Ctx::Pattern),
    NonExhaustivePattern(Vec<Ctx::Pattern>),
    On(Ctx::Use, Box<Error<Ctx>>),
    RequiredFor(Ctx::Constraint, Box<Error<Ctx>>),
}

impl<Ctx: ErrorContext> Error<Ctx> {
    pub fn drop_context_info(self) -> Self {
        match self {
            Self::On(_, e) => e.drop_context_info(),
            Self::RequiredFor(_, e) => e.drop_context_info(),
            e => e,
        }
    }

    pub fn multiple_declarations(name: impl Into<String>, a: Ctx::Def, b: Ctx::Def) -> Self {
        Self::MultipleDeclarations(name.into(), a, b)
    }

    pub fn conflicting_exports(name: impl Into<String>, a: Ctx::Def, b: Ctx::Def) -> Self {
        Self::ConflictingExports(name.into(), a, b)
    }

    pub fn duplicated_identifier(name: impl Into<String>, a: Ctx::Def, b: Ctx::Def) -> Self {
        Self::DuplicatedIdentifier(name.into(), a, b)
    }

    pub fn unresolved(
        loc: SourceLocation,
        kind: impl Into<String>,
        name: impl Into<String>,
    ) -> Self {
        Self::Unresolved(loc, kind.into(), name.into())
    }

    pub fn on(self, use_: impl Into<Ctx::Use>) -> Self {
        Self::On(use_.into(), Box::new(self))
    }

    pub fn required_for(self, c: Ctx::Constraint) -> Self {
        Self::RequiredFor(c, Box::new(self))
    }
}

impl Error<SemaErrorContext> {
    pub fn into_result_error<M: ModuleSet>(self, formatter: Formatter<M>) -> Error {
        PassToResult { formatter }.error(self)
    }
}

impl<Ctx: ErrorContext> From<matcher::Error> for Error<Ctx> {
    fn from(e: matcher::Error) -> Self {
        Self::Syntax(e)
    }
}

impl ContextualDisplay<SourceLocationTable> for Error {
    fn fmt_with(&self, ctx: &SourceLocationTable, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::DependentModuleBuildFailed => write!(f, "Dependent module build failed"),
            Self::Syntax(e) => write!(f, "{}", e.fmt_on(ctx)),
            Self::MultipleDeclarations(name, a, b) => {
                let (path, a) = ctx.review(*a);
                let (_, b) = ctx.review(*b);
                write!(
                    f,
                    "{}:{}: Multiple declarations: {} is first declared at {}",
                    path, b, name, a
                )
            }
            Self::ConflictingExports(name, a, b) => {
                let (path, a) = ctx.review(*a);
                let (_, b) = ctx.review(*b);
                write!(
                    f,
                    "{}:{}: Conflicting exports: {} is first exported at {}",
                    path, b, name, a
                )
            }
            Self::DuplicatedIdentifier(name, a, b) => {
                let (path, a) = ctx.review(*a);
                let (_, b) = ctx.review(*b);
                write!(
                    f,
                    "{}:{}: Duplicated identifier: {} is first defined at {}",
                    path, b, name, a
                )
            }
            Self::WildcardPortNameMustBeWildcard(loc) => {
                write!(
                    f,
                    "{}: Wildcard port name must be wildcard",
                    loc.fmt_on(ctx)
                )
            }
            Self::Unresolved(loc, kind, name) => {
                write!(f, "{}: Unresolved {} {}", loc.fmt_on(ctx), kind, name)
            }
            Self::CannotDeclareSealedClassInstanceInAnotherModule(loc) => {
                write!(
                    f,
                    "{}: Cannot declare sealed-class instance in another module",
                    loc.fmt_on(ctx)
                )
            }
            Self::MacroExpansionFailed(loc, e) => {
                write!(f, "{}: Macro expansion failed: {}", loc.fmt_on(ctx), e)
            }
            Self::CannotUseMacroDefinedInTheSameModule(loc) => {
                write!(
                    f,
                    "{}: Cannot use macro defined in the same module",
                    loc.fmt_on(ctx)
                )
            }
            Self::ClassMethodTypeSchemeUnspecified(loc) => {
                write!(
                    f,
                    "{}: Class method type scheme unspecified",
                    loc.fmt_on(ctx)
                )
            }
            Self::CannotGeneralize(loc) => {
                write!(f, "{}: Cannot generalize this declaration", loc.fmt_on(ctx))
            }
            Self::CannotUnifyKind(a, b, u) => {
                write!(f, "Cannot unify kind {} with {}: {}", a, b, u)
            }
            Self::UnsupportedKind(k) => {
                write!(
                    f,
                    "Unsupported kind `{}`: Only forms consisting of `*` and `->` are supported",
                    k
                )
            }
            Self::CyclicClasses(classes) => {
                write!(f, "Cyclic classes detected: ")?;
                for (i, c) in classes.iter().enumerate() {
                    if i == 0 {
                        write!(f, "{} (defined at {})", c.name, c.loc.fmt_on(ctx))?;
                    } else {
                        write!(f, ", {} (defined at {})", c.name, c.loc.fmt_on(ctx))?;
                    }
                }
                Ok(())
            }
            Self::OverlappingInstances(insts) => {
                write!(f, "Overlapping instances detected: ")?;
                for (i, c) in insts.iter().enumerate() {
                    if i == 0 {
                        write!(f, "{} (defined at {})", c.name, c.loc.fmt_on(ctx))?;
                    } else {
                        write!(f, ", {} (defined at {})", c.name, c.loc.fmt_on(ctx))?;
                    }
                }
                Ok(())
            }
            Self::NoMatchingInstances(c, cs) => {
                write!(f, "No matching instances: Cannot satisfy {}", c)?;
                if !cs.is_empty() {
                    write!(f, " (where ")?;
                    for (i, c) in cs.iter().enumerate() {
                        if i == 0 {
                            write!(f, "{}", c)?;
                        } else {
                            write!(f, ", {}", c)?;
                        }
                    }
                    write!(f, ")")?;
                }
                Ok(())
            }
            Self::RecursionLimitExceeded(c) => {
                write!(f, "Recursion limit exceeded: Cannot satisfy {}", c)
            }
            Self::CannotUseReturnInThisContext => {
                write!(f, "Cannot use return in this context")
            }
            Self::CannotUnifyType(a, b, u) => {
                write!(f, "Cannot unify type {} with {}: {}", a, b, u)
            }
            Self::CannotResolveAmbiguity(ty, cs) => {
                write!(
                    f,
                    "Cannot resolve ambiguity: No matching defaulting candidate types for {}",
                    ty
                )?;
                if !cs.is_empty() {
                    write!(f, " (where ")?;
                    for (i, c) in cs.iter().enumerate() {
                        if i == 0 {
                            write!(f, "{}", c)?;
                        } else {
                            write!(f, ", {}", c)?;
                        }
                    }
                    write!(f, ")")?;
                }
                Ok(())
            }
            Self::ArityMismatch(a, b) => {
                let a = match a {
                    Some(n) => format!("({})", (0..*n + 1).map(|_| "_").join(" ")),
                    None => "_".to_string(),
                };
                let b = match b {
                    Some(n) => format!("({})", (0..*n + 1).map(|_| "_").join(" ")),
                    None => "_".to_string(),
                };
                write!(f, "Arity mismatch: {} with {}", a, b)
            }
            Self::MethodTypeSchemeMismatch(a, b) => {
                write!(f, "Method type scheme mismatch: {} with {}", a, b)
            }
            Self::BuildSatisfactionFailedAtThisScope => {
                write!(f, "Build satisfaction failed at this scope")
            }
            Self::UselessPattern(pat) => {
                write!(f, "Useless pattern: {}", pat)
            }
            Self::NonExhaustivePattern(pats) => {
                write!(
                    f,
                    "Non-exhaustive pattern: the following patterns are not covered: {}",
                    pats.join(" | ")
                )
            }
            Self::On(unit, e) => {
                write!(f, "{}: {}", unit.loc.fmt_on(ctx), e.fmt_on(ctx))
            }
            Self::RequiredFor(c, e) => {
                write!(f, "{} (required for {})", e.fmt_on(ctx), c)
            }
        }
    }
}

struct PassToResult<M> {
    formatter: Formatter<M>,
}

impl<M: ModuleSet> PassToResult<M> {
    fn error(&self, error: Error<SemaErrorContext>) -> Error {
        match error {
            Error::DependentModuleBuildFailed => Error::DependentModuleBuildFailed,
            Error::Syntax(e) => Error::Syntax(e),
            Error::MultipleDeclarations(name, a, b) => {
                Error::MultipleDeclarations(name, self.def(a), self.def(b))
            }
            Error::ConflictingExports(name, a, b) => {
                Error::ConflictingExports(name, self.def(a), self.def(b))
            }
            Error::DuplicatedIdentifier(name, a, b) => {
                Error::DuplicatedIdentifier(name, self.def(a), self.def(b))
            }
            Error::WildcardPortNameMustBeWildcard(loc) => {
                Error::WildcardPortNameMustBeWildcard(loc)
            }
            Error::Unresolved(loc, kind, name) => Error::Unresolved(loc, kind, name),
            Error::CannotDeclareSealedClassInstanceInAnotherModule(loc) => {
                Error::CannotDeclareSealedClassInstanceInAnotherModule(loc)
            }
            Error::MacroExpansionFailed(loc, e) => Error::MacroExpansionFailed(loc, e),
            Error::CannotUseMacroDefinedInTheSameModule(loc) => {
                Error::CannotUseMacroDefinedInTheSameModule(loc)
            }
            Error::ClassMethodTypeSchemeUnspecified(loc) => {
                Error::ClassMethodTypeSchemeUnspecified(loc)
            }
            Error::CannotGeneralize(loc) => Error::CannotGeneralize(loc),
            Error::CannotUnifyKind(a, b, u) => {
                Error::CannotUnifyKind(self.kind(a), self.kind(b), u)
            }
            Error::UnsupportedKind(k) => Error::UnsupportedKind(self.kind(k)),
            Error::CyclicClasses(classes) => {
                Error::CyclicClasses(classes.into_iter().map(|c| self.class_use(c)).collect())
            }
            Error::OverlappingInstances(insts) => Error::OverlappingInstances(
                insts.into_iter().map(|c| self.instance_use(c)).collect(),
            ),
            Error::NoMatchingInstances(c, cs) => Error::NoMatchingInstances(
                self.constraint(c),
                cs.into_iter().map(|c| self.constraint(c)).collect(),
            ),
            Error::RecursionLimitExceeded(c) => Error::RecursionLimitExceeded(self.constraint(c)),
            Error::CannotUseReturnInThisContext => Error::CannotUseReturnInThisContext,
            Error::CannotUnifyType(a, b, u) => {
                Error::CannotUnifyType(self.type_(a), self.type_(b), u)
            }
            Error::CannotResolveAmbiguity(t, cs) => Error::CannotResolveAmbiguity(
                self.type_(t),
                cs.into_iter().map(|c| self.constraint(c)).collect(),
            ),
            Error::ArityMismatch(a, b) => Error::ArityMismatch(a, b),
            Error::MethodTypeSchemeMismatch(a, b) => {
                Error::MethodTypeSchemeMismatch(self.scheme(a), self.scheme(b))
            }
            Error::BuildSatisfactionFailedAtThisScope => Error::BuildSatisfactionFailedAtThisScope,
            Error::UselessPattern(pat) => Error::UselessPattern(self.pattern(pat)),
            Error::NonExhaustivePattern(pats) => {
                Error::NonExhaustivePattern(pats.into_iter().map(|p| self.pattern(p)).collect())
            }
            Error::On(c, e) => Error::On(self.use_(c), Box::new(self.error(*e))),
            Error::RequiredFor(c, e) => {
                Error::RequiredFor(self.constraint(c), Box::new(self.error(*e)))
            }
        }
    }

    fn def(&self, c: LocatedConstruct) -> SourceLocation {
        c.loc
    }

    fn use_(&self, construct: ast::Construct) -> Symbol {
        self.formatter.symbol_of(construct).cloned().unwrap()
    }

    fn class_use(&self, class_use: ast::NodeId<ast::ClassCon>) -> Symbol {
        self.formatter.symbol_of(class_use).cloned().unwrap()
    }

    fn instance_use(&self, instance_use: ast::NodeId<ast::InstanceCon>) -> Symbol {
        self.formatter.symbol_of(instance_use).cloned().unwrap()
    }

    fn kind(&self, kind: ast::Kind) -> String {
        kind.fmt_on(&self.formatter).to_string()
    }

    fn type_(&self, ty: ast::Type) -> String {
        ty.fmt_on(&self.formatter).to_string()
    }

    fn constraint(&self, constraint: ast::Constraint) -> String {
        constraint.fmt_on(&self.formatter).to_string()
    }

    fn scheme(&self, scheme: ast::Scheme) -> String {
        scheme.fmt_on(&self.formatter).to_string()
    }

    fn pattern(&self, pattern: pattern_matching::Pattern) -> String {
        pattern.fmt_on(&self.formatter).to_string()
    }
}
