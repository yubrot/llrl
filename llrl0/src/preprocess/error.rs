use crate::formatting::ContextualDisplay;
use crate::source_loc::{SourceLocation, SourceLocationTable};
use std::fmt;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone)]
pub enum Error {
    UndefinedMetaVariable(SourceLocation, String),
    ExpansionDisallowed(SourceLocation, &'static str),
    DirectiveSyntax(SourceLocation, &'static str),
    EmptySymbol(SourceLocation),
}

impl ContextualDisplay<SourceLocationTable> for Error {
    fn fmt_with(&self, ctx: &SourceLocationTable, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::UndefinedMetaVariable(loc, name) => {
                write!(f, "{}: Undefined meta variable: {}", loc.fmt_on(ctx), name)
            }
            Error::ExpansionDisallowed(loc, place) => {
                write!(f, "{}: Expansion disallowed at {}", loc.fmt_on(ctx), place)
            }
            Error::DirectiveSyntax(loc, expected) => {
                write!(
                    f,
                    "{}: Directive syntax error: expected {}",
                    loc.fmt_on(ctx),
                    expected
                )
            }
            Error::EmptySymbol(loc) => write!(f, "{}: Empty symbol", loc.fmt_on(ctx)),
        }
    }
}
