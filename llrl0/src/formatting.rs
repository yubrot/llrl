use std::fmt;

/// Utilities to implement `std::fmt::Format` with contextual information.
#[derive(Debug, Clone, Copy)]
pub struct Formatting<'a, T: ?Sized, C: ?Sized> {
    pub rep: &'a T,
    pub ctx: &'a C,
}

impl<'a, T: ContextualDisplay<C> + ?Sized, C: ?Sized> fmt::Display for Formatting<'a, T, C> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.rep.fmt_with(self.ctx, f)
    }
}

pub trait ContextualDisplay<C: ?Sized> {
    fn fmt_on<'a>(&'a self, ctx: &'a C) -> Formatting<'a, Self, C> {
        Formatting { rep: self, ctx }
    }

    fn fmt_with(&self, ctx: &C, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}
