use crate::formatting::ContextualDisplay;
use crate::interning::{InternTable, Interned};
use crate::path::Path;
use std::collections::HashMap;
use std::fmt;

mod location;
mod location_range;

pub use location::Location;
pub use location_range::LocationRange;

/// Data that indicates the specific location of a particular file of the llrl source.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
pub struct SourceLocation {
    path: Interned<Path>,
    range: Interned<LocationRange>,
}

impl ContextualDisplay<SourceLocationTable> for SourceLocation {
    fn fmt_with(&self, ctx: &SourceLocationTable, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (path, range) = ctx.review(*self);
        write!(f, "{}:{}", path, range)
    }
}

#[derive(Debug, Clone, Default)]
pub struct SourceLocationTable {
    path_table: InternTable<Path>,
    completed_locators: HashMap<Interned<Path>, SourceLocator>,
}

impl SourceLocationTable {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn begin_locate(&mut self, path: &Path) -> SourceLocator {
        assert!(!self.path_table.is_interned(path));
        let path = self.path_table.intern(path);
        SourceLocator {
            path,
            table: InternTable::new(),
        }
    }

    pub fn complete_locate(&mut self, locator: SourceLocator) {
        self.completed_locators.insert(locator.path, locator);
    }

    pub fn review(&self, source_loc: SourceLocation) -> (Path, LocationRange) {
        let path = self.path_table.review(source_loc.path).clone();
        let range = *self.completed_locators[&source_loc.path]
            .table
            .review(source_loc.range);
        (path, range)
    }
}

#[derive(Debug, Clone)]
pub struct SourceLocator {
    path: Interned<Path>,
    table: InternTable<LocationRange>,
}

impl SourceLocator {
    pub fn temporary() -> Self {
        SourceLocationTable::new().begin_locate(&Path::default())
    }

    pub fn issue(&mut self, range: LocationRange) -> SourceLocation {
        let range = self.table.intern(&range);
        SourceLocation {
            path: self.path,
            range,
        }
    }
}
