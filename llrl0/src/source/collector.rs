use super::{Loader, Source, SourceSet};
use crate::path::Path;
use crate::preprocess::Preprocessor;
use crate::report::{Phase, Report};
use crate::source_loc::SourceLocationTable;
use std::collections::HashSet;

/// Collects all necessary sources by loading the source and tracing dependencies.
pub fn collect<'a>(
    inputs: impl IntoIterator<Item = &'a Path> + Send,
    loader: &Loader,
    source_location_table: &mut SourceLocationTable,
    preprocessor: &Preprocessor,
    report: &mut Report,
) -> SourceSet {
    report.on(Phase::CollectSource, || {
        let mut collector = Collector {
            ongoing: HashSet::new(),
            result: SourceSet::new(),
            source_location_table,
            preprocessor,
            loader,
        };

        for input in inputs {
            collector.collect(input);
        }

        collector.result
    })
}

struct Collector<'l> {
    ongoing: HashSet<Path>,
    result: SourceSet,
    source_location_table: &'l mut SourceLocationTable,
    preprocessor: &'l Preprocessor,
    loader: &'l Loader,
}

impl<'l> Collector<'l> {
    fn collect(&mut self, path: &Path) {
        // Checks whether the load process is ongoing
        if self.ongoing.contains(path) {
            return;
        }
        self.ongoing.insert(path.clone());

        let mut locator = self.source_location_table.begin_locate(path);
        let mut source = self
            .loader
            .load(path.clone(), &mut locator)
            .unwrap_or_else(|e| Source::from_error(e.path, e.error));
        self.source_location_table.complete_locate(locator);

        source.preprocess(self.preprocessor);
        source.resolve_dependencies();

        for dep in source.dependencies.values() {
            self.collect(dep);
        }
        self.result.insert(source);
    }
}
