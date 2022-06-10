use super::{Loader, Source, SourceSet};
use crate::path::Path;
use crate::report::{Phase, Report};
use crate::source_loc::SourceLocationTable;
use std::collections::HashSet;
use std::sync::Mutex;

pub fn collect<'a>(
    inputs: impl IntoIterator<Item = &'a Path> + Send,
    loader: &Loader,
    source_location_table: &mut SourceLocationTable,
    report: &mut Report,
) -> SourceSet {
    report.enter_phase(Phase::CollectSource);

    let collector = Collector {
        ongoing: Mutex::new(HashSet::new()),
        result: Mutex::new(SourceSet::new()),
        source_location_table: Mutex::new(source_location_table),
        loader,
    };

    rayon::scope(|scope| {
        for input in inputs {
            collector.collect(scope, input);
        }
    });

    report.leave_phase(Phase::CollectSource);
    collector.result.into_inner().unwrap()
}

struct Collector<'l> {
    ongoing: Mutex<HashSet<Path>>,
    result: Mutex<SourceSet>,
    source_location_table: Mutex<&'l mut SourceLocationTable>,
    loader: &'l Loader,
}

impl<'l> Collector<'l> {
    fn collect<'a>(&'a self, scope: &rayon::Scope<'a>, path: &Path) {
        // Checks whether the load process is ongoing
        {
            let mut ongoing = self.ongoing.lock().unwrap();
            if ongoing.contains(path) {
                return;
            }
            ongoing.insert(path.clone());
        }

        let path = path.clone();
        scope.spawn(move |scope| {
            let mut locator = self
                .source_location_table
                .lock()
                .unwrap()
                .begin_locate(&path);

            let source = self
                .loader
                .load(path, &mut locator)
                .unwrap_or_else(|(path, error)| Source::from_error(path, error));

            self.source_location_table
                .lock()
                .unwrap()
                .complete_locate(locator);

            for dep in source.dependencies.values() {
                self.collect(scope, dep);
            }
            self.result.lock().unwrap().insert(source);
        });
    }
}
