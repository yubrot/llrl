use itertools::Itertools;
use std::collections::{hash_map, HashMap};
use std::fmt;
use std::time::{Duration, Instant};

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
pub enum Phase {
    CollectSource,
    BuildModule,
    Import,
    BuildAst,
    Resolve,
    KindInference,
    TypeInference,
    Validate,
    Export,
    Lowerize,
    Codegen,
    JIT,
    Link,
}

impl fmt::Display for Phase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::CollectSource => write!(f, "collect-source"),
            Self::BuildModule => write!(f, "build-module"),
            Self::Import => write!(f, "import"),
            Self::BuildAst => write!(f, "build-ast"),
            Self::Resolve => write!(f, "resolve"),
            Self::KindInference => write!(f, "kind_inference"),
            Self::TypeInference => write!(f, "type_inference"),
            Self::Validate => write!(f, "validate"),
            Self::Export => write!(f, "export"),
            Self::Lowerize => write!(f, "lowerize"),
            Self::Codegen => write!(f, "codegen"),
            Self::JIT => write!(f, "jit"),
            Self::Link => write!(f, "link"),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Report {
    map: HashMap<Phase, (Instant, Duration)>,
}

impl Report {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn durations(&self) -> impl Iterator<Item = (Phase, Duration)> + '_ {
        self.map
            .iter()
            .sorted()
            .map(|(phase, (_, duration))| (*phase, *duration))
    }

    pub fn on<T>(&mut self, phase: Phase, f: impl FnOnce() -> T) -> T {
        self.enter_phase(phase);
        let ret = f();
        self.leave_phase(phase);
        ret
    }

    pub fn enter_phase(&mut self, phase: Phase) {
        match self.map.entry(phase) {
            hash_map::Entry::Occupied(mut entry) => {
                entry.get_mut().0 = Instant::now();
            }
            hash_map::Entry::Vacant(entry) => {
                entry.insert((Instant::now(), Duration::default()));
            }
        }
    }

    pub fn leave_phase(&mut self, phase: Phase) {
        let entry = self.map.get_mut(&phase).unwrap();
        entry.1 += entry.0.elapsed();
    }

    pub fn merge(&mut self, other: &Report) {
        for (phase, duration) in other.durations() {
            self.map
                .entry(phase)
                .or_insert_with(|| (Instant::now(), Duration::default()))
                .1 += duration;
        }
    }
}

impl fmt::Display for Report {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (phase, duration) in self.durations() {
            writeln!(f, "{}: {}s", phase, duration.as_secs_f64())?;
        }
        Ok(())
    }
}
