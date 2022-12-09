#[derive(Debug, Default, Clone, Copy)]
pub struct Options {
    pub optimize: Option<bool>,
    pub verbose: bool,
}

impl Options {
    pub fn optimize(self, optimize: bool) -> Self {
        Self {
            optimize: Some(optimize),
            ..self
        }
    }

    pub fn verbose(self, verbose: bool) -> Self {
        Self { verbose, ..self }
    }
}
