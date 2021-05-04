#[derive(Debug, Default, Clone, Copy)]
pub struct Options {
    pub opt_level: Option<llvm::OptLevel>,
    pub reloc_mode: Option<llvm::RelocMode>,
    pub code_model: Option<llvm::CodeModel>,
    pub verbose: bool,
}

impl Options {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn optimize(self, optimize: bool) -> Self {
        Self {
            opt_level: Some(match optimize {
                true => llvm::OptLevel::Default,
                false => llvm::OptLevel::None,
            }),
            ..self
        }
    }

    pub fn verbose(self, verbose: bool) -> Self {
        Self { verbose, ..self }
    }
}
