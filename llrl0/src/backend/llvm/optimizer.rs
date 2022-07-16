use llvm::prelude::*;

#[derive(Debug)]
pub struct Optimizer {
    pm: LLVMBox<LLVMPassManager>,
}

impl Optimizer {
    pub fn new(opt_level: Option<llvm::OptLevel>) -> Self {
        let pm = LLVMPassManager::new();
        let pmb = LLVMPassManagerBuilder::new();
        pmb.set_opt_level(opt_level.unwrap_or_default().into());
        pmb.populate_pass_manager(&pm);
        Self { pm }
    }

    pub fn run(&self, module: &LLVMModule<'_>) {
        self.pm.run(module);
    }
}
