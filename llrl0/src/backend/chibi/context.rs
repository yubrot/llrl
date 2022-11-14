use crate::backend::native::calling::CallConv;
use crate::backend::native::mem_layout::{Layout, LayoutResolver};
use crate::lowering::ir::*;
use derive_new::new;
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Debug)]
pub struct Context {
    layout_resolver: LayoutResolver,
    function_symbols: HashMap<CtId, FunctionSymbol>,
    main_function_symbol: Option<FunctionSymbol>,
}

impl Context {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            layout_resolver: LayoutResolver::new(),
            function_symbols: HashMap::new(),
            main_function_symbol: None,
        }
    }

    pub fn function_symbol(&self, name: CtId) -> Option<&FunctionSymbol> {
        self.function_symbols.get(&name)
    }

    pub fn define_function_symbol(&mut self, name: CtId, symbol: FunctionSymbol) {
        assert!(
            self.function_symbols.insert(name, symbol).is_none(),
            "Duplicate function symbol definition: {}",
            name
        );
    }

    pub fn main_function_symbol(&self) -> Option<&FunctionSymbol> {
        self.main_function_symbol.as_ref()
    }

    pub fn define_main_function_symbol(&mut self, symbol: FunctionSymbol) {
        assert!(
            self.main_function_symbol.replace(symbol).is_none(),
            "Duplicate main function symbol definition",
        );
    }

    pub fn add_types(&mut self, defs: &HashMap<CtId, Arc<Def>>) {
        self.layout_resolver.register(defs);
    }

    pub fn layout(&self, ct: &Ct) -> Layout {
        self.layout_resolver.get(ct)
    }
}

#[derive(PartialEq, Eq, Debug, Clone, new)]
pub struct FunctionSymbol {
    pub name: String,
    pub call_conv: CallConv,
    pub ty: Ct,
}
