use super::FunctionSymbol;
use crate::backend::native::calling::{native_macro, native_main, CallConv};
use crate::lowering::ir::*;
use once_cell::sync::Lazy;
use std::collections::HashMap;
use xten::asm::Object;
use xten::jit;

pub struct Executor {
    engine: jit::Engine,
}

impl Executor {
    pub fn new() -> Self {
        let engine: jit::Engine = jit::Engine::new(Self::symbol_resolver);
        Self { engine }
    }

    pub fn add_object(&mut self, obj: &Object) {
        self.engine.add_object(obj).unwrap();
    }

    pub fn call_macro(&self, f: &FunctionSymbol, s: Syntax<Sexp>) -> Result<Syntax<Sexp>, String> {
        assert!(
            matches!(f.call_conv, CallConv::Macro),
            "Calling convention mismatch: expected macro but got {:?}",
            f.call_conv
        );
        match self.engine.get(&f.name) {
            Some(f) => {
                let handler = unsafe { native_macro(f as _) };
                handler(s)
            }
            None => panic!("Function not found: {}", f.name),
        }
    }

    pub fn call_main(&self, f: &FunctionSymbol) -> bool {
        assert!(
            matches!(f.call_conv, CallConv::Main),
            "Calling convention mismatch: expected main but got {:?}",
            f.call_conv
        );
        match self.engine.get(&f.name) {
            Some(f) => {
                let handler = unsafe { native_main(f as _) };
                handler()
            }
            None => panic!("Function not found: {}", f.name),
        }
    }

    fn symbol_resolver(sym: &str) -> *const u8 {
        static LLRT_SYMBOLS: Lazy<HashMap<String, LlrtSymbol>> = Lazy::new(|| {
            let mut map = HashMap::new();

            for (name, addr) in llrt::symbols() {
                map.insert(name.to_string(), LlrtSymbol(addr as *mut u8));
            }

            map
        });

        match LLRT_SYMBOLS.get(sym) {
            Some(sym) => sym.0,
            None => jit::symbol_resolver::dl::default(sym),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
struct LlrtSymbol(*mut u8);

unsafe impl Send for LlrtSymbol {}
unsafe impl Sync for LlrtSymbol {}
