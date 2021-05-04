pub fn prelude_module_for_module_test() -> String {
    PRELUDE_FOR_MODULE_TEST.to_string()
}

pub fn prelude_module_for_backend_test() -> String {
    PRELUDE_FOR_BACKEND_TEST.to_string()
}

pub static PRELUDE_FOR_MODULE_TEST: &str = include_str!("test-std/prelude-for-module-test.llrl");
pub static PRELUDE_FOR_BACKEND_TEST: &str = include_str!("test-std/prelude-for-backend-test.llrl");
