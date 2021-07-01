use std::cmp::Ordering;
use std::ffi::c_void;

// To make the executable binary of the llrl compiler standalone, `libllrt.a` itself is embedded as binary data.
pub static ARCHIVE: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/libllrt.a"));

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct Result<T> {
    pub success: i32,
    pub value: T,
}

impl<T> Result<T> {
    pub fn get(self) -> Option<T> {
        if self.success != 0 {
            Some(self.value)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct RtString {
    pub ptr: *const u8,
    pub len: u64,
}

impl RtString {
    pub unsafe fn concat(self, other: Self) -> Self {
        llrt_string_concat(self, other)
    }
}

impl From<&'static str> for RtString {
    fn from(src: &'static str) -> Self {
        Self {
            ptr: src.as_ptr(),
            len: src.len() as u64,
        }
    }
}

impl PartialEq for RtString {
    fn eq(&self, other: &Self) -> bool {
        unsafe { llrt_string_eq(*self, *other) != 0 }
    }
}

impl Eq for RtString {}

impl PartialOrd for RtString {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for RtString {
    fn cmp(&self, other: &Self) -> Ordering {
        match unsafe { llrt_string_cmp(*self, *other) } {
            x if x < 0 => Ordering::Less,
            x if x > 0 => Ordering::Greater,
            _ => Ordering::Equal,
        }
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct RtArgs {
    pub argc: u64,
    pub argv: *const RtString,
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct RtProcess {
    pub err: i32,
    pub pid: i32,
    pub cin: *const u8,
    pub cout: *const u8,
    pub cerr: *const u8,
}

extern "C" {
    pub fn llrt_init(argc: i32, argv: *const *const u8);
    pub fn llrt_args() -> RtArgs;

    pub fn llrt_panic(a: RtString) -> !;
    pub fn llrt_exit(exitcode: i32);

    pub fn llrt_spawn_process(name: *const u8, argv: *const *const u8) -> RtProcess;
    pub fn llrt_execute_process(name: *const u8, argv: *const *const u8) -> RtProcess;
    pub fn llrt_wait(pid: i32) -> i32;

    pub fn llrt_time() -> f64;

    pub fn llrt_getcwd() -> RtString;

    pub fn llrt_string_genid() -> RtString;
    pub fn llrt_string_eq(a: RtString, b: RtString) -> i32;
    pub fn llrt_string_cmp(a: RtString, b: RtString) -> i32;
    pub fn llrt_string_concat(a: RtString, b: RtString) -> RtString;

    pub fn llrt_f32_to_string(a: f32) -> RtString;
    pub fn llrt_f64_to_string(a: f64) -> RtString;
    pub fn llrt_i64_to_string(radix: u8, value: i64) -> RtString;
    pub fn llrt_u64_to_string(radix: u8, value: u64) -> RtString;

    pub fn llrt_string_to_i64(radix: u8, s: RtString) -> Result<i64>;
    pub fn llrt_string_to_u64(radix: u8, s: RtString) -> Result<u64>;
    pub fn llrt_string_to_f32(s: RtString) -> Result<f32>;
    pub fn llrt_string_to_f64(s: RtString) -> Result<f64>;

    pub fn llrt_readdir(dir: *const u8) -> RtString;

    pub fn llrt_stdin() -> *mut *const u8;
    pub fn llrt_stdout() -> *mut *const u8;
    pub fn llrt_stderr() -> *mut *const u8;

    pub fn llrt_current_errno() -> i32;

    pub fn llrt_xxh_seed() -> u64;

    pub fn GC_malloc(size: usize) -> *mut c_void;
}

#[test]
fn test_string() {
    unsafe {
        let hello = RtString::from("hello");
        let world = RtString::from("world");
        let helloworld = RtString::from("helloworld");
        assert_eq!(hello, hello);
        assert_ne!(hello, world);
        assert!(hello < world);
        assert!(hello < helloworld);

        let a = llrt_string_genid();
        let b = llrt_string_genid();
        assert_ne!(a, b);
        assert_eq!(helloworld, hello.concat(world));

        assert_eq!(RtString::from("0"), llrt_i64_to_string(10, 0));
        assert_eq!(RtString::from("123"), llrt_i64_to_string(10, 123));
        assert_eq!(RtString::from("-45678"), llrt_i64_to_string(10, -45678));
        assert_eq!(RtString::from("0"), llrt_u64_to_string(10, 0));
        assert_eq!(RtString::from("11121"), llrt_u64_to_string(10, 11121));
        assert_eq!(RtString::from("1234"), llrt_u64_to_string(16, 0x1234));
        assert_eq!(RtString::from("110101"), llrt_i64_to_string(2, 0b110101));

        assert_eq!(None, llrt_string_to_i64(10, "".into()).get());
        assert_eq!(None, llrt_string_to_i64(10, "@".into()).get());
        assert_eq!(None, llrt_string_to_i64(10, "0@".into()).get());
        assert_eq!(None, llrt_string_to_i64(10, "@0".into()).get());
        assert_eq!(Some(0), llrt_string_to_i64(10, "0".into()).get());
        assert_eq!(Some(5), llrt_string_to_i64(10, "5".into()).get());
        assert_eq!(Some(139), llrt_string_to_i64(10, "139".into()).get());
        assert_eq!(Some(-72), llrt_string_to_i64(10, "-72".into()).get());
        assert_eq!(Some(100001), llrt_string_to_i64(10, "100001".into()).get());
        assert_eq!(None, llrt_string_to_i64(2, "3".into()).get());
        assert_eq!(Some(0b101010), llrt_string_to_i64(2, "101010".into()).get());
        assert_eq!(
            Some(0xDEADBEAF),
            llrt_string_to_i64(16, "DeAdBeAf".into()).get()
        );
        assert_eq!(
            Some(-9223372036854775808),
            llrt_string_to_i64(10, "-9223372036854775808".into()).get()
        );
        assert_eq!(
            None,
            llrt_string_to_i64(10, "-9223372036854775809".into()).get()
        );
        assert_eq!(
            None,
            llrt_string_to_i64(10, "9223372036854775808".into()).get()
        );
        assert_eq!(
            Some(9223372036854775807),
            llrt_string_to_i64(10, "9223372036854775807".into()).get()
        );
        assert_eq!(
            Some(0x7FFFFFFFFFFFFFFF),
            llrt_string_to_i64(16, "7FFFFFFFFFFFFFFF".into()).get()
        );
        assert_eq!(
            Some(-0x8000000000000000),
            llrt_string_to_i64(16, "-8000000000000000".into()).get()
        );
        assert_eq!(None, llrt_string_to_u64(10, "".into()).get());
        assert_eq!(Some(0), llrt_string_to_u64(10, "0".into()).get());
        assert_eq!(Some(101), llrt_string_to_u64(10, "101".into()).get());
        assert_eq!(None, llrt_string_to_u64(10, "-0".into()).get()); // NOTE: Should this be Some(0)?
        assert_eq!(None, llrt_string_to_u64(10, "-1".into()).get());
        assert_eq!(None, llrt_string_to_u64(10, "-32".into()).get());
        assert_eq!(
            Some(18446744073709551615),
            llrt_string_to_u64(10, "18446744073709551615".into()).get()
        );
        assert_eq!(
            Some(18446744073709551611),
            llrt_string_to_u64(10, "18446744073709551611".into()).get()
        );
        assert_eq!(
            None,
            llrt_string_to_u64(10, "18446744073709551616".into()).get()
        );

        assert_eq!(None, llrt_string_to_f32("".into()).get());
        assert_eq!(Some(0.0), llrt_string_to_f32("0".into()).get());
        assert_eq!(Some(34.0), llrt_string_to_f32("34".into()).get());
        assert_eq!(Some(3.14), llrt_string_to_f32("3.14".into()).get());
        assert_eq!(Some(-2.5), llrt_string_to_f32("-2.5".into()).get());
        assert_eq!(Some(1e+10), llrt_string_to_f32("1e+10".into()).get());
        assert_eq!(Some(-1e-10), llrt_string_to_f32("-1e-10".into()).get());
        assert_eq!(
            Some(0.03125e+30),
            llrt_string_to_f64("0.03125e+30".into()).get()
        );
        assert_eq!(
            Some(96.03125e-30),
            llrt_string_to_f64("96.03125e-30".into()).get()
        );
        assert_eq!(None, llrt_string_to_f64("@".into()).get());
        assert_eq!(None, llrt_string_to_f64("0@".into()).get());
        assert_eq!(None, llrt_string_to_f64("@0".into()).get());
    }
}
