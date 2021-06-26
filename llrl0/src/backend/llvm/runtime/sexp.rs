use super::*;
use crate::emitter::ir::Sexp;

#[derive(Clone, Copy)]
#[repr(C)]
pub struct RtSexp {
    tag: u64,
    body: RtSexpBody,
}

impl RtSexp {
    pub fn integer(signed: bool, value: u64) -> Self {
        let integer = RtSexpInteger::new(signed, value);
        Self {
            tag: 0,
            body: RtSexpBody { integer },
        }
    }

    pub fn fpnumber(value: f64) -> Self {
        let fpnumber = RtSexpFPNumber::new(value);
        Self {
            tag: 1,
            body: RtSexpBody { fpnumber },
        }
    }

    pub fn bool(value: bool) -> Self {
        let bool = RtSexpBool::new(value);
        Self {
            tag: 2,
            body: RtSexpBody { bool },
        }
    }

    pub fn symbol(value: RtString) -> Self {
        let symbol = RtSexpSymbol::new(value);
        Self {
            tag: 3,
            body: RtSexpBody { symbol },
        }
    }

    pub fn string(value: RtString) -> Self {
        let string = RtSexpString::new(value);
        Self {
            tag: 4,
            body: RtSexpBody { string },
        }
    }

    pub fn char(value: RtChar) -> Self {
        let char = RtSexpChar::new(value);
        Self {
            tag: 5,
            body: RtSexpBody { char },
        }
    }

    pub fn cons(car: RtSyntax<RtSexp>, cdr: RtSyntax<RtSexp>) -> Self {
        let cons = RtSexpCons::new(car, cdr);
        Self {
            tag: 6,
            body: RtSexpBody { cons },
        }
    }

    pub fn nil() -> Self {
        let nil = RtSexpNil;
        Self {
            tag: 7,
            body: RtSexpBody { nil },
        }
    }

    pub fn use_(value: RtCapturedUse) -> Self {
        let use_ = RtSexpUse::new(value);
        Self {
            tag: 8,
            body: RtSexpBody { use_ },
        }
    }

    pub fn into_view(self) -> RtSexpView {
        match self.tag {
            0 => RtSexpView::Integer(unsafe { self.body.integer }),
            1 => RtSexpView::FPNumber(unsafe { self.body.fpnumber }),
            2 => RtSexpView::Bool(unsafe { self.body.bool }),
            3 => RtSexpView::Symbol(unsafe { self.body.symbol }),
            4 => RtSexpView::String(unsafe { self.body.string }),
            5 => RtSexpView::Char(unsafe { self.body.char }),
            6 => RtSexpView::Cons(unsafe { self.body.cons }),
            7 => RtSexpView::Nil(unsafe { self.body.nil }),
            8 => RtSexpView::Use(unsafe { self.body.use_ }),
            _ => panic!("Wrong tag: {}", self.tag),
        }
    }
}

impl RtValue for RtSexp {
    type Native = Sexp;

    fn size_align() -> (usize, usize) {
        (size_of::<Self>(), align_of::<Self>())
    }

    fn into_native(self) -> Self::Native {
        match self.into_view() {
            RtSexpView::Integer(i) => Sexp::Integer(i.signed, i.value),
            RtSexpView::FPNumber(f) => Sexp::FPNumber(f.value.into()),
            RtSexpView::Bool(b) => Sexp::Bool(b.value),
            RtSexpView::Symbol(s) => Sexp::Symbol(s.value.into_native()),
            RtSexpView::String(s) => Sexp::String(s.value.into_native()),
            RtSexpView::Char(c) => Sexp::Char(c.value.into_native()),
            RtSexpView::Cons(c) => Sexp::Cons(c.car.into_native(), c.cdr.into_native()),
            RtSexpView::Nil(_) => Sexp::Nil,
            RtSexpView::Use(u) => Sexp::Use(u.value.into_native()),
        }
    }

    fn from_native(native: Self::Native) -> Self {
        match native {
            Sexp::Integer(signed, value) => RtSexp::integer(signed, value),
            Sexp::FPNumber(value) => RtSexp::fpnumber(value.into_inner()),
            Sexp::Bool(value) => RtSexp::bool(value),
            Sexp::Symbol(value) => RtSexp::symbol(RtString::from_native(value)),
            Sexp::String(value) => RtSexp::string(RtString::from_native(value)),
            Sexp::Char(value) => RtSexp::char(RtChar::from_native(value)),
            Sexp::Cons(car, cdr) => RtSexp::cons(
                RtSyntax::<RtSexp>::from_native(car),
                RtSyntax::<RtSexp>::from_native(cdr),
            ),
            Sexp::Nil => RtSexp::nil(),
            Sexp::Use(value) => RtSexp::use_(RtCapturedUse::from_native(value)),
        }
    }
}

impl RtExpand for RtSexp {
    fn expand_on_buffer<'ctx: 'm, 'm>(
        &self,
        buf: &mut [LLVMConstant<'ctx, 'm>],
        module: &'m LLVMModule<'ctx>,
    ) {
        let o = buf_offset!(Self, body);
        match self.into_view() {
            RtSexpView::Symbol(s) => s.expand_on_buffer(&mut buf[o..], module),
            RtSexpView::String(s) => s.expand_on_buffer(&mut buf[o..], module),
            RtSexpView::Cons(c) => c.expand_on_buffer(&mut buf[o..], module),
            _ => {}
        }
    }
}

pub enum RtSexpView {
    Integer(RtSexpInteger),
    FPNumber(RtSexpFPNumber),
    Bool(RtSexpBool),
    Symbol(RtSexpSymbol),
    String(RtSexpString),
    Char(RtSexpChar),
    Cons(RtSexpCons),
    Nil(RtSexpNil),
    Use(RtSexpUse),
}

#[derive(Clone, Copy)]
#[repr(C)]
union RtSexpBody {
    integer: RtSexpInteger,   // 0
    fpnumber: RtSexpFPNumber, // 1
    bool: RtSexpBool,         // 2
    symbol: RtSexpSymbol,     // 3
    string: RtSexpString,     // 4
    char: RtSexpChar,         // 5
    cons: RtSexpCons,         // 6
    nil: RtSexpNil,           // 7
    use_: RtSexpUse,          // 8
}

#[derive(Debug, Clone, Copy, new)]
#[repr(C)]
pub struct RtSexpInteger {
    pub signed: bool,
    pub value: u64,
}

#[derive(Debug, Clone, Copy, new)]
#[repr(C)]
pub struct RtSexpFPNumber {
    pub value: f64,
}

#[derive(Debug, Clone, Copy, new)]
#[repr(C)]
pub struct RtSexpBool {
    pub value: bool,
}

#[derive(Debug, Clone, Copy, new)]
#[repr(C)]
pub struct RtSexpSymbol {
    pub value: RtString,
}

impl RtExpand for RtSexpSymbol {
    fn expand_on_buffer<'ctx: 'm, 'm>(
        &self,
        buf: &mut [LLVMConstant<'ctx, 'm>],
        module: &'m LLVMModule<'ctx>,
    ) {
        self.value.expand_on_buffer(buf, module);
    }
}

#[derive(Debug, Clone, Copy, new)]
#[repr(C)]
pub struct RtSexpString {
    pub value: RtString,
}

impl RtExpand for RtSexpString {
    fn expand_on_buffer<'ctx: 'm, 'm>(
        &self,
        buf: &mut [LLVMConstant<'ctx, 'm>],
        module: &'m LLVMModule<'ctx>,
    ) {
        self.value.expand_on_buffer(buf, module);
    }
}

#[derive(Debug, Clone, Copy, new)]
#[repr(C)]
pub struct RtSexpChar {
    pub value: RtChar,
}

#[derive(Clone, Copy, new)]
#[repr(C)]
pub struct RtSexpCons {
    pub car: RtSyntax<RtSexp>,
    pub cdr: RtSyntax<RtSexp>,
}

impl RtExpand for RtSexpCons {
    fn expand_on_buffer<'ctx: 'm, 'm>(
        &self,
        buf: &mut [LLVMConstant<'ctx, 'm>],
        module: &'m LLVMModule<'ctx>,
    ) {
        self.car.expand_on_buffer(buf, module);
        self.cdr
            .expand_on_buffer(&mut buf[buf_offset!(Self, cdr)..], module);
    }
}

#[derive(Debug, Clone, Copy, new)]
#[repr(C)]
pub struct RtSexpNil;

#[derive(Debug, Clone, Copy, new)]
#[repr(C)]
pub struct RtSexpUse {
    pub value: RtCapturedUse,
}
