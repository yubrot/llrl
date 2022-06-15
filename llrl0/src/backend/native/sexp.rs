use super::*;
use crate::lowering::ir::Sexp;

#[derive(Clone, Copy)]
#[repr(C)]
pub struct NativeSexp {
    tag: u64,
    body: NativeSexpBody,
}

impl NativeSexp {
    pub fn integer(signed: bool, value: u64) -> Self {
        let integer = NativeSexpInteger::new(signed, value);
        Self {
            tag: 0,
            body: NativeSexpBody { integer },
        }
    }

    pub fn fpnumber(value: f64) -> Self {
        let fpnumber = NativeSexpFPNumber::new(value);
        Self {
            tag: 1,
            body: NativeSexpBody { fpnumber },
        }
    }

    pub fn bool(value: bool) -> Self {
        let bool = NativeSexpBool::new(value);
        Self {
            tag: 2,
            body: NativeSexpBody { bool },
        }
    }

    pub fn symbol(value: NativeString) -> Self {
        let symbol = NativeSexpSymbol::new(value);
        Self {
            tag: 3,
            body: NativeSexpBody { symbol },
        }
    }

    pub fn string(value: NativeString) -> Self {
        let string = NativeSexpString::new(value);
        Self {
            tag: 4,
            body: NativeSexpBody { string },
        }
    }

    pub fn char(value: NativeChar) -> Self {
        let char = NativeSexpChar::new(value);
        Self {
            tag: 5,
            body: NativeSexpBody { char },
        }
    }

    pub fn cons(car: NativeSyntax<NativeSexp>, cdr: NativeSyntax<NativeSexp>) -> Self {
        let cons = NativeSexpCons::new(car, cdr);
        Self {
            tag: 6,
            body: NativeSexpBody { cons },
        }
    }

    pub fn nil() -> Self {
        let nil = NativeSexpNil;
        Self {
            tag: 7,
            body: NativeSexpBody { nil },
        }
    }

    pub fn use_(value: NativeCapturedUse) -> Self {
        let use_ = NativeSexpUse::new(value);
        Self {
            tag: 8,
            body: NativeSexpBody { use_ },
        }
    }

    fn into_view(self) -> NativeSexpView {
        match self.tag {
            0 => NativeSexpView::Integer(unsafe { self.body.integer }),
            1 => NativeSexpView::FPNumber(unsafe { self.body.fpnumber }),
            2 => NativeSexpView::Bool(unsafe { self.body.bool }),
            3 => NativeSexpView::Symbol(unsafe { self.body.symbol }),
            4 => NativeSexpView::String(unsafe { self.body.string }),
            5 => NativeSexpView::Char(unsafe { self.body.char }),
            6 => NativeSexpView::Cons(unsafe { self.body.cons }),
            7 => NativeSexpView::Nil(unsafe { self.body.nil }),
            8 => NativeSexpView::Use(unsafe { self.body.use_ }),
            _ => panic!("Wrong tag: {}", self.tag),
        }
    }
}

impl NativeValue for NativeSexp {
    type HostValue = Sexp;

    fn into_host(self) -> Self::HostValue {
        match self.into_view() {
            NativeSexpView::Integer(i) => Sexp::Integer(i.signed, i.value),
            NativeSexpView::FPNumber(f) => Sexp::FPNumber(f.value.into()),
            NativeSexpView::Bool(b) => Sexp::Bool(b.value),
            NativeSexpView::Symbol(s) => Sexp::Symbol(s.value.into_host()),
            NativeSexpView::String(s) => Sexp::String(s.value.into_host()),
            NativeSexpView::Char(c) => Sexp::Char(c.value.into_host()),
            NativeSexpView::Cons(c) => Sexp::Cons(c.car.into_host(), c.cdr.into_host()),
            NativeSexpView::Nil(_) => Sexp::Nil,
            NativeSexpView::Use(u) => Sexp::Use(u.value.into_host()),
        }
    }

    fn from_host(host_value: Self::HostValue) -> Self {
        match host_value {
            Sexp::Integer(signed, value) => NativeSexp::integer(signed, value),
            Sexp::FPNumber(value) => NativeSexp::fpnumber(value.into_inner()),
            Sexp::Bool(value) => NativeSexp::bool(value),
            Sexp::Symbol(value) => NativeSexp::symbol(NativeString::from_host(value)),
            Sexp::String(value) => NativeSexp::string(NativeString::from_host(value)),
            Sexp::Char(value) => NativeSexp::char(NativeChar::from_host(value)),
            Sexp::Cons(car, cdr) => NativeSexp::cons(
                NativeSyntax::<NativeSexp>::from_host(car),
                NativeSyntax::<NativeSexp>::from_host(cdr),
            ),
            Sexp::Nil => NativeSexp::nil(),
            Sexp::Use(value) => NativeSexp::use_(NativeCapturedUse::from_host(value)),
        }
    }
}

impl NativeData for NativeSexp {
    fn direct_data(&self) -> &[u8] {
        sized_direct_data(self)
    }

    fn has_indirect_data(&self) -> bool {
        matches!(
            self.into_view(),
            NativeSexpView::Symbol(_) | NativeSexpView::String(_) | NativeSexpView::Cons(_)
        )
    }

    fn traverse_indirect_data_(&self, mut handler: NativeIndirectDataHandler) {
        let mut handler = handler.offset(offset_of!(Self, body));
        match self.into_view() {
            NativeSexpView::Symbol(s) => {
                s.value
                    .traverse_indirect_data_(handler.offset(offset_of!(NativeSexpSymbol, value)));
            }
            NativeSexpView::String(s) => {
                s.value
                    .traverse_indirect_data_(handler.offset(offset_of!(NativeSexpString, value)));
            }
            NativeSexpView::Cons(c) => {
                c.car
                    .traverse_indirect_data_(handler.offset(offset_of!(NativeSexpCons, car)));
                c.cdr
                    .traverse_indirect_data_(handler.offset(offset_of!(NativeSexpCons, cdr)));
            }
            _ => {}
        }
    }
}

enum NativeSexpView {
    Integer(NativeSexpInteger),
    FPNumber(NativeSexpFPNumber),
    Bool(NativeSexpBool),
    Symbol(NativeSexpSymbol),
    String(NativeSexpString),
    Char(NativeSexpChar),
    Cons(NativeSexpCons),
    Nil(NativeSexpNil),
    Use(NativeSexpUse),
}

#[derive(Clone, Copy)]
#[repr(C)]
union NativeSexpBody {
    integer: NativeSexpInteger,   // for tag=0
    fpnumber: NativeSexpFPNumber, // for tag=1
    bool: NativeSexpBool,         // for tag=2
    symbol: NativeSexpSymbol,     // for tag=3
    string: NativeSexpString,     // for tag=4
    char: NativeSexpChar,         // for tag=5
    cons: NativeSexpCons,         // for tag=6
    nil: NativeSexpNil,           // for tag=7
    use_: NativeSexpUse,          // for tag=8
}

#[derive(Debug, Clone, Copy, new)]
#[repr(C)]
struct NativeSexpInteger {
    signed: bool,
    value: u64,
}

#[derive(Debug, Clone, Copy, new)]
#[repr(C)]
struct NativeSexpFPNumber {
    value: f64,
}

#[derive(Debug, Clone, Copy, new)]
#[repr(C)]
struct NativeSexpBool {
    value: bool,
}

#[derive(Debug, Clone, Copy, new)]
#[repr(C)]
struct NativeSexpSymbol {
    value: NativeString,
}

#[derive(Debug, Clone, Copy, new)]
#[repr(C)]
struct NativeSexpString {
    value: NativeString,
}

#[derive(Debug, Clone, Copy, new)]
#[repr(C)]
struct NativeSexpChar {
    value: NativeChar,
}

#[derive(Clone, Copy, new)]
#[repr(C)]
struct NativeSexpCons {
    car: NativeSyntax<NativeSexp>,
    cdr: NativeSyntax<NativeSexp>,
}

#[derive(Debug, Clone, Copy, new)]
#[repr(C)]
struct NativeSexpNil;

#[derive(Debug, Clone, Copy, new)]
#[repr(C)]
struct NativeSexpUse {
    value: NativeCapturedUse,
}
