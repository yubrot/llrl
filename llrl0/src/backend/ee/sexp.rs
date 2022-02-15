use super::*;
use crate::emitter::ir::Sexp;

#[derive(Clone, Copy)]
#[repr(C)]
pub struct EeSexp {
    tag: u64,
    body: EeSexpBody,
}

impl EeSexp {
    pub fn integer(signed: bool, value: u64) -> Self {
        let integer = EeSexpInteger::new(signed, value);
        Self {
            tag: 0,
            body: EeSexpBody { integer },
        }
    }

    pub fn fpnumber(value: f64) -> Self {
        let fpnumber = EeSexpFPNumber::new(value);
        Self {
            tag: 1,
            body: EeSexpBody { fpnumber },
        }
    }

    pub fn bool(value: bool) -> Self {
        let bool = EeSexpBool::new(value);
        Self {
            tag: 2,
            body: EeSexpBody { bool },
        }
    }

    pub fn symbol(value: EeString) -> Self {
        let symbol = EeSexpSymbol::new(value);
        Self {
            tag: 3,
            body: EeSexpBody { symbol },
        }
    }

    pub fn string(value: EeString) -> Self {
        let string = EeSexpString::new(value);
        Self {
            tag: 4,
            body: EeSexpBody { string },
        }
    }

    pub fn char(value: EeChar) -> Self {
        let char = EeSexpChar::new(value);
        Self {
            tag: 5,
            body: EeSexpBody { char },
        }
    }

    pub fn cons(car: EeSyntax<EeSexp>, cdr: EeSyntax<EeSexp>) -> Self {
        let cons = EeSexpCons::new(car, cdr);
        Self {
            tag: 6,
            body: EeSexpBody { cons },
        }
    }

    pub fn nil() -> Self {
        let nil = EeSexpNil;
        Self {
            tag: 7,
            body: EeSexpBody { nil },
        }
    }

    pub fn use_(value: EeCapturedUse) -> Self {
        let use_ = EeSexpUse::new(value);
        Self {
            tag: 8,
            body: EeSexpBody { use_ },
        }
    }

    fn into_view(self) -> EeSexpView {
        match self.tag {
            0 => EeSexpView::Integer(unsafe { self.body.integer }),
            1 => EeSexpView::FPNumber(unsafe { self.body.fpnumber }),
            2 => EeSexpView::Bool(unsafe { self.body.bool }),
            3 => EeSexpView::Symbol(unsafe { self.body.symbol }),
            4 => EeSexpView::String(unsafe { self.body.string }),
            5 => EeSexpView::Char(unsafe { self.body.char }),
            6 => EeSexpView::Cons(unsafe { self.body.cons }),
            7 => EeSexpView::Nil(unsafe { self.body.nil }),
            8 => EeSexpView::Use(unsafe { self.body.use_ }),
            _ => panic!("Wrong tag: {}", self.tag),
        }
    }
}

impl EeValue for EeSexp {
    type HostValue = Sexp;

    fn into_host(self) -> Self::HostValue {
        match self.into_view() {
            EeSexpView::Integer(i) => Sexp::Integer(i.signed, i.value),
            EeSexpView::FPNumber(f) => Sexp::FPNumber(f.value.into()),
            EeSexpView::Bool(b) => Sexp::Bool(b.value),
            EeSexpView::Symbol(s) => Sexp::Symbol(s.value.into_host()),
            EeSexpView::String(s) => Sexp::String(s.value.into_host()),
            EeSexpView::Char(c) => Sexp::Char(c.value.into_host()),
            EeSexpView::Cons(c) => Sexp::Cons(c.car.into_host(), c.cdr.into_host()),
            EeSexpView::Nil(_) => Sexp::Nil,
            EeSexpView::Use(u) => Sexp::Use(u.value.into_host()),
        }
    }

    fn from_host(host_value: Self::HostValue) -> Self {
        match host_value {
            Sexp::Integer(signed, value) => EeSexp::integer(signed, value),
            Sexp::FPNumber(value) => EeSexp::fpnumber(value.into_inner()),
            Sexp::Bool(value) => EeSexp::bool(value),
            Sexp::Symbol(value) => EeSexp::symbol(EeString::from_host(value)),
            Sexp::String(value) => EeSexp::string(EeString::from_host(value)),
            Sexp::Char(value) => EeSexp::char(EeChar::from_host(value)),
            Sexp::Cons(car, cdr) => EeSexp::cons(
                EeSyntax::<EeSexp>::from_host(car),
                EeSyntax::<EeSexp>::from_host(cdr),
            ),
            Sexp::Nil => EeSexp::nil(),
            Sexp::Use(value) => EeSexp::use_(EeCapturedUse::from_host(value)),
        }
    }
}

impl EeData for EeSexp {
    fn direct_data(&self) -> &[u8] {
        sized_direct_data(self)
    }

    fn has_indirect_data(&self) -> bool {
        matches!(
            self.into_view(),
            EeSexpView::Symbol(_) | EeSexpView::String(_) | EeSexpView::Cons(_)
        )
    }

    fn traverse_indirect_data_(&self, mut handler: EeIndirectDataHandler) {
        let mut handler = handler.offset(offset_of!(Self, body));
        match self.into_view() {
            EeSexpView::Symbol(s) => {
                s.value
                    .traverse_indirect_data_(handler.offset(offset_of!(EeSexpSymbol, value)));
            }
            EeSexpView::String(s) => {
                s.value
                    .traverse_indirect_data_(handler.offset(offset_of!(EeSexpString, value)));
            }
            EeSexpView::Cons(c) => {
                c.car
                    .traverse_indirect_data_(handler.offset(offset_of!(EeSexpCons, car)));
                c.cdr
                    .traverse_indirect_data_(handler.offset(offset_of!(EeSexpCons, cdr)));
            }
            _ => {}
        }
    }
}

enum EeSexpView {
    Integer(EeSexpInteger),
    FPNumber(EeSexpFPNumber),
    Bool(EeSexpBool),
    Symbol(EeSexpSymbol),
    String(EeSexpString),
    Char(EeSexpChar),
    Cons(EeSexpCons),
    Nil(EeSexpNil),
    Use(EeSexpUse),
}

#[derive(Clone, Copy)]
#[repr(C)]
union EeSexpBody {
    integer: EeSexpInteger,   // for tag=0
    fpnumber: EeSexpFPNumber, // for tag=1
    bool: EeSexpBool,         // for tag=2
    symbol: EeSexpSymbol,     // for tag=3
    string: EeSexpString,     // for tag=4
    char: EeSexpChar,         // for tag=5
    cons: EeSexpCons,         // for tag=6
    nil: EeSexpNil,           // for tag=7
    use_: EeSexpUse,          // for tag=8
}

#[derive(Debug, Clone, Copy, new)]
#[repr(C)]
struct EeSexpInteger {
    signed: bool,
    value: u64,
}

#[derive(Debug, Clone, Copy, new)]
#[repr(C)]
struct EeSexpFPNumber {
    value: f64,
}

#[derive(Debug, Clone, Copy, new)]
#[repr(C)]
struct EeSexpBool {
    value: bool,
}

#[derive(Debug, Clone, Copy, new)]
#[repr(C)]
struct EeSexpSymbol {
    value: EeString,
}

#[derive(Debug, Clone, Copy, new)]
#[repr(C)]
struct EeSexpString {
    value: EeString,
}

#[derive(Debug, Clone, Copy, new)]
#[repr(C)]
struct EeSexpChar {
    value: EeChar,
}

#[derive(Clone, Copy, new)]
#[repr(C)]
struct EeSexpCons {
    car: EeSyntax<EeSexp>,
    cdr: EeSyntax<EeSexp>,
}

#[derive(Debug, Clone, Copy, new)]
#[repr(C)]
struct EeSexpNil;

#[derive(Debug, Clone, Copy, new)]
#[repr(C)]
struct EeSexpUse {
    value: EeCapturedUse,
}
