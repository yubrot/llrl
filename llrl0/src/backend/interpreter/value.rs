use super::{Error, Result};
use crate::lowering::ir::*;
use crate::string;
use if_chain::if_chain;
use itertools::Itertools;
use ordered_float::OrderedFloat;
use std::cmp::Ordering;
use std::fmt;
use std::sync::{Arc, Mutex};

#[derive(Debug, Clone)]
pub enum Value {
    S(i64),
    U(u64),
    F32(OrderedFloat<f32>),
    F64(OrderedFloat<f64>),
    String(String),
    CapturedUse(CapturedUse),
    Unit,
    Clos(CtId, Option<Box<Value>>),
    Env(Vec<Value>),
    Struct(Vec<Value>),
    Syntax(SyntaxMetadata, Box<Value>),
    Box(Arc<Mutex<Value>>),
}

impl Default for Value {
    fn default() -> Self {
        Self::Unit
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::S(value) => write!(f, "{}", value),
            Self::U(value) => write!(f, "{}", value),
            Self::F32(value) => write!(f, "{}", value),
            Self::F64(value) => write!(f, "{}", value),
            Self::String(s) => write!(f, "\"{}\"", string::escape(s)),
            Self::CapturedUse(use_) => write!(f, "{}", use_),
            Self::Unit => write!(f, "unit"),
            Self::Clos(id, None) => write!(f, "closure({}, none)", id),
            Self::Clos(id, Some(env)) => write!(f, "closure({}, {})", id, env),
            Self::Env(elems) => write!(f, "env({})", elems.iter().format(", ")),
            Self::Syntax(_, body) => write!(f, "syntax({})", body),
            Self::Struct(fields) => write!(f, "{{{}}}", fields.iter().format(", ")),
            Self::Box(v) => write!(f, "box({})", v.lock().unwrap()),
        }
    }
}

impl Value {
    pub fn bool(value: bool) -> Self {
        Self::U(if value { 1 } else { 0 })
    }

    pub fn char(value: char) -> Self {
        Self::U(value as u64)
    }

    pub fn r#box(value: Self) -> Self {
        Self::Box(Arc::new(Mutex::new(value)))
    }

    pub fn from_const(c: &Const) -> Result<Self> {
        Ok(match c {
            Const::Integer(ty, signed, value) => match ty {
                Ct::S(_) => Self::S(*value as i64),
                Ct::U(_) if !*signed || 0 <= *value as i64 => Self::U(*value),
                Ct::F32 => Self::F32((*value as f32 * if *signed { -1.0 } else { 1.0 }).into()),
                Ct::F64 => Self::F64((*value as f64 * if *signed { -1.0 } else { 1.0 }).into()),
                _ => internal_error!("Const::Integer[{}]", ty)?,
            },
            Const::FPNumber(ty, value) => match ty {
                Ct::F32 => Self::F32((value.into_inner() as f32).into()),
                Ct::F64 => Self::F64(*value),
                _ => internal_error!("Const::FPNumber[{}]", ty)?,
            },
            Const::String(s) => Self::String(s.clone()),
            Const::Char(c) => Self::char(*c),
            Const::SyntaxSexp(_, s) => Self::from_syntax_sexp(s.as_ref().clone()),
            Const::Unit => Self::Unit,
        })
    }

    pub fn into_bool(self) -> Result<bool> {
        match self {
            Self::U(1) => Ok(true),
            Self::U(0) => Ok(false),
            v => internal_error!("Value::into_bool({})", v),
        }
    }

    pub fn into_string(self) -> Result<String> {
        match self {
            Self::String(s) => Ok(s),
            v => internal_error!("Value::into_string({})", v),
        }
    }

    pub fn into_clos(self) -> Result<(CtId, Option<Box<Value>>)> {
        match self {
            Self::Clos(id, env) => Ok((id, env)),
            v => internal_error!("Value::into_clos({})", v),
        }
    }

    pub fn into_env(self) -> Result<Vec<Value>> {
        match self {
            Self::Env(elems) => Ok(elems),
            v => internal_error!("Value::into_env({})", v),
        }
    }

    pub fn load(self) -> Result<Self> {
        match self {
            Self::Box(v) => Ok(v.lock().unwrap().clone()),
            v => internal_error!("Value::load({})", v),
        }
    }

    pub fn struct_elem(self, index: usize) -> Result<Self> {
        match self {
            Self::Struct(mut fields) => Ok(fields.remove(index)),
            v => internal_error!("Value::struct_elem({})", v),
        }
    }

    pub fn syntax_body(self) -> Result<Self> {
        match self {
            Self::Syntax(_, body) => Ok(*body),
            v => internal_error!("Value::syntax_body({})", v),
        }
    }

    pub fn set(self, other: Self) -> Result<Self> {
        if let Self::Box(ref v) = self {
            *v.lock().unwrap() = other;
            Ok(Self::Unit)
        } else {
            internal_error!("Value::set({}, {})", self, other)
        }
    }

    pub fn equal(self, other: Self) -> Result<bool> {
        Ok(match (self, other) {
            (Self::S(a), Self::S(b)) => a == b,
            (Self::U(a), Self::U(b)) => a == b,
            (Self::F32(a), Self::F32(b)) => a == b,
            (Self::F64(a), Self::F64(b)) => a == b,
            (Self::String(a), Self::String(b)) => a == b,
            (Self::Unit, Self::Unit) => true,
            (a, b) => internal_error!("Value::equal({}, {})", a, b)?,
        })
    }

    pub fn compare(self, other: Self) -> Result<Ordering> {
        Ok(match (self, other) {
            (Self::S(a), Self::S(b)) => a.cmp(&b),
            (Self::U(a), Self::U(b)) => a.cmp(&b),
            (Self::F32(a), Self::F32(b)) => a.cmp(&b),
            (Self::F64(a), Self::F64(b)) => a.cmp(&b),
            (Self::String(a), Self::String(b)) => a.cmp(&b),
            (Self::Unit, Self::Unit) => Ordering::Equal,
            (a, b) => internal_error!("Value::compare({}, {})", a, b)?,
        })
    }

    #[allow(clippy::should_implement_trait)]
    pub fn add(self, other: Self) -> Result<Self> {
        Ok(match (self, other) {
            (Self::S(a), Self::S(b)) => Self::S(a + b),
            (Self::U(a), Self::U(b)) => Self::U(a.wrapping_add(b)),
            (Self::F32(a), Self::F32(b)) => Self::F32(a + b),
            (Self::F64(a), Self::F64(b)) => Self::F64(a + b),
            (a, b) => internal_error!("Value::add({}, {})", a, b)?,
        })
    }

    #[allow(clippy::should_implement_trait)]
    pub fn sub(self, other: Self) -> Result<Self> {
        Ok(match (self, other) {
            (Self::S(a), Self::S(b)) => Self::S(a - b),
            (Self::U(a), Self::U(b)) => Self::U(a.wrapping_sub(b)),
            (Self::F32(a), Self::F32(b)) => Self::F32(a - b),
            (Self::F64(a), Self::F64(b)) => Self::F64(a - b),
            (a, b) => internal_error!("Value::sub({}, {})", a, b)?,
        })
    }

    #[allow(clippy::should_implement_trait)]
    pub fn mul(self, other: Self) -> Result<Self> {
        Ok(match (self, other) {
            (Self::S(a), Self::S(b)) => Self::S(a * b),
            (Self::U(a), Self::U(b)) => Self::U(a.wrapping_mul(b)),
            (Self::F32(a), Self::F32(b)) => Self::F32(a * b),
            (Self::F64(a), Self::F64(b)) => Self::F64(a * b),
            (a, b) => internal_error!("Value::mul({}, {})", a, b)?,
        })
    }

    #[allow(clippy::should_implement_trait)]
    pub fn div(self, other: Self) -> Result<Self> {
        Ok(match (self, other) {
            (Self::S(a), Self::S(b)) => Self::S(a / b),
            (Self::U(a), Self::U(b)) => Self::U(a.wrapping_div(b)),
            (Self::F32(a), Self::F32(b)) => Self::F32(a / b),
            (Self::F64(a), Self::F64(b)) => Self::F64(a / b),
            (a, b) => internal_error!("Value::div({}, {})", a, b)?,
        })
    }

    #[allow(clippy::should_implement_trait)]
    pub fn rem(self, other: Self) -> Result<Self> {
        Ok(match (self, other) {
            (Self::S(a), Self::S(b)) => Self::S(a % b),
            (Self::U(a), Self::U(b)) => Self::U(a.wrapping_rem(b)),
            (Self::F32(a), Self::F32(b)) => Self::F32(a % b),
            (Self::F64(a), Self::F64(b)) => Self::F64(a % b),
            (a, b) => internal_error!("Value::rem({}, {})", a, b)?,
        })
    }

    pub fn concat(self, other: Self) -> Result<Self> {
        Ok(match (self, other) {
            (Self::String(a), Self::String(b)) => Self::String(a + &b),
            (a, b) => internal_error!("Value::concat({}, {})", a, b)?,
        })
    }

    pub fn from_syntax_sexp(sexp: Syntax<Sexp>) -> Self {
        let (meta, body) = SyntaxBody::unpack(sexp);
        let (tag, body) = match body {
            Sexp::Integer(s, v) => (0, vec![Self::bool(s), Self::U(v)]),
            Sexp::FPNumber(value) => (1, vec![Self::F64(value)]),
            Sexp::Bool(value) => (2, vec![Self::bool(value)]),
            Sexp::Symbol(s) => (3, vec![Self::String(s)]),
            Sexp::String(s) => (4, vec![Self::String(s)]),
            Sexp::Char(c) => (5, vec![Self::char(c)]),
            Sexp::Cons(car, cdr) => (
                6,
                vec![Self::from_syntax_sexp(car), Self::from_syntax_sexp(cdr)],
            ),
            Sexp::Nil => (7, vec![]),
            Sexp::Use(use_) => (8, vec![Self::CapturedUse(use_)]),
        };

        Self::Syntax(
            meta,
            Box::new(Self::Struct(vec![Self::U(tag), Self::Struct(body)])),
        )
    }

    pub fn into_syntax_sexp(self) -> Result<Syntax<Sexp>> {
        if_chain! {
            if let Self::Syntax(meta, body) = self;
            if let Self::Struct(mut fields) = *body;
            if let [tag, body] = fields.as_mut_slice();
            if let Self::U(tag) = std::mem::take(tag);
            if let Self::Struct(mut body) = std::mem::take(body);

            if let Some(body) = match (tag, body.as_mut_slice()) {
                (0, [Self::U(signed), Self::U(value)]) => {
                    Some(Sexp::Integer(*signed == 1, *value))
                }
                (1, [Self::F64(value)]) => Some(Sexp::FPNumber(*value)),
                (2, [Self::U(value)]) => Some(Sexp::Bool(*value == 1)),
                (3, [Self::String(s)]) => Some(Sexp::Symbol(std::mem::take(s))),
                (4, [Self::String(s)]) => Some(Sexp::String(std::mem::take(s))),
                (5, [Self::U(c)]) => Some(Sexp::Char(char::try_from(*c as u32).unwrap())),
                (6, [car, cdr]) => {
                    let car = std::mem::take(car).into_syntax_sexp()?;
                    let cdr = std::mem::take(cdr).into_syntax_sexp()?;
                    Some(Sexp::Cons(car, cdr))
                }
                (7, []) => Some(Sexp::Nil),
                (8, [Self::CapturedUse(use_)]) => Some(Sexp::Use(*use_)),
                _ => None,
            };

            then { Ok(body.pack(meta)) }
            else { internal_error!("Value::into_syntax_sexp(...)") }
        }
    }

    pub fn into_result_syntax_sexp_string(
        self,
    ) -> Result<std::result::Result<Syntax<Sexp>, String>> {
        if_chain! {
            if let Self::Struct(mut fields) = self;
            if let [tag, body] = fields.as_mut_slice();
            if let Self::U(tag) = std::mem::take(tag);
            if let Self::Struct(mut body) = std::mem::take(body);

            if let Some(result) = match (tag, body.as_mut_slice()) {
                (0, [Self::String(error)]) => Some(Err(std::mem::take(error))),
                (1, [syntax]) => std::mem::take(syntax).into_syntax_sexp().ok().map(Ok),
                _ => None,
            };

            then { Ok(result) }
            else { internal_error!("Value::into_result_syntax_sexp_string") }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source_loc::SourceLocator;

    macro_rules! assert_sexp_value {
        ($src:expr) => {
            assert_eq!(
                Value::from_syntax_sexp($src)
                    .into_syntax_sexp()
                    .map_err(|e| e.to_string()),
                Ok($src)
            );
        };
    }

    #[test]
    fn test_sexp() {
        let l = SourceLocator::temporary().issue(Default::default());
        assert_sexp_value!(Syntax::<Sexp>::unsigned(l, 0));
        assert_sexp_value!(Syntax::<Sexp>::unsigned(l, 123));
        assert_sexp_value!(Syntax::<Sexp>::unsigned(l, 10000000000000000000));
        assert_sexp_value!(Syntax::<Sexp>::signed(l, -1000000000000000000));
        assert_sexp_value!(Syntax::<Sexp>::fpnumber(l, 3.14.into()));
        assert_sexp_value!(Syntax::<Sexp>::symbol(l, "hello"));
        assert_sexp_value!(Syntax::<Sexp>::string(l, "Hello, World!"));
        assert_sexp_value!(Syntax::<Sexp>::bool(l, true));
        assert_sexp_value!(Syntax::<Sexp>::bool(l, false));
        assert_sexp_value!(Syntax::<Sexp>::list(
            l,
            vec![
                Syntax::<Sexp>::signed(l, 123),
                Syntax::<Sexp>::bool(l, true),
                Syntax::<Sexp>::string(l, "string")
            ]
        ));
        assert_sexp_value!(Syntax::<Sexp>::list_like(
            l,
            Syntax::<Sexp>::signed(l, 123),
            vec![
                Syntax::<Sexp>::bool(l, true),
                Syntax::<Sexp>::bool(l, false)
            ],
            Syntax::<Sexp>::string(l, "string")
        ));
    }
}
