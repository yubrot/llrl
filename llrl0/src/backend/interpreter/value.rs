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
    S(Ct, i64),
    U(Ct, u64),
    F32(OrderedFloat<f32>),
    F64(OrderedFloat<f64>),
    String(String),
    Char(char),
    CapturedUse(CapturedUse),
    Unit,
    Uninitialized(Ct),
    Clos(CtId, Option<Box<Value>>),
    Env(Vec<Value>),
    Struct(Ct, Vec<Value>),
    Reinterpret(Ct, Ct, Box<Value>),
    Syntax(SyntaxMetadata, Box<Value>),
    Box(Arc<Mutex<Value>>),
}

impl Value {
    pub fn exclude_uninitialized_as_ub(&self, name: &str) -> Result<()> {
        match self {
            Self::Uninitialized(_) => Err(Error::UndefinedBehavior(format!(
                "Uninitialized value appeared in {}",
                name
            ))),
            _ => Ok(()),
        }
    }

    pub fn signed(size: usize, value: i64) -> Self {
        // TODO: Overflow check
        Self::S(Ct::S(size), value)
    }

    pub fn unsigned(size: usize, value: u64) -> Self {
        // TODO: Overflow check
        Self::U(Ct::U(size), value)
    }

    pub fn bool(value: bool) -> Self {
        Self::U(Ct::U(1), if value { 1 } else { 0 })
    }

    pub fn alloc(_: Location, value: Self) -> Self {
        // NOTE: Should we simulate stack allocation to detect dangling pointer?
        Self::Box(Arc::new(Mutex::new(value)))
    }

    pub fn construct_env(_: Location, elems: Vec<Self>) -> Self {
        // NOTE: Should we simulate stack allocation to detect dangling pointer?
        Self::Env(elems)
    }

    pub fn construct_syntax(meta: SyntaxMetadata, body: Self) -> Self {
        Self::Syntax(meta, Box::new(body))
    }

    pub fn from_const(c: &Const) -> Result<Self> {
        match c {
            Const::Integer(ty, signed, value) => Ok(match ty {
                Ct::S(size) => Self::signed(*size, *value as i64),
                Ct::U(size) if !*signed || 0 <= *value as i64 => Self::unsigned(*size, *value),
                Ct::F32 => Self::F32((*value as f32 * if *signed { -1.0 } else { 1.0 }).into()),
                Ct::F64 => Self::F64((*value as f64 * if *signed { -1.0 } else { 1.0 }).into()),
                _ => Err(Error::Internal(format!("Const::Number[{}]", ty)))?,
            }),
            Const::FPNumber(ty, value) => Ok(match ty {
                Ct::F32 => Self::F32((value.into_inner() as f32).into()),
                Ct::F64 => Self::F64(*value),
                _ => Err(Error::Internal(format!("Const::FPNumber[{}]", ty)))?,
            }),
            Const::String(s) => Ok(Self::String(s.clone())),
            Const::Char(c) => Ok(Self::Char(*c)),
            Const::SyntaxSexp(_, s) => Ok(Self::from_syntax_sexp(s.as_ref().clone())),
            Const::Unit => Ok(Self::Unit),
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Self::U(Ct::U(1), v) => Some(*v == 1),
            _ => None,
        }
    }

    pub fn test(&self) -> Result<bool> {
        self.exclude_uninitialized_as_ub("Value::test")?;

        match self.as_bool() {
            Some(v) => Ok(v),
            None => Err(Error::Internal(format!("Value::test({})", self))),
        }
    }

    pub fn load(self) -> Result<Self> {
        self.exclude_uninitialized_as_ub("Value::load")?;

        match self {
            Self::Box(v) => Ok(v.lock().unwrap().clone()),
            v => Err(Error::Internal(format!("Value::load({})", v))),
        }
    }

    pub fn struct_elem(self, ty: &Ct, index: usize) -> Result<Self> {
        self.exclude_uninitialized_as_ub("Value::struct_elem")?;

        match self {
            Self::Struct(sty, mut fields) => {
                if !ty.is_compatible_with(&sty) {
                    Err(Error::Internal(format!(
                        "Value::struct_elem[{}]({})",
                        sty, ty
                    )))?;
                }
                Ok(fields.remove(index))
            }
            v => Err(Error::Internal(format!("Value::struct_elem({})", v))),
        }
    }

    pub fn reinterpret(self, a: &Ct, b: &Ct) -> Result<Self> {
        self.exclude_uninitialized_as_ub("Value::reinterpret")?;

        match self {
            Self::Reinterpret(b2, a2, v) if a2.is_compatible_with(a) => {
                if b2.is_compatible_with(b) {
                    Ok(*v)
                } else {
                    Ok(Self::Reinterpret(b2, b.clone(), v))
                }
            }
            v => Ok(Self::Reinterpret(a.clone(), b.clone(), Box::new(v))),
        }
    }

    pub fn syntax_body(self) -> Result<Self> {
        self.exclude_uninitialized_as_ub("Value::syntax_body")?;

        match self {
            Self::Syntax(_, body) => Ok(*body),
            v => Err(Error::Internal(format!("Value::syntax_body({})", v))),
        }
    }

    pub fn set(self, other: Self) -> Result<Self> {
        self.exclude_uninitialized_as_ub("Value::set")?;

        if let Self::Box(v) = self {
            *v.lock().unwrap() = other;
        } else {
            Err(Error::Internal(format!("Value::set({}, {})", self, other)))?;
        }

        Ok(Self::Unit)
    }

    pub fn equal(&self, other: &Self) -> Result<bool> {
        self.exclude_uninitialized_as_ub("Value::equal")?;
        other.exclude_uninitialized_as_ub("Value::equal")?;

        use Value::*;
        match (self, other) {
            (S(at, a), S(bt, b)) if at.is_compatible_with(bt) => Ok(a == b),
            (U(at, a), U(bt, b)) if at.is_compatible_with(bt) => Ok(a == b),
            (F32(a), F32(b)) => Ok(a == b),
            (F64(a), F64(b)) => Ok(a == b),
            (String(a), String(b)) => Ok(a == b),
            (Char(a), Char(b)) => Ok(a == b),
            (Unit, Unit) => Ok(true),
            (a, b) => Err(Error::Internal(format!("Value::equal({}, {})", a, b))),
        }
    }

    pub fn compare(&self, other: &Self) -> Result<Ordering> {
        self.exclude_uninitialized_as_ub("Value::compare")?;
        other.exclude_uninitialized_as_ub("Value::compare")?;

        use Value::*;
        match (self, other) {
            (S(at, a), S(bt, b)) if at.is_compatible_with(bt) => Ok(a.cmp(b)),
            (U(at, a), U(bt, b)) if at.is_compatible_with(bt) => Ok(a.cmp(b)),
            (F32(a), F32(b)) => Ok(a.cmp(b)),
            (F64(a), F64(b)) => Ok(a.cmp(b)),
            (String(a), String(b)) => Ok(a.cmp(b)),
            (Unit, Unit) => Ok(Ordering::Equal),
            (a, b) => Err(Error::Internal(format!("Value::compare({}, {})", a, b))),
        }
    }

    pub fn add(self, other: Self) -> Result<Self> {
        self.exclude_uninitialized_as_ub("Value::add")?;
        other.exclude_uninitialized_as_ub("Value::add")?;

        use Value::*;
        match (self, other) {
            (S(at, a), S(bt, b)) if at.is_compatible_with(&bt) => Ok(S(at, a + b)),
            (U(at, a), U(bt, b)) if at.is_compatible_with(&bt) => Ok(U(at, a.wrapping_add(b))),
            (F32(a), F32(b)) => Ok(F32(a + b)),
            (F64(a), F64(b)) => Ok(F64(a + b)),
            (a, b) => Err(Error::Internal(format!("Value::add({}, {})", a, b))),
        }
    }

    pub fn sub(self, other: Self) -> Result<Self> {
        self.exclude_uninitialized_as_ub("Value::sub")?;
        other.exclude_uninitialized_as_ub("Value::sub")?;

        use Value::*;
        match (self, other) {
            (S(at, a), S(bt, b)) if at.is_compatible_with(&bt) => Ok(S(at, a - b)),
            (U(at, a), U(bt, b)) if at.is_compatible_with(&bt) => Ok(U(at, a.wrapping_sub(b))),
            (F32(a), F32(b)) => Ok(F32(a - b)),
            (F64(a), F64(b)) => Ok(F64(a - b)),
            (a, b) => Err(Error::Internal(format!("Value::sub({}, {})", a, b))),
        }
    }

    pub fn mul(self, other: Self) -> Result<Self> {
        self.exclude_uninitialized_as_ub("Value::mul")?;
        other.exclude_uninitialized_as_ub("Value::mul")?;

        use Value::*;
        match (self, other) {
            (S(at, a), S(bt, b)) if at.is_compatible_with(&bt) => Ok(S(at, a * b)),
            (U(at, a), U(bt, b)) if at.is_compatible_with(&bt) => Ok(U(at, a.wrapping_mul(b))),
            (F32(a), F32(b)) => Ok(F32(a * b)),
            (F64(a), F64(b)) => Ok(F64(a * b)),
            (a, b) => Err(Error::Internal(format!("Value::mul({}, {})", a, b))),
        }
    }

    pub fn div(self, other: Self) -> Result<Self> {
        self.exclude_uninitialized_as_ub("Value::div")?;
        other.exclude_uninitialized_as_ub("Value::div")?;

        use Value::*;
        match (self, other) {
            (S(at, a), S(bt, b)) if at.is_compatible_with(&bt) => Ok(S(at, a / b)),
            (U(at, a), U(bt, b)) if at.is_compatible_with(&bt) => Ok(U(at, a.wrapping_div(b))),
            (F32(a), F32(b)) => Ok(F32(a / b)),
            (F64(a), F64(b)) => Ok(F64(a / b)),
            (a, b) => Err(Error::Internal(format!("Value::div({}, {})", a, b))),
        }
    }

    pub fn rem(self, other: Self) -> Result<Self> {
        self.exclude_uninitialized_as_ub("Value::rem")?;
        other.exclude_uninitialized_as_ub("Value::rem")?;

        use Value::*;
        match (self, other) {
            (S(at, a), S(bt, b)) if at.is_compatible_with(&bt) => Ok(S(at, a % b)),
            (U(at, a), U(bt, b)) if at.is_compatible_with(&bt) => Ok(U(at, a.wrapping_rem(b))),
            (F32(a), F32(b)) => Ok(F32(a % b)),
            (F64(a), F64(b)) => Ok(F64(a % b)),
            (a, b) => Err(Error::Internal(format!("Value::rem({}, {})", a, b))),
        }
    }

    pub fn concat(self, other: Self) -> Result<Self> {
        self.exclude_uninitialized_as_ub("Value::concat")?;
        other.exclude_uninitialized_as_ub("Value::concat")?;

        use Value::*;
        match (self, other) {
            (String(a), String(b)) => Ok(String(a + &b)),
            (a, b) => Err(Error::Internal(format!("Value::concat({}, {})", a, b))),
        }
    }

    pub fn from_syntax_sexp(sexp: Syntax<Sexp>) -> Self {
        let (meta, body) = SyntaxBody::unpack(sexp.clone());
        let (tag, body) = match body {
            Sexp::Integer(s, v) => (0, vec![Self::bool(s), Self::unsigned(64, v)]),
            Sexp::FPNumber(value) => (1, vec![Self::F64(value)]),
            Sexp::Bool(value) => (2, vec![Self::bool(value)]),
            Sexp::Symbol(s) => (3, vec![Self::String(s.to_string())]),
            Sexp::String(s) => (4, vec![Self::String(s.to_string())]),
            Sexp::Char(c) => (5, vec![Self::Char(c)]),
            Sexp::Cons(car, cdr) => (
                6,
                vec![Self::from_syntax_sexp(car), Self::from_syntax_sexp(cdr)],
            ),
            Sexp::Nil => (7, vec![]),
            Sexp::Use(use_) => (8, vec![Self::CapturedUse(use_)]),
        };

        Self::construct_syntax(
            meta,
            Self::Struct(
                Ct::Hole,
                vec![
                    Self::unsigned(4, tag),
                    Self::Reinterpret(Ct::Hole, Ct::Hole, Box::new(Self::Struct(Ct::Hole, body))),
                ],
            ),
        )
    }

    pub fn into_syntax_sexp(self) -> Result<Syntax<Sexp>> {
        self.exclude_uninitialized_as_ub("Value::into_syntax_sexp")?;

        if_chain! {
            if let Self::Syntax(meta, body) = self;
            if let Self::Struct(_, mut fields) = *body;
            if let [tag, body] = fields.as_mut_slice();

            if let Self::U(Ct::U(4), tag) = std::mem::take(tag);

            if let Self::Reinterpret(_, _, body) = std::mem::take(body);
            if let Self::Struct(_, mut body) = *body;

            if let Some(body) = match (tag, body.as_mut_slice()) {
                (0, [Self::U(Ct::U(1), signed), Self::U(Ct::U(64), value)]) => {
                    Some(Sexp::Integer(*signed == 1, *value))
                }
                (1, [Self::F64(value)]) => Some(Sexp::FPNumber(*value)),
                (2, [Self::U(Ct::U(1), value)]) => Some(Sexp::Bool(*value == 1)),
                (3, [Self::String(s)]) => Some(Sexp::Symbol(std::mem::take(s))),
                (4, [Self::String(s)]) => Some(Sexp::String(std::mem::take(s))),
                (5, [Self::Char(c)]) => Some(Sexp::Char(*c)),
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
            else { Err(Error::Internal("Value::into_syntax_sexp(...)".to_string())) }
        }
    }

    pub fn into_result_syntax_sexp_string(
        self,
    ) -> Result<std::result::Result<Syntax<Sexp>, String>> {
        if_chain! {
            if let Self::Struct(_, mut fields) = self;
            if let [tag, body] = fields.as_mut_slice();

            if let Self::U(Ct::U(1), tag) = std::mem::take(tag);

            if let Self::Reinterpret(_, _, body) = std::mem::take(body);
            if let Self::Struct(_, mut body) = *body;

            if let Some(result) = match (tag, body.as_mut_slice()) {
                (0, [Self::String(error)]) => Some(Err(std::mem::take(error))),
                (1, [syntax]) => std::mem::take(syntax).into_syntax_sexp().ok().map(Ok),
                _ => None,
            };

            then { Ok(result) }
            else { Err(Error::Internal("Value::into_result_syntax_sexp_string".to_string())) }
        }
    }
}

impl Default for Value {
    fn default() -> Self {
        Self::Unit
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::S(_, value) => write!(f, "{}", value),
            Self::U(Ct::U(1), 0) => write!(f, "#f"),
            Self::U(Ct::U(1), 1) => write!(f, "#t"),
            Self::U(_, value) => write!(f, "{}", value),
            Self::F32(value) => write!(f, "{}", value),
            Self::F64(value) => write!(f, "{}", value),
            Self::String(s) => write!(f, "\"{}\"", string::escape(s)),
            Self::Char(c) => write!(f, "#\\{}", string::escape(&c.to_string())),
            Self::CapturedUse(use_) => write!(f, "{}", use_),
            Self::Unit => write!(f, "unit"),
            Self::Uninitialized(_) => write!(f, "uninitialized"),
            Self::Clos(id, None) => write!(f, "closure({}, none)", id),
            Self::Clos(id, Some(env)) => write!(f, "closure({}, {})", id, env),
            Self::Env(values) => write!(f, "env({})", values.iter().format(", ")),
            Self::Syntax(_, body) => write!(f, "syntax({})", body),
            Self::Struct(_, fields) => write!(f, "{{{}}}", fields.iter().format(", ")),
            Self::Reinterpret(_, to, body) => write!(f, "[{} {}]", to, body),
            Self::Box(v) => write!(f, "box({})", v.lock().unwrap()),
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
