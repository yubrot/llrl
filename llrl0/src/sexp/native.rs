use std::any::Any;
use std::cmp::Ordering;
use std::fmt;
use std::hash::{Hash, Hasher};

/// Dynamic values that can be embedded in S-expressions.
pub struct Native(Box<dyn NativeValue>);

impl Native {
    pub fn new(value: impl NativeValue) -> Self {
        Self(Box::new(value))
    }

    pub fn as_any(&self) -> &dyn Any {
        self.0.as_any()
    }

    pub fn downcast_ref<T: NativeValue>(&self) -> Option<&T> {
        self.as_any().downcast_ref()
    }
}

impl fmt::Debug for Native {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl fmt::Display for Native {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl Clone for Native {
    fn clone(&self) -> Self {
        Self(self.0.clone_dyn())
    }
}

impl Hash for Native {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash_dyn(state);
    }
}

impl PartialEq for Native {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq_dyn(other.as_any())
    }
}

impl Eq for Native {}

impl PartialOrd for Native {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Native {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.0.ord_dyn(other.as_any()) {
            Some(ordering) => ordering,
            None => self.as_any().type_id().cmp(&other.as_any().type_id()),
        }
    }
}

pub trait NativeValue: 'static + Send + Sync + fmt::Debug + fmt::Display {
    fn as_any(&self) -> &dyn Any;

    fn clone_dyn(&self) -> Box<dyn NativeValue>;

    fn hash_dyn(&self, state: &mut dyn Hasher);

    fn eq_dyn(&self, other: &dyn Any) -> bool;

    fn ord_dyn(&self, other: &dyn Any) -> Option<Ordering>;
}

impl<T: Any + Send + Sync + fmt::Debug + fmt::Display + Clone + Hash + Eq + Ord> NativeValue for T {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn clone_dyn(&self) -> Box<dyn NativeValue> {
        Box::new(self.clone())
    }

    fn hash_dyn(&self, mut state: &mut dyn Hasher) {
        self.hash(&mut state);
    }

    fn eq_dyn(&self, other: &dyn Any) -> bool {
        match other.downcast_ref() {
            Some(other) => self == other,
            None => false,
        }
    }

    fn ord_dyn(&self, other: &dyn Any) -> Option<Ordering> {
        other.downcast_ref().map(|other| self.cmp(other))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_native() {
        let a = Native::new(123);
        let b = Native::new(456);
        let c = Native::new(456);
        let d = Native::new("foo".to_string());

        assert_ne!(a, b);
        assert_eq!(b, c);
        assert_ne!(c, d);

        assert_eq!(a.cmp(&b), Ordering::Less);
        assert_eq!(b.cmp(&c), Ordering::Equal);
        assert_ne!(c.cmp(&d), Ordering::Equal);
        assert_ne!(c.cmp(&d), d.cmp(&c));

        let e = a.clone();
        assert_eq!(a, e);
        assert_ne!(b, e);

        assert_eq!(&a.to_string(), "123");
        assert_eq!(&b.to_string(), "456");
        assert_eq!(&d.to_string(), "foo");
    }
}
