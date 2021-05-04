use std::borrow::Borrow;
use std::fmt;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};

/// An opaque pointer association.
///
/// This trait is unsafe because the trait implementer must ensure that
/// `Self` is not instantiatable since `Self` must be used only for type checking.
pub unsafe trait Opaque {
    type Type;

    fn open(a: *mut Self) -> *mut Self::Type;

    fn close(a: *mut Self::Type) -> *mut Self;

    /// Treat `ptr` as a borrowed opaque pointer.
    ///
    /// # Safety
    /// This is marked as unsafe because the caller must ensure that
    /// `ptr` is valid and `'a` outlives `ptr` and borrowing rules are satisfied.
    unsafe fn from_ptr<'a>(ptr: *mut Self::Type) -> &'a Self {
        &*Self::close(ptr)
    }

    /// Treat `ptr` as a mutably borrowed opaque pointer.
    ///
    /// # Safety
    /// This is marked as unsafe because the caller must ensure that
    /// `ptr` is valid and `'a` outlives `ptr` and borrowing rules are satisfied.
    unsafe fn from_ptr_mut<'a>(ptr: *mut Self::Type) -> &'a mut Self {
        &mut *Self::close(ptr)
    }

    fn as_ptr(&self) -> *mut Self::Type {
        Self::open(self as *const Self as *mut Self)
    }
}

/// Implement `Opaque` trait by simple casting.
macro_rules! unsafe_impl_opaque {
    ($ty:ident($pty:ident)) => {
        unsafe_impl_opaque!([] $ty($pty));
    };

    ([$( $param:tt ),*] $ty:ident($pty:ident)) => {
        unsafe impl<$( $param ),*> Opaque for $ty<$( $param ),*> {
            type Type = $pty;

            fn open(a: *mut Self) -> *mut Self::Type {
                a as *mut Self::Type
            }

            fn close(a: *mut Self::Type) -> *mut Self {
                a as *mut Self
            }
        }
    };
}

/// Implement `Eq` and `PartialEq` trait by comparison of internal pointers.
macro_rules! impl_opaque_eq {
    ($ty:ident) => {
        impl_opaque_eq!([] $ty);
    };

    ([$( $param:tt ),*] $ty:ident) => {
        impl<$( $param ),*> PartialEq for $ty<$( $param ),*> {
            fn eq(&self, other: &Self) -> bool {
                self.as_ptr() == other.as_ptr()
            }
        }

        impl<$( $param ),*> Eq for $ty<$( $param ),*> {}
    };
}

/// Implement `Debug` trait. `Debug::fmt` simply writes an internal pointer address.
macro_rules! impl_opaque_debug {
    ($ty:ident) => {
        impl_opaque_debug!([] $ty);
    };

    ([$( $param:tt ),*] $ty:ident) => {
        impl<$( $param ),*> std::fmt::Debug for $ty<$( $param ),*> {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(
                    f,
                    "{}::from_ptr({:?})",
                    std::stringify!($ty),
                    self.as_ptr()
                )
            }
        }
    };
}

/// An opaque pointer that can be owned by Rust.
pub trait OpaqueOwn: Opaque {
    /// Executes the destructor for the opaque pointer.
    #[allow(clippy::missing_safety_doc)]
    unsafe fn drop(_a: *mut Self::Type);
}

/// An owned opaque pointer.
#[derive(Eq, PartialEq, Ord, PartialOrd)]
pub struct Oo<T: OpaqueOwn + ?Sized> {
    ptr: *mut T::Type,
    owns: PhantomData<T>,
}

impl<T: OpaqueOwn + ?Sized> Oo<T> {
    /// Treat `ptr` as an owned opaque pointer.
    ///
    /// # Safety
    /// This is marked as unsafe because the caller must ensure
    /// that the ownership of `ptr` is transferred.
    pub unsafe fn from_ptr(ptr: *mut T::Type) -> Self {
        Oo {
            ptr,
            owns: PhantomData,
        }
    }

    /// Consumes the `OO`, returning an owned opaque pointer.
    pub fn into_ptr(oo: Self) -> *mut T::Type {
        let ptr = oo.ptr;
        std::mem::forget(oo);
        ptr
    }

    /// Consumes and leaks the `OO`, retunring a mutable reference.
    pub fn leak<'a>(oo: Self) -> &'a mut T {
        let r = unsafe { Opaque::from_ptr_mut(oo.ptr) };
        std::mem::forget(oo);
        r
    }
}

impl<T: OpaqueOwn + ?Sized> Drop for Oo<T> {
    fn drop(&mut self) {
        unsafe { <T as OpaqueOwn>::drop(self.ptr) };
    }
}

impl<T: OpaqueOwn + ?Sized> Deref for Oo<T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { Opaque::from_ptr(self.ptr) }
    }
}

impl<T: OpaqueOwn + ?Sized> DerefMut for Oo<T> {
    fn deref_mut(&mut self) -> &mut T {
        unsafe { Opaque::from_ptr_mut(self.ptr) }
    }
}

impl<T: OpaqueOwn + ?Sized> Borrow<T> for Oo<T> {
    fn borrow(&self) -> &T {
        self
    }
}

impl<T: OpaqueOwn + ToOwned<Owned = Oo<T>> + ?Sized> Clone for Oo<T> {
    fn clone(&self) -> Self {
        self.to_owned()
    }
}

impl<T: OpaqueOwn + ?Sized + fmt::Debug> fmt::Debug for Oo<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&**self, f)
    }
}

impl<T: OpaqueOwn + ?Sized + fmt::Display> fmt::Display for Oo<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&**self, f)
    }
}
