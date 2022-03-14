use std::fmt;
use std::marker::PhantomData;
use std::ops::{Deref, Not};

use crate::arena::AllocPtr;

/// The inner data type that hold the value for an object variant. This type
/// should be no larger then 56 bits. The lowest bit of data is used to encode
/// the mutability flag: 1 if immutable, 0 if mutable. This should be stored in
/// the alignment bits that bottom of the pointer.
#[derive(Copy, Clone)]
pub(crate) struct Data<T> {
    data: [u8; 7],
    marker: PhantomData<T>,
}
pub(super) const UNUSED: Data<()> = Data::from_raw(0);

/// A trait to access the inner value of a [`Data`]
pub(crate) trait Inner {
    type Target;
    fn inner(self) -> Self::Target;
}

// We still need to determine when this is sound. Sending `Data<T>` across threads
// is not safe unless the values are copied with it. Maybe there is a better way
// to encode that in the type system.
unsafe impl<T> Send for Data<T> {}

impl<T> Data<T> {
    #[inline(always)]
    const fn into_raw(self) -> u64 {
        let x = self.data;
        let whole = [x[0], x[1], x[2], x[3], x[4], x[5], x[6], 0];
        u64::from_le_bytes(whole)
    }

    #[inline(always)]
    const fn from_raw(data: u64) -> Self {
        let x = data.to_le_bytes();
        // x[7] (the top byte) is removed to make room for the tag
        Data {
            data: [x[0], x[1], x[2], x[3], x[4], x[5], x[6]],
            marker: PhantomData,
        }
    }
}

impl<'a, T> Data<&'a T> {
    #[inline(always)]
    pub(super) fn from_ref(rf: &'a T) -> Self {
        let ptr: *const T = rf;
        Self::from_raw(ptr as u64)
    }
}

impl<'a, T> From<&'a T> for Data<&'a T> {
    fn from(x: &'a T) -> Self {
        Data::from_ref(x)
    }
}

impl<'a, T> Inner for Data<&'a T> {
    type Target = &'a T;
    #[inline(always)]
    fn inner(self) -> &'a T {
        let ptr = self.into_raw() as *const T;
        unsafe { &*ptr }
    }
}

impl Inner for Data<i64> {
    type Target = i64;
    #[inline(always)]
    fn inner(self) -> i64 {
        self.into_raw() as i64
    }
}

impl Data<i64> {
    pub(super) const fn from_int(data: i64) -> Self {
        Data::from_raw(data as u64)
    }
}

impl<'a, T> Data<AllocPtr<'a, T>> {
    pub(super) fn from_alloc(alloc: AllocPtr<'a, T>) -> Self {
        let raw = alloc.into_raw();
        Self::from_raw(raw)
    }
}

impl<'a, T: 'a> Inner for Data<AllocPtr<'a, T>> {
    type Target = &'a T;
    #[inline(always)]
    fn inner(self) -> &'a T {
        let alloc: AllocPtr<'a, T> = unsafe { AllocPtr::from_raw(self.into_raw()) };
        alloc.get()
    }
}

impl<'a, T> Deref for Data<AllocPtr<'a, T>>
where
    T: Copy,
{
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &'a Self::Target {
        self.inner()
    }
}

impl<T> Not for Data<T>
where
    Data<T>: Inner,
{
    type Output = <Self as Inner>::Target;

    #[inline(always)]
    fn not(self) -> Self::Output {
        self.inner()
    }
}

impl<T> PartialEq for Data<T>
where
    T: PartialEq + Copy,
    Data<T>: Inner,
    <Self as Inner>::Target: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.inner() == other.inner()
    }
}

impl PartialEq for Data<()> {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

impl<T> fmt::Display for Data<T>
where
    T: fmt::Display + Copy,
    Data<T>: Inner,
    <Self as Inner>::Target: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.inner(), f)
    }
}

impl<T> fmt::Debug for Data<T>
where
    T: fmt::Debug + Copy,
    Data<T>: Inner,
    <Self as Inner>::Target: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.inner(), f)
    }
}

impl<'a, T> Deref for Data<&'a T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &'a Self::Target {
        self.inner()
    }
}

impl<'a, T> AsRef<T> for Data<&'a T> {
    #[inline(always)]
    fn as_ref<'b>(&'b self) -> &'a T {
        self.inner()
    }
}
