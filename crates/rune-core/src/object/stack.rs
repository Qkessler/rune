use std::ops::{Deref, DerefMut, Index, IndexMut, RangeTo};

use crate::gc::{Context, Rt, Trace};
use crate::object::{nil, GcObj};

#[derive(Debug, Default)]
#[repr(transparent)]
pub struct LispStack(Vec<GcObj<'static>>);

impl Deref for Rt<LispStack> {
    type Target = Rt<Vec<GcObj<'static>>>;

    fn deref(&self) -> &Self::Target {
        unsafe { &*(self as *const Self).cast::<Self::Target>() }
    }
}

impl DerefMut for Rt<LispStack> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *(self as *mut Self).cast::<Self::Target>() }
    }
}

// To make this simpler we implement indexing from the top of the stack (end of
// the vec) instead of the bottom. This is the convention that all the bytecode
// functions use.
impl Index<usize> for Rt<LispStack> {
    type Output = Rt<GcObj<'static>>;

    fn index(&self, index: usize) -> &Self::Output {
        let index = self.offset_end(index);
        let vec: &[Rt<GcObj>] = self;
        &vec[index]
    }
}

// This impl is specifically for the Stack. It takes the index from the end of
// the vector instead of the start. This matches how the lisp stack behaves.
impl IndexMut<usize> for Rt<LispStack> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        let index = self.offset_end(index);
        let vec = unsafe { &mut *(self as *mut Self).cast::<Vec<Rt<GcObj>>>() };
        &mut vec[index]
    }
}

// This impl is specifically for the Stack. It takes the range from the end of
// the vector instead of the start. This matches how the lisp stack behaves.
impl Index<RangeTo<usize>> for Rt<LispStack> {
    type Output = [Rt<GcObj<'static>>];

    fn index(&self, index: RangeTo<usize>) -> &Self::Output {
        debug_assert!(index.end <= self.len());
        let end = self.len() - index.end;
        let vec: &[Rt<GcObj>] = self;
        &vec[end..]
    }
}

impl LispStack {
    pub fn from_root<'brw>(value: &'brw mut Rt<Vec<GcObj>>) -> &'brw mut Rt<LispStack> {
        unsafe { &mut *((value as *mut Rt<Vec<GcObj>>).cast::<Rt<LispStack>>()) }
    }
}

impl Trace for LispStack {
    fn trace(&self, stack: &mut Vec<rune_core::object::RawObj>) {
        self.0.trace(stack);
    }
}

impl Rt<LispStack> {
    pub fn pop<'ob>(&mut self, cx: &'ob Context) -> GcObj<'ob> {
        self.bind_mut(cx).pop().unwrap()
    }

    pub fn top(&mut self) -> &mut Rt<GcObj<'static>> {
        self.last_mut().unwrap()
    }
}

impl Rt<LispStack> {
    pub fn offset_end(&self, i: usize) -> usize {
        debug_assert!(i < self.len());
        self.len() - (i + 1)
    }

    pub fn push_ref(&mut self, i: impl Into<i32>, cx: &Context) {
        let obj = self[i.into() as usize].bind(cx);
        self.push(obj);
    }

    pub fn set_ref(&mut self, i: impl Into<usize>) {
        let index = self.offset_end(i.into());
        self.swap_remove(index);
    }

    pub fn fill_extra_args(&mut self, fill_args: u16) {
        for _ in 0..fill_args {
            self.push(nil());
        }
    }

    pub fn remove_top(&mut self, i: impl Into<usize>) {
        let offset = self.offset_end(i.into());
        self.truncate(offset + 1);
    }
}
