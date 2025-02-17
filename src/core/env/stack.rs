use crate::core::{
    gc::{Context, IntoRoot, Rt},
    object::{GcObj, NIL},
};
use rune_macros::Trace;
use std::ops::{Deref, DerefMut, Index, IndexMut, RangeBounds, RangeTo};

/// The stack of lisp objects used to pass and store arguments in the bytecode
/// VM and interpreter. The top of the stack is index 0 and all indexing
/// functions operate from top to bottom. The stack is partitioned into frames.
/// Each frame represents a function call and it's arguments. The API is
/// designed so that code cannot access elements outside of their frame (doing
/// so results in a panic). Frames are added and removed with
/// [push_frame](RootedLispStack::push_frame) and
/// [pop_frame](RootedLispStack::pop_frame) respectively.
#[derive(Debug, Default, Trace)]
pub(crate) struct LispStack<'a> {
    vec: Vec<GcObj<'a>>,
    #[no_trace]
    current: Frame,
    #[no_trace]
    frames: Vec<Frame>,
}

/// A function call frame. These mirror the lisp call stack and are used to
/// display backtraces as well as return.
#[derive(Debug, Clone, Copy)]
struct Frame {
    /// The start of the call frame, as a index from the start of the stack (not the end).
    start: usize,
    /// The maximum size this stack frame can grow.
    end: usize,
    /// Number of arguments in this call frame. The first is the count and the
    /// second is boolean indicating if the last argument is a cons cell with
    /// the remaining variadic arguments.
    arg_cnt: (u16, bool),
}

impl Default for Frame {
    fn default() -> Self {
        Self { start: 0, end: usize::MAX, arg_cnt: (0, false) }
    }
}

/// Type representing a slice of arguments on the stack. Used to avoid
/// allocations and copies when calling functions.
#[derive(Copy, Clone)]
pub(crate) struct ArgSlice(usize);

impl ArgSlice {
    pub(crate) fn new(size: usize) -> Self {
        Self(size)
    }

    pub(crate) fn len(&self) -> usize {
        self.0
    }
}

// To make this simpler we implement indexing from the top of the stack (end of
// the vec) instead of the bottom. This is the convention that all the bytecode
// functions use.
impl<'a> Index<usize> for RootedLispStack<'a> {
    type Output = Rt<GcObj<'a>>;

    fn index(&self, index: usize) -> &Self::Output {
        let index = self.offset_end(index);
        &self.vec[index]
    }
}

// This impl is specifically for the Stack. It takes the index from the end of
// the vector instead of the start. This matches how the lisp stack behaves.
impl IndexMut<usize> for RootedLispStack<'_> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        let index = self.offset_end(index);
        &mut self.vec[index]
    }
}

// This impl is specifically for the Stack. It takes the range from the end of
// the vector instead of the start. This matches how the lisp stack behaves.
impl<'a> Index<RangeTo<usize>> for RootedLispStack<'a> {
    type Output = [Rt<GcObj<'a>>];

    fn index(&self, index: RangeTo<usize>) -> &Self::Output {
        assert!(index.end <= self.len());
        let end = self.len() - index.end;
        let vec: &[Rt<GcObj>] = &self.vec;
        &vec[end..]
    }
}

impl<'a> RootedLispStack<'a> {
    pub(crate) fn push_bytecode_frame(&mut self, start: usize, depth: usize) {
        assert!(start <= self.len());
        assert!(self.current.start <= start);
        self.frames.push(self.current);
        let end = start + depth;
        // allocate space so that we don't have to reallocate later. This will
        // also let us do unchecked pushes later.
        if end > self.vec.capacity() {
            assert!(end - self.vec.len() < 100); // make sure this doesn't blow up
            self.vec.reserve(end - self.vec.len());
        }
        self.current = Frame { start, end, ..Frame::default() };
    }

    pub(crate) fn push_frame(&mut self) {
        let start = self.len();
        assert!(self.current.start <= start);
        self.frames.push(self.current);
        self.current = Frame { start, ..Frame::default() };
    }

    pub(crate) fn push_frame_with_args(&mut self, arg_cnt: usize) {
        assert!(arg_cnt <= self.len());
        let start = self.len() - arg_cnt;
        assert!(self.current.start <= start);
        self.frames.push(self.current);
        self.current =
            Frame { start, arg_cnt: (u16::try_from(arg_cnt).unwrap(), false), ..Frame::default() };
    }

    pub(crate) fn pop_frame(&mut self) {
        self.vec.truncate(self.current.start);
        self.current = self.frames.pop().unwrap();
    }

    pub(crate) fn return_frame(&mut self) {
        self.vec.swap_remove(self.current.start);
        self.vec.truncate(self.current.start + 1);
        self.current = self.frames.pop().unwrap();
    }

    pub(crate) fn current_frame(&self) -> usize {
        self.frames.len()
    }

    pub(crate) fn unwind_frames(&mut self, frame: usize) {
        if frame == self.current_frame() {
            return; /* no frames to unwind */
        }
        assert!(frame < self.current_frame());
        self.current = self.frames[frame];
        self.frames.truncate(frame);
    }

    pub(crate) fn len(&self) -> usize {
        self.vec.len()
    }

    pub(crate) fn set_depth(&mut self, depth: usize) {
        let end = self.current.start + depth;
        self.current.end = end;

        if end > self.vec.capacity() {
            assert!(end - self.vec.len() < 1000); // make sure this doesn't blow up if we have a bug
            self.vec.reserve(end - self.vec.len());
        }
    }

    pub(crate) fn set_arg_count(&mut self, arg_cnt: u16, rest: bool) {
        self.current.arg_cnt = (arg_cnt, rest);
    }

    pub(crate) fn push<T: IntoRoot<GcObj<'a>>>(&mut self, value: T) {
        if self.len() >= self.current.end {
            panic!(
                "overflowed max depth - len was {}, but limit was {}",
                self.len(),
                self.current.end
            );
        }
        // could use https://github.com/rust-lang/rust/issues/100486
        self.vec.push(value);
    }

    pub(crate) fn pop<'ob>(&mut self, cx: &'ob Context) -> GcObj<'ob> {
        assert!(self.len() > self.current.start);
        self.vec.bind_mut(cx).pop().unwrap()
    }

    pub(crate) fn top(&mut self) -> &mut Rt<GcObj<'a>> {
        assert!(self.len() > self.current.start);
        self.vec.last_mut().unwrap()
    }

    pub(crate) fn offset_end(&self, i: usize) -> usize {
        assert!(i < self.len());
        let from_end = self.len() - (i + 1);
        assert!(self.current.start <= from_end);
        from_end
    }

    pub(crate) fn push_ref(&mut self, i: impl Into<i32>, cx: &Context) {
        let obj = self[i.into() as usize].bind(cx);
        self.push(obj);
    }

    pub(crate) fn set_ref(&mut self, i: impl Into<usize>) {
        let index = self.offset_end(i.into());
        self.vec.swap_remove(index);
    }

    pub(crate) fn fill_extra_args(&mut self, fill_args: u16) {
        for _ in 0..fill_args {
            self.push(NIL);
        }
    }

    pub(crate) fn remove_top(&mut self, i: usize) {
        if i == 0 {
            return;
        }
        let offset = self.offset_end(i - 1);
        self.truncate(offset);
    }

    pub(crate) fn truncate(&mut self, len: usize) {
        self.vec.truncate(len);
    }

    pub(crate) fn extend_from_slice(&mut self, src: &[GcObj]) {
        self.vec.extend_from_slice(src);
    }

    // This indexing is backwards from the normal stack sematics, so we add
    // "as_vec" to the name to hopefully make it clearer
    pub(crate) fn extend_as_vec_from_within(&mut self, src: impl RangeBounds<usize>) {
        self.vec.extend_from_within(src);
    }

    pub(crate) fn frame_iter(&self) -> impl Iterator<Item = &Rt<GcObj>> {
        self.vec[self.current.start..].iter().rev()
    }

    pub(crate) fn arg_count(&self) -> usize {
        self.len() - self.current.start
    }

    pub(crate) fn current_args(&self) -> &[Rt<GcObj>] {
        // index as vec
        &self.vec[self.current.start..]
    }

    pub(crate) fn arg_slice(&self, arg_slice: ArgSlice) -> &[Rt<GcObj>] {
        // index as stack
        &self[..arg_slice.0]
    }
}

/// A function call Frame.
///
/// This is a guard type that will pop the frame when it
/// goes out of scope.
pub(crate) struct FnFrame<'brw, 'rt> {
    env: &'brw mut Rt<super::Env<'rt>>,
}

impl<'brw, 'rt> FnFrame<'brw, 'rt> {
    pub(crate) fn new(env: &'brw mut Rt<super::Env<'rt>>) -> Self {
        env.stack.push_frame();
        Self { env }
    }

    pub(crate) fn new_with_args(env: &'brw mut Rt<super::Env<'rt>>, args: usize) -> Self {
        env.stack.push_frame_with_args(args);
        Self { env }
    }

    pub(crate) fn set_depth(&mut self, depth: usize) {
        self.env.stack.set_depth(depth)
    }

    pub(crate) fn push_arg(&mut self, arg: impl IntoRoot<GcObj<'rt>>) {
        self.env.stack.push(arg);
    }

    /// Set the total argument count before a function call
    pub(crate) fn finalize_arguments(&mut self) {
        let args = self.env.stack.arg_count().try_into().unwrap();
        self.env.stack.set_arg_count(args, false);
    }

    pub(crate) fn arg_count(&self) -> usize {
        let count1 = self.env.stack.arg_count();
        let count2 = self.env.stack.current.arg_cnt.0 as usize;
        assert_eq!(count1, count2);
        count1
    }

    pub(crate) fn arg_slice(&self) -> &[Rt<GcObj<'rt>>] {
        &self.env.stack[..self.arg_count()]
    }

    pub(crate) fn push_arg_slice(&mut self, src: &[GcObj]) {
        self.env.stack.extend_from_slice(src);
    }
}

impl Drop for FnFrame<'_, '_> {
    fn drop(&mut self) {
        self.env.stack.pop_frame();
    }
}

impl<'rt> Deref for FnFrame<'_, 'rt> {
    type Target = Rt<super::Env<'rt>>;

    fn deref(&self) -> &Self::Target {
        self.env
    }
}

impl<'b> DerefMut for FnFrame<'_, 'b> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.env
    }
}
