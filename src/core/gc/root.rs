use super::super::{
    cons::Cons,
    object::{GcObj, RawObj},
};
use super::{Block, Context, RootSet, Trace};
use crate::core::object::{Gc, IntoObject, LispString, Object, Symbol, Untag, WithLifetime};
use rune_core::hashmap::{HashMap, HashSet};
use std::ops::{Deref, DerefMut, Index, IndexMut, RangeBounds};
use std::slice::SliceIndex;
use std::{
    collections::VecDeque,
    hash::{Hash, Hasher},
};
use std::{
    fmt::{Debug, Display},
    marker::PhantomPinned,
};

/// Helper trait to break the dependency between an object and the lifetimes of
/// it's [traceable](Trace) children. If This trait is implemented, than the
/// object can be traced by the garbage collector. Once it becomes rooted, it as
/// well as all of it's tracable children will be live until it is unrooted.
/// This essentially makes any lifetimes of a tracable objects meaningless. They
/// can be anything, including 'static. When an object is removed from a root it
/// will be given the proper lifetime again. Care must be taken to ensure that
/// any lifetimes that are changed only belong to traceable objects. Object can
/// contain lifetimes parameters for both traceable and untracable children, and
/// only the traceable children's lifetimes can be changed.
///
/// On top of scrubbing the lifetimes, this trait can also do a transformation
/// of the underlying type for convenience, similar to calling `Into::into`.
pub(crate) trait IntoRoot<T> {
    unsafe fn into_root(self) -> T;
}

impl<'a, T, U> IntoRoot<U> for T
where
    T: WithLifetime<'a, Out = U>,
    U: 'a,
{
    unsafe fn into_root(self) -> U {
        self.with_lifetime()
    }
}

impl<'a, T, U> IntoRoot<U> for &Rt<T>
where
    T: WithLifetime<'a, Out = U> + Copy,
{
    unsafe fn into_root(self) -> U {
        self.inner.with_lifetime()
    }
}

impl<'a> IntoRoot<GcObj<'a>> for bool {
    unsafe fn into_root(self) -> GcObj<'a> {
        self.into()
    }
}

impl<'a> IntoRoot<GcObj<'a>> for i64 {
    unsafe fn into_root(self) -> GcObj<'a> {
        self.into()
    }
}

impl<T> Trace for Gc<T> {
    fn trace(&self, stack: &mut Vec<RawObj>) {
        self.as_obj().trace_mark(stack);
    }
}

// Represents an object T rooted on the Stack. This will remove the the object
// from the root set when dropped.
#[doc(hidden)]
pub(crate) struct __StackRoot<'rt, T> {
    data: &'rt mut Rt<T>,
    root_set: &'rt RootSet,
}

impl<T> AsMut<Rt<T>> for __StackRoot<'_, T> {
    fn as_mut(&mut self) -> &mut Rt<T> {
        self.data
    }
}

impl<T: Debug> Debug for __StackRoot<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self.data, f)
    }
}

impl<T: Display> Display for __StackRoot<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self.data, f)
    }
}

// Do not use this function directly. Use the `root` macro instead.
//
// SAFETY: An owned StackRoot must never be exposed to the rest of the program.
// That could result in calling `mem::forget` on the root, which would
// invalidate the stack property of the root set.
impl<'rt, T: Trace> __StackRoot<'rt, T> {
    pub(crate) unsafe fn new(data: &'rt mut T, root_set: &'rt RootSet) -> __StackRoot<'rt, T> {
        let dyn_ptr = data as &mut dyn Trace as *mut dyn Trace;
        // We are using this transmute to dissociate the `dyn Trace` from the T.
        // Otherwise rust tries to require us to add a 'static bound. We don't
        // need this because stackroot can't outlive data (due to the 'rt
        // lifetime), and therefore it can't outlive T.
        let dyn_ptr = std::mem::transmute::<*mut dyn Trace, *mut dyn Trace>(dyn_ptr);
        let data = &mut *(dyn_ptr.cast::<Rt<T>>());
        let root = Self { data, root_set };
        root_set.roots.borrow_mut().push(dyn_ptr);
        root
    }
}

impl<T> Drop for __StackRoot<'_, T> {
    fn drop(&mut self) {
        self.root_set.roots.borrow_mut().pop();
    }
}

/// Trait created to overpass the orphan rule when deriving the
/// [Trace](`rune_macros::Trace`) derive macro. The derive
/// macro contains a blanket `Deref` (and `DerefMut`) like this:
///
/// ```ignore
/// unsafe { &*(rt as *const Rt<Self>).cast::<Self::Target>() }
/// ```
///
/// By creating a trait that the functions defined in the main crate
/// can define, we avoid the orphan rule by implementing `Deref`
/// on the rooted version of the types: [Rt\<T\>](`self::Rt`).
pub trait RootedDeref {
    type Target;
    fn rooted_deref(rooted: &Rt<Self>) -> &Self::Target;
    fn rooted_derefmut(rooted: &mut Rt<Self>) -> &mut Self::Target;
}

impl<T: RootedDeref> Deref for Rt<T> {
    type Target = <T as RootedDeref>::Target;
    fn deref(&self) -> &Self::Target {
        RootedDeref::rooted_deref(self)
    }
}

impl<T: RootedDeref> DerefMut for Rt<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        RootedDeref::rooted_derefmut(self)
    }
}

/// A Rooted type. If a type is wrapped in Rt, it is known to be rooted and hold
/// items past garbage collection. This type is never used as an owned type,
/// only a reference. This ensures that underlying data does not move. In order
/// to access the inner data, the [`Rt::bind`] method must be used.
#[repr(transparent)]
pub struct Rt<T: ?Sized> {
    _aliasable: PhantomPinned,
    inner: T,
}

impl<T: Debug> Debug for Rt<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.inner, f)
    }
}

impl<T: Display> Display for Rt<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.inner, f)
    }
}

impl PartialEq for Rt<GcObj<'_>> {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}

impl PartialEq for Rt<Symbol<'_>> {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}

impl Eq for Rt<Symbol<'_>> {}

impl<T: PartialEq<U>, U> PartialEq<U> for Rt<T> {
    fn eq(&self, other: &U) -> bool {
        self.inner == *other
    }
}

impl Deref for Rt<Gc<&LispString>> {
    type Target = LispString;

    fn deref(&self) -> &Self::Target {
        self.inner.untag()
    }
}

impl<T> Hash for Rt<T>
where
    T: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.inner.hash(state);
    }
}

impl<'new, T, const N: usize> WithLifetime<'new> for [T; N]
where
    T: WithLifetime<'new>,
{
    type Out = [<T as WithLifetime<'new>>::Out; N];
    unsafe fn with_lifetime(self) -> Self::Out {
        // work around since we can't transmute arrays
        let ptr = &self as *const [T; N] as *const Self::Out;
        let value = unsafe { ptr.read() };
        std::mem::forget(self);
        value
    }
}

impl<'new, T> WithLifetime<'new> for Vec<T>
where
    T: WithLifetime<'new>,
{
    type Out = Vec<<T as WithLifetime<'new>>::Out>;

    unsafe fn with_lifetime(self) -> Self::Out {
        std::mem::transmute(self)
    }
}

impl<'new, T> WithLifetime<'new> for VecDeque<T>
where
    T: WithLifetime<'new>,
{
    type Out = VecDeque<<T as WithLifetime<'new>>::Out>;

    unsafe fn with_lifetime(self) -> Self::Out {
        std::mem::transmute(self)
    }
}

impl<'new, T> WithLifetime<'new> for Option<T>
where
    T: WithLifetime<'new>,
{
    type Out = Option<<T as WithLifetime<'new>>::Out>;

    unsafe fn with_lifetime(self) -> Self::Out {
        self.map(|x| x.with_lifetime())
    }
}

impl<'new, T, U> WithLifetime<'new> for (T, U)
where
    T: WithLifetime<'new>,
    U: WithLifetime<'new>,
{
    type Out = (<T as WithLifetime<'new>>::Out, <U as WithLifetime<'new>>::Out);

    unsafe fn with_lifetime(self) -> Self::Out {
        (self.0.with_lifetime(), self.1.with_lifetime())
    }
}

impl<T> Rt<T> {
    pub(crate) fn bind<'ob>(&self, _: &'ob Context) -> <T as WithLifetime<'ob>>::Out
    where
        T: WithLifetime<'ob> + Copy,
    {
        // SAFETY: We are holding a reference to the context
        unsafe { self.inner.with_lifetime() }
    }

    pub(crate) unsafe fn bind_unchecked<'ob>(&'ob self) -> <T as WithLifetime<'ob>>::Out
    where
        T: WithLifetime<'ob> + Copy,
    {
        self.inner.with_lifetime()
    }

    pub(crate) fn bind_ref<'a, 'ob>(&'a self, _: &'ob Context) -> &'a <T as WithLifetime<'ob>>::Out
    where
        T: WithLifetime<'ob>,
    {
        // SAFETY: We are holding a reference to the context
        unsafe { std::mem::transmute::<&'a T, &'a <T as WithLifetime<'ob>>::Out>(&self.inner) }
    }

    pub(crate) fn bind_mut<'a, 'ob>(
        &'a mut self,
        _: &'ob Context,
    ) -> &'a mut <T as WithLifetime<'ob>>::Out
    where
        T: WithLifetime<'ob>,
    {
        // SAFETY: We are holding a reference to the context
        unsafe {
            std::mem::transmute::<&'a mut T, &'a mut <T as WithLifetime<'ob>>::Out>(&mut self.inner)
        }
    }

    pub(crate) fn bind_slice<'brw, 'ob, U>(slice: &'brw [Rt<T>], _: &'ob Context) -> &'brw [U]
    where
        T: WithLifetime<'ob, Out = U>,
        U: 'ob,
    {
        unsafe { &*(slice as *const [Rt<T>] as *const [U]) }
    }

    pub(crate) fn set<U: IntoRoot<T>>(&mut self, item: U) {
        // SAFETY: we drop the old type so it never exposed and take the new
        // rooted type and replace it.
        unsafe { self.inner = item.into_root() }
    }
}

impl TryFrom<&Rt<GcObj<'_>>> for usize {
    type Error = anyhow::Error;

    fn try_from(value: &Rt<GcObj>) -> Result<Self, Self::Error> {
        value.inner.try_into()
    }
}

impl<T> Rt<Gc<T>> {
    /// Like `try_into`, but needed to due no specialization
    pub(crate) fn try_into<U, E>(&self) -> Result<&Rt<Gc<U>>, E>
    where
        Gc<T>: TryInto<Gc<U>, Error = E> + Copy,
    {
        let _: Gc<U> = self.inner.try_into()?;
        // SAFETY: This is safe because all Gc types have the same representation
        unsafe { Ok(&*((self as *const Self).cast::<Rt<Gc<U>>>())) }
    }

    /// Like `try_into().bind(cx)`, but needed to due no specialization
    pub(crate) fn bind_as<'ob, U, E>(&self, _cx: &'ob Context) -> Result<U, E>
    where
        Gc<T>: WithLifetime<'ob> + Copy,
        <Gc<T> as WithLifetime<'ob>>::Out: TryInto<U, Error = E> + Copy,
    {
        unsafe { self.inner.with_lifetime().try_into() }
    }

    /// Like `From`, but needed to due no specialization
    pub(crate) fn use_as<U>(&self) -> &Rt<Gc<U>>
    where
        Gc<T>: Into<Gc<U>> + Copy,
    {
        // SAFETY: This is safe because all Gc types have the same representation
        unsafe { &*((self as *const Self).cast::<Rt<Gc<U>>>()) }
    }

    // TODO: Find a way to remove this method. We should never need to guess
    // if something is cons
    pub(crate) fn as_cons(&self) -> &Rt<Gc<&Cons>> {
        match self.inner.as_obj().untag() {
            crate::core::object::Object::Cons(_) => unsafe {
                &*(self as *const Self).cast::<Rt<Gc<&Cons>>>()
            },
            x => panic!("attempt to convert type that was not cons: {x}"),
        }
    }

    /// Calls [untag](Untag::untag_erased) on the tagged Gc pointer
    pub(crate) fn untag<'ob, U>(&self, cx: &'ob Context) -> U
    where
        Gc<T>: WithLifetime<'ob, Out = Gc<U>> + Copy,
        Gc<U>: Untag<U>,
    {
        self.bind(cx).untag_erased()
    }
}

impl From<&Rt<GcObj<'_>>> for Option<()> {
    fn from(value: &Rt<GcObj<'_>>) -> Self {
        value.inner.is_nil().then_some(())
    }
}

impl<'a> Rt<GcObj<'a>> {
    pub(crate) fn try_as_option<T, E>(&self) -> Result<Option<&Rt<Gc<T>>>, E>
    where
        GcObj<'a>: TryInto<Gc<T>, Error = E>,
    {
        if self.inner.is_nil() {
            Ok(None)
        } else {
            let _: Gc<T> = self.inner.try_into()?;
            unsafe { Ok(Some(&*((self as *const Self).cast::<Rt<Gc<T>>>()))) }
        }
    }
}

impl IntoObject for &Rt<GcObj<'_>> {
    type Out<'ob> = Object<'ob>;

    fn into_obj<const C: bool>(self, _block: &Block<C>) -> Gc<Self::Out<'_>> {
        unsafe { self.inner.with_lifetime() }
    }
}

impl IntoObject for &mut Rt<GcObj<'_>> {
    type Out<'ob> = Object<'ob>;

    fn into_obj<const C: bool>(self, _block: &Block<C>) -> Gc<Self::Out<'_>> {
        unsafe { self.inner.with_lifetime() }
    }
}

impl Rt<&Cons> {
    pub(crate) fn car<'ob>(&self, cx: &'ob Context) -> GcObj<'ob> {
        self.bind(cx).car()
    }

    pub(crate) fn cdr<'ob>(&self, cx: &'ob Context) -> GcObj<'ob> {
        self.bind(cx).cdr()
    }
}

impl<T, U> Deref for Rt<(T, U)> {
    type Target = (Rt<T>, Rt<U>);

    fn deref(&self) -> &Self::Target {
        unsafe { &*(self as *const Self).cast::<(Rt<T>, Rt<U>)>() }
    }
}

impl<T, U> DerefMut for Rt<(T, U)> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *(self as *mut Rt<(T, U)>).cast::<(Rt<T>, Rt<U>)>() }
    }
}

// Can't implement [`DerefMut`] because it would allow you to call
// [`Option::take`] which would return an owned Rt and break the chain of
// traceability
impl<T> Deref for Rt<Option<T>> {
    type Target = Option<Rt<T>>;
    fn deref(&self) -> &Self::Target {
        unsafe { &*(self as *const Self).cast::<Self::Target>() }
    }
}

impl<T> Rt<Option<T>> {
    // This is not really dead code, but the static analysis fails to find it
    #[allow(dead_code)]
    pub(crate) fn as_ref(&self) -> Option<&Rt<T>> {
        let option = self.inner.as_ref();
        option.map(|x| unsafe { &*(x as *const T).cast::<Rt<T>>() })
    }
}

impl<T, I, const N: usize> Index<I> for Rt<[T; N]>
where
    [Rt<T>]: Index<I>,
{
    type Output = <[Rt<T>] as Index<I>>::Output;

    fn index(&self, index: I) -> &Self::Output {
        let slice = unsafe { std::mem::transmute::<&[T], &[Rt<T>]>(&self.inner) };
        Index::index(slice, index)
    }
}

impl<T, I, const N: usize> IndexMut<I> for Rt<[T; N]>
where
    [Rt<T>]: IndexMut<I>,
{
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        let slice = unsafe { std::mem::transmute::<&mut [T], &mut [Rt<T>]>(&mut self.inner) };
        IndexMut::index_mut(slice, index)
    }
}

impl<T, const N: usize> AsRef<[Rt<T>]> for Rt<[T; N]> {
    fn as_ref(&self) -> &[Rt<T>] {
        unsafe { std::mem::transmute::<&[T], &[Rt<T>]>(&self.inner) }
    }
}

impl<T> Rt<Vec<T>> {
    // This is not safe to expose pub(crate) because you could call pop and get
    // an owned Rt
    fn as_mut_ref(&mut self) -> &mut Vec<Rt<T>> {
        // SAFETY: `Rt<T>` has the same memory layout as `T`.
        unsafe { &mut *(self as *mut Self).cast::<Vec<Rt<T>>>() }
    }

    pub(crate) fn push<U: IntoRoot<T>>(&mut self, item: U) {
        self.inner.push(unsafe { item.into_root() });
    }

    pub(crate) fn truncate(&mut self, len: usize) {
        self.inner.truncate(len);
    }

    pub(crate) fn pop(&mut self) {
        self.inner.pop();
    }

    pub(crate) fn swap_remove(&mut self, index: usize) {
        self.inner.swap_remove(index);
    }

    pub(crate) fn reserve(&mut self, additional: usize) {
        self.inner.reserve(additional);
    }

    pub(crate) fn capacity(&self) -> usize {
        self.inner.capacity()
    }
}

impl<T: Copy> Rt<Vec<T>> {
    pub(crate) fn extend_from_slice<U: IntoRoot<T> + Copy>(&mut self, src: &[U]) {
        self.inner.extend_from_slice(unsafe { std::mem::transmute::<&[U], &[T]>(src) });
    }

    pub(crate) fn extend_from_within(&mut self, src: impl RangeBounds<usize>) {
        self.inner.extend_from_within(src);
    }
}

impl<T> Deref for Rt<Vec<T>> {
    type Target = [Rt<T>];
    fn deref(&self) -> &Self::Target {
        // SAFETY: `Rt<T>` has the same memory layout as `T`.
        unsafe { &*(self as *const Self).cast::<Vec<Rt<T>>>() }
    }
}

impl<T> DerefMut for Rt<Vec<T>> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        // SAFETY: `Rt<T>` has the same memory layout as `T`.
        unsafe { &mut *(self as *mut Self).cast::<Vec<Rt<T>>>() }
    }
}

impl<T, I: SliceIndex<[Rt<T>]>> Index<I> for Rt<Vec<T>> {
    type Output = I::Output;

    fn index(&self, index: I) -> &Self::Output {
        let slice: &[Rt<T>] = self;
        Index::index(slice, index)
    }
}

impl<T, I: SliceIndex<[Rt<T>]>> IndexMut<I> for Rt<Vec<T>> {
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        IndexMut::index_mut(self.as_mut_ref(), index)
    }
}

impl<K, V> Rt<HashMap<K, V>>
where
    K: Eq + Hash,
{
    pub(crate) fn insert<Kx: IntoRoot<K>, Vx: IntoRoot<V>>(&mut self, k: Kx, v: Vx) {
        self.inner.insert(unsafe { k.into_root() }, unsafe { v.into_root() });
    }

    pub(crate) fn get<Q: IntoRoot<K>>(&self, k: Q) -> Option<&Rt<V>> {
        self.inner
            .get(unsafe { &k.into_root() })
            .map(|x| unsafe { &*(x as *const V).cast::<Rt<V>>() })
    }

    pub(crate) fn get_mut<Q: IntoRoot<K>>(&mut self, k: Q) -> Option<&mut Rt<V>> {
        self.inner
            .get_mut(unsafe { &k.into_root() })
            .map(|x| unsafe { &mut *(x as *mut V).cast::<Rt<V>>() })
    }

    pub(crate) fn remove<Q: IntoRoot<K>>(&mut self, k: Q) {
        self.inner.remove(unsafe { &k.into_root() });
    }
}

impl<K, V> Deref for Rt<HashMap<K, V>> {
    type Target = HashMap<Rt<K>, Rt<V>>;
    fn deref(&self) -> &Self::Target {
        // SAFETY: `Rt<T>` has the same memory layout as `T`.
        unsafe { &*(self as *const Self).cast::<Self::Target>() }
    }
}

impl<T> Deref for Rt<HashSet<T>> {
    type Target = HashSet<Rt<T>>;
    fn deref(&self) -> &Self::Target {
        // SAFETY: `Rt<T>` has the same memory layout as `T`.
        unsafe { &*(self as *const Self).cast::<Self::Target>() }
    }
}

#[cfg(test)]
mod test {
    use crate::core::object::NIL;
    use rune_core::macros::root;

    use super::super::RootSet;
    use super::*;

    #[test]
    fn mem_swap() {
        let root = &RootSet::default();
        let cx = &mut Context::new(root);
        let outer = cx.add("outer");
        root!(outer, cx);
        {
            let inner = cx.add("inner");
            root!(inner, cx);
            std::mem::swap(outer, inner);
        }
        cx.garbage_collect(true);
        assert_eq!(outer.bind(cx), "inner");
    }

    #[test]
    fn indexing() {
        let root = &RootSet::default();
        let cx = &Context::new(root);
        let mut vec = Rt { inner: vec![], _aliasable: PhantomPinned };

        vec.push(NIL);
        assert_eq!(vec[0], NIL);
        let str1 = cx.add("str1");
        let str2 = cx.add("str2");
        vec.push(str1);
        vec.push(str2);
        assert_eq!(vec![NIL, str1, str2], vec.bind_ref(cx)[0..3]);
    }
}
