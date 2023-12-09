#![allow(unstable_name_collisions)]
use super::gc::{Block, Context, Rt};
use super::hashmap::HashMap;
use super::object::{CloneIn, Function, Gc, GcObj, LispBuffer, OpenBuffer, WithLifetime};
use anyhow::{anyhow, Result};
use rune_macros::Trace;

mod stack;
mod symbol;
use stack::LispStack;
pub(crate) use symbol::*;

#[derive(Debug, Default, Trace)]
pub struct Env {
    pub vars: HashMap<Symbol<'static>, GcObj<'static>>,
    pub props: HashMap<Symbol<'static>, Vec<(Symbol<'static>, GcObj<'static>)>>,
    pub catch_stack: Vec<GcObj<'static>>,
    exception: (GcObj<'static>, GcObj<'static>),
    #[no_trace]
    exception_id: u32,
    binding_stack: Vec<(Symbol<'static>, Option<GcObj<'static>>)>,
    pub match_data: GcObj<'static>,
    #[no_trace]
    pub current_buffer: Option<OpenBuffer<'static>>,
    pub stack: LispStack,
}

// RootedEnv created by #[derive(Trace)]
impl RootedEnv {
    pub(crate) fn set_var(&mut self, sym: Symbol, value: GcObj) -> Result<()> {
        if sym.is_const() {
            Err(anyhow!("Attempt to set a constant symbol: {sym}"))
        } else {
            self.vars.insert(sym, value);
            Ok(())
        }
    }

    pub fn set_prop(&mut self, symbol: Symbol, propname: Symbol, value: GcObj) {
        match self.props.get_mut(symbol) {
            Some(plist) => match plist.iter_mut().find(|x| x.0 == propname) {
                Some(x) => x.1.set(value),
                None => plist.push((propname, value)),
            },
            None => {
                self.props.insert(symbol, vec![(propname, value)]);
            }
        }
    }

    pub fn set_exception(&mut self, tag: GcObj, data: GcObj) -> u32 {
        self.exception.0.set(tag);
        self.exception.1.set(data);
        self.exception_id += 1;
        self.exception_id
    }

    pub fn get_exception(&self, id: u32) -> Option<(&Rt<GcObj<'static>>, &Rt<GcObj<'static>>)> {
        (id == self.exception_id).then_some((&self.exception.0, &self.exception.1))
    }

    pub fn varbind(&mut self, var: Symbol, value: GcObj, cx: &Context) {
        let prev_value = self.vars.get(var).map(|x| x.bind(cx));
        self.binding_stack.push((var, prev_value));
        self.vars.insert(var, value);
    }

    pub fn unbind(&mut self, count: u16, cx: &Context) {
        for _ in 0..count {
            match self.binding_stack.bind_mut(cx).pop() {
                Some((sym, val)) => match val {
                    Some(val) => self.vars.insert(sym, val),
                    None => self.vars.remove(sym),
                },
                None => panic!("Binding stack was empty"),
            }
        }
    }

    pub fn defvar(&mut self, var: Symbol, value: GcObj) -> Result<()> {
        // TOOD: Handle `eval-sexp` on defvar, which should always update the
        // value
        if self.vars.get(var).is_none() {
            self.set_var(var, value)?;
            var.make_special();
        }

        // If this variable was unbound previously in the binding stack,
        // we will bind it to the new value
        for binding in &mut *self.binding_stack {
            if binding.0 == var && binding.1.is_none() {
                binding.1.set(value);
            }
        }
        Ok(())
    }

    pub fn set_buffer(&mut self, buffer: &LispBuffer) -> Result<()> {
        if let Some(current) = &self.current_buffer {
            if buffer == current {
                return Ok(());
            }
        }
        // SAFETY: We are not dropping the buffer until we have can trace it
        // with the garbage collector
        let lock = unsafe { buffer.lock()?.with_lifetime() };
        self.current_buffer = Some(lock);
        Ok(())
    }

    pub fn with_buffer<T>(
        &self,
        buffer: Option<&LispBuffer>,
        func: impl Fn(&OpenBuffer) -> T,
    ) -> Option<T> {
        match (&self.current_buffer, buffer) {
            (Some(_), None) => Some(func(self.current_buffer.as_ref().unwrap())),
            (Some(current), Some(buffer)) if current == buffer => {
                Some(func(self.current_buffer.as_ref().unwrap()))
            }
            (_, Some(buffer)) => {
                if let Ok(buffer) = buffer.lock().as_mut() {
                    Some(func(buffer))
                } else {
                    None
                }
            }
            (None, None) => None,
        }
    }

    pub fn with_buffer_mut<T>(
        &mut self,
        buffer: Option<&LispBuffer>,
        func: impl Fn(&mut OpenBuffer) -> T,
    ) -> Option<T> {
        match (&self.current_buffer, buffer) {
            (Some(current), Some(buffer)) if current == buffer => {
                Some(func(self.current_buffer.as_mut().unwrap()))
            }
            (Some(_), None) => Some(func(self.current_buffer.as_mut().unwrap())),
            (_, Some(buffer)) => {
                if let Ok(buffer) = buffer.lock().as_mut() {
                    Some(func(buffer))
                } else {
                    None
                }
            }
            (None, None) => None,
        }
    }
}

pub struct ObjectMap {
    pub map: SymbolMap,
    pub block: Block<true>,
}

/// Box is marked as unique. However we are freely sharing the pointer to this
/// Symbol amoung threads. So instead of Box we need to use a custom wrapper
/// type for this to be sound.
struct SymbolBox(*const SymbolCell);
unsafe impl Send for SymbolBox {}

impl SymbolBox {
    fn new(inner: SymbolCell) -> Self {
        let ptr = Box::into_raw(Box::new(inner));
        Self(ptr)
    }

    fn from_static(inner: Symbol<'static>) -> Self {
        Self(inner.get())
    }
}

impl AsRef<SymbolCell> for SymbolBox {
    fn as_ref(&self) -> &SymbolCell {
        unsafe { &*self.0 }
    }
}

// `SymbolBox` should not be dropped until we
// have a garbage collector
impl Drop for SymbolBox {
    fn drop(&mut self) {
        assert!(!std::thread::panicking(), "Error: Tried to drop Symbol: {:?}", unsafe {
            &*self.0
        });
    }
}

pub struct SymbolMap {
    map: HashMap<&'static str, SymbolBox>,
}

impl SymbolMap {
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            map: HashMap::with_capacity_and_hasher(cap, std::hash::BuildHasherDefault::default()),
        }
    }

    fn get(&self, name: &str) -> Option<Symbol> {
        unsafe {
            self.map.get(name).map(|x| {
                let ptr: *const SymbolCell = x.as_ref();
                Symbol::new(&*ptr)
            })
        }
    }

    fn intern<'ob>(&mut self, name: &str, _cx: &'ob Context) -> Symbol<'ob> {
        let sym = match self.map.get(name) {
            Some(x) => x.0,
            None => {
                let name = name.to_owned();
                // Leak the memory so that it is static
                let static_name: &'static str = unsafe {
                    let name_ptr: *const str = Box::into_raw(name.into_boxed_str());
                    &*name_ptr
                };
                let inner = SymbolCell::new(static_name);
                let sym = SymbolBox::new(inner);
                let ptr: *const SymbolCell = sym.as_ref();
                self.map.insert(static_name, sym);
                ptr
            }
        };
        // SAFETY: We can guarantee that the reference is static because we have
        // no methods to remove items from SymbolMap and SymbolMap has a private
        // constructor, so the only one that exists is the one we create in this
        // module, which is static.
        unsafe { Symbol::new(&*sym) }
    }

    pub fn pre_init(&mut self, sym: Symbol<'static>) {
        use std::collections::hash_map::Entry;
        let name = sym.get().name();
        let entry = self.map.entry(name);
        assert!(matches!(entry, Entry::Vacant(_)), "Attempt to intitalize {name} twice");
        entry.or_insert_with(|| SymbolBox::from_static(sym));
    }
}

impl ObjectMap {
    pub fn intern<'ob>(&mut self, name: &str, cx: &'ob Context) -> Symbol<'ob> {
        self.map.intern(name, cx)
    }

    pub fn set_func(&self, symbol: Symbol, func: Gc<Function>) -> Result<()> {
        let new_func = func.clone_in(&self.block);
        self.block.uninterned_symbol_map.clear();
        #[cfg(miri)]
        new_func.untag().set_as_miri_root();
        // SAFETY: The object is marked read-only, we have cloned in the
        // map's context, and it is not const, so calling this function
        // is safe.
        unsafe { symbol.set_func(new_func) }
    }

    pub fn create_buffer(&self, name: &str) -> &LispBuffer {
        LispBuffer::create(name.to_owned(), &self.block)
    }

    pub fn get(&self, name: &str) -> Option<Symbol> {
        self.map.get(name)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use std::mem::size_of;

    #[test]
    fn size() {
        assert_eq!(size_of::<isize>(), size_of::<Symbol>());
        assert_eq!(size_of::<isize>(), size_of::<Gc<Function>>());
    }
}
