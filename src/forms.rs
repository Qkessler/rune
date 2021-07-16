use crate::intern::INTERNED_SYMBOLS;
use crate::object::{Function, Object, Symbol, NIL};
use fn_macros::lisp_fn;

#[lisp_fn]
pub fn defalias(symbol: Symbol, definition: Function) -> Symbol {
    let map = INTERNED_SYMBOLS.lock().unwrap();
    map.set_func(symbol, definition);
    symbol
}

#[lisp_fn]
pub fn progn<'ob>(forms: &[Object<'ob>]) -> Object<'ob> {
    match forms.last() {
        Some(form) => *form,
        None => NIL,
    }
}

defsubr!(defalias, progn);
