use std::cell::RefCell;

use crate::core::arena::Rt;
use crate::core::env::Environment;
use crate::core::object::HashTable;
use crate::core::{
    arena::{Arena, Root},
    cons::Cons,
    env::Symbol,
    error::{Error, Type},
    object::{Callable, Function, Gc, GcObj, List, Object},
};
use crate::{data, element_iter, rebind, root};
use anyhow::anyhow;
use anyhow::{bail, Result};
use fn_macros::defun;
use streaming_iterator::StreamingIterator;

pub(crate) fn slice_into_list<'ob>(
    slice: &[GcObj<'ob>],
    tail: Option<GcObj<'ob>>,
    arena: &'ob Arena,
) -> GcObj<'ob> {
    let from_end = slice.iter().rev();
    from_end.fold(tail.into(), |acc, obj| cons!(*obj, acc; arena))
}

#[defun]
pub(crate) fn prin1_to_string(object: GcObj, _noescape: Option<GcObj>) -> String {
    format!("{object}")
}

impl<'ob> Rt<Gc<Function<'ob>>> {
    pub(crate) fn call<'gc>(
        &self,
        args: &mut Root<Vec<GcObj<'static>>>,
        env: &mut Root<Environment>,
        arena: &'gc mut Arena,
        name: Option<&str>,
    ) -> Result<GcObj<'gc>> {
        use crate::interpreter;
        match self.bind(arena).get() {
            Function::LispFn(_) => todo!("call lisp functions"),
            Function::SubrFn(f) => (*f).call(args, env, arena),
            Function::Cons(cons) => {
                root!(cons, arena);
                interpreter::call(cons, args, env, name.unwrap_or("closure"), arena)
            }
            Function::Symbol(sym) => {
                if let Some(resolved) = sym.resolve_callable(arena) {
                    match resolved.get() {
                        Callable::LispFn(_) => todo!("call lisp functions"),
                        Callable::SubrFn(f) => (*f).call(args, env, arena),
                        Callable::Cons(cons) => {
                            root!(cons, arena);
                            interpreter::call(cons, args, env, sym.name, arena)
                        }
                    }
                } else {
                    Err(anyhow!("Void Function: {}", sym))
                }
            }
        }
    }
}

#[defun]
pub(crate) fn mapcar<'ob>(
    function: &Rt<Gc<Function>>,
    sequence: &Rt<Gc<List>>,
    env: &mut Root<Environment>,
    gc: &'ob mut Arena,
) -> Result<GcObj<'ob>> {
    match sequence.bind(gc).get() {
        List::Nil => Ok(GcObj::NIL),
        List::Cons(cons) => {
            root!(outputs, Vec::new(), gc);
            root!(call_arg, Vec::new(), gc);
            element_iter!(iter, cons, gc);
            while let Some(x) = iter.next() {
                let obj = x.bind(gc);
                call_arg.deref_mut(gc).push(obj);
                let output = function.call(call_arg, env, gc, None)?;
                rebind!(output, gc);
                outputs.deref_mut(gc).push(output);
                call_arg.deref_mut(gc).clear();
            }
            // TODO: remove this intermediate vector
            let slice = outputs.as_slice().as_ref(gc);
            Ok(slice_into_list(slice, None, gc))
        }
    }
}

#[defun]
pub(crate) fn mapc<'ob>(
    function: &Rt<Gc<Function>>,
    sequence: &Rt<Gc<List>>,
    env: &mut Root<Environment>,
    gc: &'ob mut Arena,
) -> Result<GcObj<'ob>> {
    match sequence.bind(gc).get() {
        List::Nil => Ok(GcObj::NIL),
        List::Cons(cons) => {
            root!(call_arg, Vec::new(), gc);
            element_iter!(elements, cons, gc);
            while let Some(elem) = elements.next() {
                call_arg.deref_mut(gc).push(elem.bind(gc));
                function.call(call_arg, env, gc, None)?;
                call_arg.deref_mut(gc).clear();
            }
            Ok(sequence.bind(gc).into())
        }
    }
}

#[defun]
pub(crate) fn nreverse(seq: Gc<List>) -> Result<GcObj> {
    let mut prev = GcObj::NIL;
    for tail in seq.conses() {
        let tail = tail?;
        tail.set_cdr(prev);
        prev = tail.into();
    }
    Ok(prev)
}

fn join<'ob>(list: &mut Vec<GcObj<'ob>>, seq: Gc<List<'ob>>, arena: &'ob Arena) -> Result<()> {
    match seq.get() {
        List::Cons(cons) => {
            for elt in cons.elements(arena) {
                list.push(elt?);
            }
        }
        List::Nil => {}
    }
    Ok(())
}

#[defun]
pub(crate) fn append<'ob>(
    append: GcObj<'ob>,
    sequences: &[GcObj<'ob>],
    arena: &'ob Arena,
) -> Result<GcObj<'ob>> {
    let mut list = Vec::new();
    join(&mut list, append.try_into()?, arena)?;
    for seq in sequences {
        join(&mut list, (*seq).try_into()?, arena)?;
    }
    Ok(slice_into_list(&list, None, arena))
}

#[defun]
pub(crate) fn assq<'ob>(key: GcObj<'ob>, alist: Gc<List<'ob>>, arena: &'ob Arena) -> GcObj<'ob> {
    match alist.get() {
        List::Nil => GcObj::NIL,
        List::Cons(cons) => cons
            .elements(arena)
            .find(|x| match x {
                Ok(elem) => match elem.get() {
                    Object::Cons(cons) => data::eq(key, cons.car()),
                    _ => false,
                },
                _ => false,
            })
            .transpose()
            .expect("should never return error value")
            .unwrap_or_default(),
    }
}

type EqFunc = for<'ob> fn(GcObj<'ob>, GcObj<'ob>) -> bool;

fn delete_from_list<'ob>(
    elt: GcObj<'ob>,
    list: Gc<List<'ob>>,
    eq_fn: EqFunc,
) -> Result<GcObj<'ob>> {
    let mut head = list.into();
    let mut prev: Option<&'ob Cons> = None;
    for tail in list.conses() {
        let tail = tail?;
        if eq_fn(tail.car(), elt) {
            if let Some(prev_tail) = &mut prev {
                prev_tail.set_cdr(tail.cdr());
            } else {
                head = tail.cdr();
            }
        } else {
            prev = Some(tail);
        }
    }
    Ok(head)
}

#[defun]
pub(crate) fn delete<'ob>(elt: GcObj<'ob>, list: Gc<List<'ob>>) -> Result<GcObj<'ob>> {
    delete_from_list(elt, list, data::equal)
}

#[defun]
pub(crate) fn delq<'ob>(elt: GcObj<'ob>, list: Gc<List<'ob>>) -> Result<GcObj<'ob>> {
    delete_from_list(elt, list, data::eq)
}

fn member_of_list<'ob>(elt: GcObj<'ob>, list: Gc<List<'ob>>, eq_fn: EqFunc) -> Result<GcObj<'ob>> {
    let val = list.conses().find(|x| match x {
        Ok(obj) => eq_fn(obj.car(), elt),
        Err(_) => true,
    });
    match val {
        Some(Ok(elem)) => Ok(elem.into()),
        Some(Err(e)) => Err(e),
        None => Ok(GcObj::NIL),
    }
}

#[defun]
pub(crate) fn memq<'ob>(elt: GcObj<'ob>, list: Gc<List<'ob>>) -> Result<GcObj<'ob>> {
    member_of_list(elt, list, data::eq)
}

#[defun]
pub(crate) fn member<'ob>(elt: GcObj<'ob>, list: Gc<List<'ob>>) -> Result<GcObj<'ob>> {
    member_of_list(elt, list, data::equal)
}

#[defun]
pub(crate) fn defvaralias(
    new_alias: Symbol,
    _base_variable: Symbol,
    _docstring: Option<&str>,
) -> Symbol {
    // TODO: implement
    new_alias
}

#[defun]
pub(crate) fn featurep(_feature: Symbol, _subfeature: Option<Symbol>) -> bool {
    // TODO: implement
    false
}

#[defun]
fn require<'ob>(
    feature: Symbol,
    filename: Option<&Rt<Gc<&String>>>,
    noerror: Option<()>,
    env: &mut Root<Environment>,
    arena: &'ob mut Arena,
) -> Result<GcObj<'ob>> {
    if crate::data::FEATURES.lock().unwrap().contains(feature) {
        return Ok(feature.into());
    }
    let file = match filename {
        None => format!("lisp/{}.el", feature.name),
        Some(file) => file.bind(arena).get().clone(),
    };
    let file: GcObj = arena.add(file);
    root!(file, arena);
    match crate::lread::load(file, None, arena, env) {
        Ok(_) => Ok(feature.into()),
        Err(e) => match noerror {
            Some(()) => Ok(Gc::NIL),
            None => Err(e),
        },
    }
}

#[defun]
pub(crate) fn concat(sequences: &[GcObj]) -> Result<String> {
    let mut concat = String::new();
    for elt in sequences {
        match elt.get() {
            Object::String(string) => concat.push_str(string),
            _ => bail!("Currently only concatenating strings are supported"),
        }
    }
    Ok(concat)
}

#[defun]
pub(crate) fn length<'ob>(sequence: GcObj<'ob>, arena: &'ob Arena) -> Result<i64> {
    let size = match sequence.get() {
        Object::Cons(x) => x.elements(arena).len(),
        Object::Vec(x) => x.borrow().len(),
        Object::String(x) => x.len(),
        Object::Nil => 0,
        obj => bail!(Error::from_object(Type::Sequence, obj)),
    };
    Ok(size
        .try_into()
        .expect("conversion from usize to isize should never fail"))
}

#[defun]
pub(crate) fn nth<'ob>(n: usize, list: Gc<List<'ob>>, arena: &'ob Arena) -> Result<GcObj<'ob>> {
    list.elements(arena).nth(n).unwrap_or(Ok(GcObj::NIL))
}

#[defun]
pub(crate) fn nthcdr(n: usize, list: Gc<List>) -> Result<Gc<List>> {
    match list.conses().nth(n) {
        Some(x) => x.map(Into::into),
        None => Ok(List::EMPTY),
    }
}

#[defun]
pub(crate) fn make_hash_table<'ob>(
    keyword_args: &[GcObj<'ob>],
    gc: &'ob Arena,
) -> Result<GcObj<'ob>> {
    use crate::core::env::sym;
    if let Some(i) = keyword_args
        .iter()
        .step_by(2)
        .position(|&x| x == sym::KW_TEST)
    {
        let val = match keyword_args.get((i * 2) + 1) {
            Some(x) => *x,
            None => bail!("Missing keyword value for :test"),
        };
        if val != sym::EQ && val != sym::EQUAL {
            // TODO: we are currently only using `equal', but eq should be okay
            bail!("only `eq' and `equal' keywords support for make-hash-table :test. Found {val}");
        }
    }
    // TODO, the rest of the keywords need to be supported here
    let map: HashTable = HashTable::with_hasher(std::hash::BuildHasherDefault::default());
    Ok(gc.add(map))
}

#[defun]
pub(crate) fn hash_table_p(obj: GcObj) -> bool {
    matches!(obj.get(), Object::HashTable(_))
}

#[defun]
pub(crate) fn puthash<'ob>(
    key: GcObj<'ob>,
    value: GcObj<'ob>,
    table: &RefCell<HashTable<'ob>>,
) -> GcObj<'ob> {
    table.borrow_mut().insert(key, value);
    value
}

#[defun]
pub(crate) fn gethash<'ob>(
    key: GcObj<'ob>,
    table: &RefCell<HashTable<'ob>>,
    dflt: Option<GcObj<'ob>>,
) -> Option<GcObj<'ob>> {
    match table.borrow().get(&key) {
        Some(x) => Some(*x),
        None => dflt,
    }
}

#[defun]
fn copy_sequence<'ob>(arg: GcObj<'ob>, arena: &'ob Arena) -> Result<GcObj<'ob>> {
    match arg.get() {
        Object::String(_) | Object::Vec(_) | Object::Nil | Object::Cons(_) => {
            Ok(arg.clone_in(arena))
        }
        _ => Err(Error::from_object(Type::Sequence, arg).into()),
    }
}

#[defun]
fn substring(string: &str, from: Option<usize>, to: Option<usize>) -> String {
    let new_string = match (from, to) {
        (None, None) => string,
        (None, Some(t)) => &string[..t],
        (Some(f), None) => &string[f..],
        (Some(f), Some(t)) => &string[f..t],
    };
    new_string.to_owned()
}

#[defun]
fn enable_debug() -> bool {
    crate::debug::enable_debug();
    false
}

#[cfg(test)]
mod test {
    use crate::core::{arena::RootSet, object::IntoObject};

    use super::*;

    #[test]
    fn test_delq() {
        let roots = &RootSet::default();
        let arena = &Arena::new(roots);
        {
            let list: GcObj = list![1, 2, 3, 1, 4, 1; arena];
            let res = delq(1.into(), list.try_into().unwrap()).unwrap();
            assert_eq!(res, list![2, 3, 4; arena]);
        }
        {
            let list: GcObj = list![true, true, true; arena];
            let res = delq(GcObj::TRUE, list.try_into().unwrap()).unwrap();
            assert_eq!(res, GcObj::NIL);
        }
    }

    #[test]
    fn test_nthcdr() {
        let roots = &RootSet::default();
        let arena = &Arena::new(roots);
        let list = list![1, 2, 3; arena];
        let res = nthcdr(1, list.try_into().unwrap()).unwrap();
        assert_eq!(res.get().car(), 2);
    }

    #[test]
    fn test_nreverse() {
        let roots = &RootSet::default();
        let arena = &Arena::new(roots);
        {
            let list: GcObj = list![1, 2, 3, 4; arena];
            let res: GcObj = nreverse(list.try_into().unwrap()).unwrap().into_obj(arena);
            assert_eq!(res, list![4, 3, 2, 1; arena]);
        }
        {
            let list: GcObj = list![1; arena];
            let res: GcObj = nreverse(list.try_into().unwrap()).unwrap().into_obj(arena);
            assert_eq!(res, list![1; arena]);
        }
    }

    #[test]
    fn test_mapcar() {
        let roots = &RootSet::default();
        let arena = &Arena::new(roots);
        let element = cons!(5, 6; arena);
        let list: GcObj = list![cons!(1, 2; arena), cons!(3, 4; arena), element; arena];
        let list = list.try_into().unwrap();
        let result = assq(5.into(), list, arena);
        assert_eq!(result, element);
    }
}

defsubr!(
    mapcar,
    mapc,
    nreverse,
    assq,
    make_hash_table,
    hash_table_p,
    puthash,
    gethash,
    length,
    nth,
    nthcdr,
    concat,
    append,
    delq,
    delete,
    memq,
    member,
    defvaralias,
    featurep,
    require,
    prin1_to_string,
    substring,
    copy_sequence,
    enable_debug,
);
