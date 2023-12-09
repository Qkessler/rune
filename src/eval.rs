//! Lisp evaluation primitives.
use crate::fns::{assq, eq};
use crate::init::defun;
use anyhow::{anyhow, bail, ensure, Result};
use fallible_iterator::FallibleIterator;
use fallible_streaming_iterator::FallibleStreamingIterator;
use rune_core::cons::Cons;
use rune_core::env::{sym, Env, Symbol};
use rune_core::error::{EvalError, Type, TypeError};
use rune_core::gc::{Block, Rt};
use rune_core::macros::{list, root, rooted_iter};
use rune_core::object::{nil, FnArgs, IntoObject, LispString, Object};
use rune_core::{
    gc::{Context, IntoRoot},
    object::{Function, Gc, GcObj},
};
use rune_macros::defun;

use crate::interpreter::CallFunction;

#[defun]
pub(crate) fn apply<'ob>(
    function: &Rt<Gc<Function>>,
    arguments: &[Rt<GcObj>],
    env: &mut Rt<Env>,
    cx: &'ob mut Context,
) -> Result<GcObj<'ob>> {
    let args = match arguments.len() {
        0 => Vec::new(),
        len => {
            let end = len - 1;
            let last = &arguments[end];
            let mut args: Vec<_> = arguments[..end].iter().map(|x| x.bind(cx)).collect();
            for element in last.bind(cx).as_list()? {
                let e = cx.bind(element?);
                args.push(e);
            }
            args
        }
    };
    root!(args, cx);
    function.call(args, None, env, cx).map_err(Into::into)
}

#[defun]
pub(crate) fn funcall<'ob>(
    function: &Rt<Gc<Function>>,
    arguments: &[Rt<GcObj>],
    env: &mut Rt<Env>,
    cx: &'ob mut Context,
) -> Result<GcObj<'ob>> {
    let arguments = unsafe { Rt::bind_slice(arguments, cx).to_vec().into_root() };
    root!(arg_list, arguments, cx);
    function.call(arg_list, None, env, cx).map_err(Into::into)
}

#[defun]
fn run_hooks<'ob>(
    hooks: &[Rt<GcObj>],
    env: &mut Rt<Env>,
    cx: &'ob mut Context,
) -> Result<GcObj<'ob>> {
    for hook in hooks {
        match hook.get(cx) {
            Object::Symbol(sym) => {
                if let Some(val) = env.vars.get(sym) {
                    let val = val.bind(cx);
                    match val.untag() {
                        Object::Cons(hook_list) => {
                            rooted_iter!(hooks, hook_list, cx);
                            while let Some(hook) = hooks.next()? {
                                let func: &Rt<Gc<Function>> = hook.try_into()?;
                                root!(args, Vec::new(), cx);
                                func.call(args, None, env, cx)?;
                            }
                        }
                        Object::NIL => {}
                        _ => {
                            let func: Gc<Function> = val.try_into()?;
                            root!(func, cx);
                            root!(args, Vec::new(), cx);
                            func.call(args, None, env, cx)?;
                        }
                    }
                }
            }
            x => bail!(TypeError::new(Type::Symbol, x)),
        }
    }
    Ok(nil())
}

#[defun]
fn run_hook_with_args<'ob>(
    hook: &Rt<GcObj>,
    args: &[Rt<GcObj>],
    env: &mut Rt<Env>,
    cx: &'ob mut Context,
) -> Result<GcObj<'ob>> {
    match hook.get(cx) {
        Object::Symbol(sym) => {
            if let Some(val) = env.vars.get(sym) {
                let val = val.bind(cx);
                match val.untag() {
                    Object::Cons(hook_list) => {
                        rooted_iter!(hooks, hook_list, cx);
                        while let Some(hook) = hooks.next()? {
                            let func: &Rt<Gc<Function>> = hook.try_into()?;
                            let args = Rt::bind_slice(args, cx).to_vec();
                            root!(args, cx);
                            func.call(args, None, env, cx)?;
                        }
                    }
                    Object::NIL => {}
                    _ => {
                        let func: Gc<Function> = val.try_into()?;
                        root!(func, cx);
                        root!(args, Vec::new(), cx);
                        func.call(args, None, env, cx)?;
                    }
                }
            }
        }
        x => bail!(TypeError::new(Type::Symbol, x)),
    }
    Ok(nil())
}

#[defun]
pub(crate) fn autoload_do_load<'ob>(
    fundef: &Rt<GcObj>,
    funname: Option<&Rt<Gc<Symbol>>>,
    macro_only: Option<&Rt<GcObj>>,
    env: &mut Rt<Env>,
    cx: &'ob mut Context,
) -> Result<GcObj<'ob>> {
    // TODO: want to handle the case where the file is already loaded.
    match fundef.get(cx) {
        Object::Cons(cons) if cons.car() == defun::AUTOLOAD => {
            ensure!(macro_only.is_none(), "autoload-do-load macro-only is not yet implemented");
            let mut iter = cons.elements();
            iter.next(); // autoload
            let file: Gc<&LispString> = match iter.next() {
                Some(x) => x?.try_into()?,
                None => bail!("Malformed autoload"),
            };
            ensure!(
                iter.fallible().all(|x| Ok(x.is_nil()))?,
                "autoload arguments are not yet implemented"
            );
            root!(file, cx);
            crate::lread::load(file, None, None, cx, env)?;
            match funname {
                Some(func) => match func.get(cx).func(cx) {
                    Some(x) => Ok(x.into()),
                    None => Err(anyhow!("autoload of {func} did not provide a definition")),
                },
                _ => Ok(nil()),
            }
        }
        _ => Ok(fundef.bind(cx)),
    }
}

#[defun]
fn autoload<'ob>(
    function: Symbol<'ob>,
    file: &str,
    docstring: Option<GcObj>,
    interactive: Option<GcObj>,
    load_type: Option<GcObj>,
    cx: &'ob Context,
) -> Result<Symbol<'ob>> {
    if function.has_func() {
        Ok(sym::NIL)
    } else {
        let autoload = list![defun::AUTOLOAD, file, docstring, interactive, load_type; cx];
        crate::data::fset(function, autoload)
    }
}

#[defun]
pub(crate) fn macroexpand<'ob>(
    form: &Rt<GcObj>,
    environment: Option<&Rt<GcObj>>,
    cx: &'ob mut Context,
    env: &mut Rt<Env>,
) -> Result<GcObj<'ob>> {
    let Object::Cons(cons) = form.get(cx) else { return Ok(form.bind(cx)) };
    let Object::Symbol(sym) = cons.car().untag() else { return Ok(form.bind(cx)) };
    // shadow the macro based on ENVIRONMENT
    let func = match environment {
        Some(env) => match assq(sym.into(), env.bind(cx).try_into()?)?.untag() {
            Object::Cons(cons) => Some(cons.cdr().try_into()?),
            _ => get_macro_func(sym, cx),
        },
        _ => get_macro_func(sym, cx),
    };
    let Some(macro_func) = func else { return Ok(form.bind(cx)) };
    let macro_args: Vec<_> = cons.cdr().as_list()?.fallible().collect()?;
    root!(macro_args, cx);
    root!(macro_func, cx);
    let name = sym.name().to_owned();
    let new_form = macro_func.call(macro_args, Some(&name), env, cx)?;
    root!(new_form, cx); // polonius
    if eq(new_form.bind(cx), form.bind(cx)) {
        Ok(form.bind(cx))
    } else {
        // recursively expand the macro's
        macroexpand(new_form, environment, cx, env)
    }
}

fn get_macro_func<'ob>(name: Symbol, cx: &'ob Context) -> Option<Gc<Function<'ob>>> {
    if let Some(callable) = name.follow_indirect(cx) {
        if let Function::Cons(cons) = callable.untag() {
            if cons.car() == sym::MACRO {
                return cons.cdr().try_into().ok();
            }
        }
    }
    None
}

#[defun]
fn func_arity<'ob>(function: Gc<Function>, cx: &'ob Context) -> Result<&'ob Cons> {
    let from_args = |args: FnArgs| {
        let min = args.required;
        if args.rest {
            // TODO: Handle unevalled
            Cons::new(min, sym::MANY, cx)
        } else {
            Cons::new(min, args.optional + min, cx)
        }
    };
    match function.untag() {
        Function::ByteFn(func) => Ok(from_args(func.args)),
        Function::SubrFn(func) => Ok(from_args(func.args)),
        Function::Cons(func) => {
            let arg_pos = match func.car().untag() {
                Object::Symbol(sym::CLOSURE) => 2,
                Object::Symbol(sym::LAMBDA) => 1,
                other => bail!(TypeError::new(Type::Func, other)),
            };
            let Some(args) = func.elements().fallible().nth(arg_pos)? else {
                bail!("Invalid function: {func}")
            };
            let (req, opt, rest) = crate::interpreter::parse_arg_list(args)?;
            let args = FnArgs {
                required: req.len() as u16,
                optional: opt.len() as u16,
                rest: rest.is_some(),
                ..FnArgs::default()
            };
            Ok(from_args(args))
        }
        Function::Symbol(sym) => {
            let Some(func) = sym.follow_indirect(cx) else { bail!("Void Function: {sym}") };
            func_arity(func, cx)
        }
    }
}

#[allow(non_snake_case)]
#[defun]
fn internal__define_uninitialized_variable<'ob>(
    _symbol: Symbol<'ob>,
    _doc: Option<GcObj>,
) -> GcObj<'ob> {
    // TODO: implement doc strings
    nil()
}

#[defun]
fn signal(mut error_symbol: GcObj, data: GcObj, env: &mut Rt<Env>) -> Result<bool> {
    if error_symbol.is_nil() && data.is_nil() {
        error_symbol = sym::ERROR.into();
    }
    Err(EvalError::signal(error_symbol, data, env).into())
}

#[defun]
fn special_variable_p(symbol: Symbol) -> bool {
    symbol.is_special()
}

#[defun]
fn set_default_toplevel_value<'ob>(
    symbol: Symbol,
    value: GcObj,
    env: &'ob mut Rt<Env>,
) -> Result<GcObj<'ob>> {
    env.set_var(symbol, value)?;
    Ok(nil())
}

#[defun]
fn set_default<'ob>(
    symbol: Symbol,
    value: GcObj<'ob>,
    env: &'ob mut Rt<Env>,
) -> Result<GcObj<'ob>> {
    // TODO: implement buffer local variables
    env.set_var(symbol, value)?;
    Ok(value)
}

struct Bool(bool);

impl IntoObject for Bool {
    type Out<'a> = Symbol<'a>;

    fn into_obj<const C: bool>(self, _: &Block<C>) -> Gc<Self::Out<'_>> {
        let sym = match self.0 {
            true => sym::TRUE.into(),
            false => sym::NIL.into(),
        };
        unsafe { Self::Out::tag_ptr(sym.get_ptr()) }
    }
}
