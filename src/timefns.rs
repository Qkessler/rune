//! Time analysis
use std::time::SystemTime;

use rune_core::{
    env::{sym, Env},
    gc::{Context, Rt},
    macros::list,
    object::GcObj,
};
use rune_macros::defun;

#[defun]
fn current_time<'ob>(cx: &'ob Context, env: &Rt<Env>) -> GcObj<'ob> {
    assert!(
        env.vars.get(sym::CURRENT_TIME_LIST).unwrap() == &sym::TRUE,
        "current-time-list is nil"
    );
    let duration = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .expect("System time is before the epoch");

    let secs = duration.as_secs();
    let micros = duration.subsec_micros();
    let low = secs & 0xffff;
    let high = secs >> 16;

    list![high, low, micros, 0; cx]
}
