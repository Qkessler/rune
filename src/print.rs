//! Printing utilities.
use rune_core::object::GcObj;
use rune_macros::defun;

#[defun]
fn error_message_string(obj: GcObj) -> String {
    // TODO: implement
    format!("Error: {obj}")
}
