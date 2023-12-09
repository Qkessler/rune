//! Arithmetic operators.
use rune_core::object::{Gc, Number, NumberValue};
use rune_macros::defun;
use std::cmp::PartialOrd;

//////////////////////////
// Arithmetic operators //
//////////////////////////

#[defun(name = "+")]
pub(crate) fn add(vars: &[Gc<Number>]) -> NumberValue {
    vars.iter().fold(NumberValue::Int(0), |acc, x| acc + x.val())
}

#[defun(name = "-")]
pub(crate) fn sub(number: Option<Gc<Number>>, numbers: &[Gc<Number>]) -> NumberValue {
    match number {
        Some(num) => {
            let num = num.val();
            if numbers.is_empty() {
                -num
            } else {
                numbers.iter().fold(num, |acc, x| acc - x.val())
            }
        }
        None => NumberValue::Int(0),
    }
}

#[defun(name = "*")]
pub(crate) fn mul(numbers: &[Gc<Number>]) -> NumberValue {
    numbers.iter().fold(NumberValue::Int(1), |acc, x| acc * x.val())
}

#[defun(name = "/")]
pub(crate) fn div(number: Gc<Number>, divisors: &[Gc<Number>]) -> NumberValue {
    divisors.iter().fold(number.val(), |acc, x| acc / x.val())
}

#[defun(name = "1+")]
pub(crate) fn add_one(number: Gc<Number>) -> NumberValue {
    number.val() + NumberValue::Int(1)
}

#[defun(name = "1-")]
pub(crate) fn sub_one(number: Gc<Number>) -> NumberValue {
    number.val() - NumberValue::Int(1)
}

#[defun(name = "=")]
pub(crate) fn num_eq(number: Gc<Number>, numbers: &[Gc<Number>]) -> bool {
    match number.val() {
        NumberValue::Int(num) => numbers.iter().all(|&x| x == num),
        NumberValue::Float(num) => numbers.iter().all(|&x| x == num),
    }
}

#[defun(name = "/=")]
#[allow(clippy::float_cmp)] // This is a bug in clippy, we are not comparing floats directly
pub(crate) fn num_ne(number: Gc<Number>, numbers: &[Gc<Number>]) -> bool {
    match number.val() {
        NumberValue::Int(num) => numbers.iter().all(|&x| x != num),
        NumberValue::Float(num) => numbers.iter().all(|&x| x != num),
    }
}

fn cmp(
    number: Gc<Number>,
    numbers: &[Gc<Number>],
    cmp: fn(&NumberValue, &NumberValue) -> bool,
) -> bool {
    numbers
        .iter()
        .try_fold(number.val(), |acc, &x| cmp(&acc, &x.val()).then_some(NumberValue::Int(0)))
        .is_some()
}

#[defun(name = "<")]
pub(crate) fn less_than(number: Gc<Number>, numbers: &[Gc<Number>]) -> bool {
    cmp(number, numbers, NumberValue::lt)
}

#[defun(name = "<=")]
pub(crate) fn less_than_or_eq(number: Gc<Number>, numbers: &[Gc<Number>]) -> bool {
    cmp(number, numbers, NumberValue::le)
}

#[defun(name = ">")]
pub(crate) fn greater_than(number: Gc<Number>, numbers: &[Gc<Number>]) -> bool {
    cmp(number, numbers, NumberValue::gt)
}

#[defun(name = ">=")]
pub(crate) fn greater_than_or_eq(number: Gc<Number>, numbers: &[Gc<Number>]) -> bool {
    cmp(number, numbers, NumberValue::ge)
}

#[defun]
pub(crate) fn logior(ints_or_markers: &[Gc<i64>]) -> i64 {
    ints_or_markers.iter().fold(0, |acc, x| acc | x.untag())
}

#[defun]
fn logand(int_or_markers: &[Gc<i64>]) -> i64 {
    int_or_markers.iter().fold(-1, |accum, x| accum & x.untag())
}

#[defun(name = "mod")]
pub(crate) fn modulo(x: Gc<Number>, y: Gc<Number>) -> NumberValue {
    x.val() % y.val()
}

#[defun(name = "%")]
pub(crate) fn remainder(x: i64, y: i64) -> i64 {
    // TODO: Handle markers
    x % y
}

#[allow(clippy::trivially_copy_pass_by_ref)]
fn max_val(x: NumberValue, y: &Gc<Number>) -> NumberValue {
    let y = y.val();
    if x > y {
        x
    } else {
        y
    }
}

#[allow(clippy::trivially_copy_pass_by_ref)]
fn min_val(x: NumberValue, y: &Gc<Number>) -> NumberValue {
    let y = y.val();
    if x < y {
        x
    } else {
        y
    }
}

#[defun]
pub(crate) fn max(number_or_marker: Gc<Number>, number_or_markers: &[Gc<Number>]) -> NumberValue {
    number_or_markers.iter().fold(number_or_marker.val(), max_val)
}

#[defun]
pub(crate) fn min(number_or_marker: Gc<Number>, number_or_markers: &[Gc<Number>]) -> NumberValue {
    number_or_markers.iter().fold(number_or_marker.val(), min_val)
}

#[cfg(test)]
mod test {
    use super::*;
    use rune_core::gc::{Context, RootSet};
    use rune_core::object::IntoObject;

    #[test]
    fn test_add() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        assert_eq!(add(&[]), NumberValue::Int(0));
        assert_eq!(add(&[7.into(), 13.into()]), NumberValue::Int(20));
        assert_eq!(add(&[1.into(), cx.add_as(2.5)]), NumberValue::Float(3.5));
        assert_eq!(add(&[0.into(), (-1).into()]), NumberValue::Int(-1));
    }

    #[test]
    fn test_sub() {
        assert_eq!(sub(None, &[]), NumberValue::Int(0));
        assert_eq!(sub(Some(7.into()), &[]), NumberValue::Int(-7));
        assert_eq!(sub(Some(7.into()), &[13.into()]), NumberValue::Int(-6));
        assert_eq!(sub(Some(0.into()), &[(-1).into()]), NumberValue::Int(1));
    }

    #[test]
    fn test_mul() {
        assert_eq!(mul(&[]), NumberValue::Int(1));
        assert_eq!(mul(&[7.into(), 13.into()]), NumberValue::Int(91));
        assert_eq!(mul(&[(-1).into(), 1.into()]), NumberValue::Int(-1));
    }

    #[test]
    fn test_div() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);

        assert_eq!(div(cx.add_as(12.0), &[]), NumberValue::Float(12.0));
        assert_eq!(div(12.into(), &[5.into(), 2.into()]), NumberValue::Int(1));
    }

    #[test]
    fn test_eq() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        let int1 = 1.into();
        let float1 = cx.add_as(1.0);
        let float1_1 = cx.add_as(1.1);

        assert!(num_eq(int1, &[]));
        assert!(num_eq(int1, &[cx.add_as(1.0)]));
        assert!(num_eq(float1, &[1.into()]));
        assert!(!num_eq(float1, &[1.into(), 1.into(), float1_1]));
    }

    #[test]
    fn test_cmp() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        assert!(less_than(1.into(), &[]));
        assert!(less_than(1.into(), &[cx.add_as(1.1)]));
        assert!(!less_than(cx.add_as(1.0), &[1.into()]));
        assert!(less_than(cx.add_as(1.0), &[cx.add_as(1.1), 2.into(), cx.add_as(2.1)]));
    }

    #[test]
    fn test_max_min() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        assert_eq!(
            max(cx.add_as(1.0), &[cx.add_as(2.1), cx.add_as(1.1), cx.add_as(1.0)]),
            cx.add_as(2.1).val()
        );
        assert_eq!(
            min(cx.add_as(1.1), &[cx.add_as(1.0), cx.add_as(2.1), cx.add_as(1.0)]),
            cx.add_as(1.0).val()
        );
    }

    #[test]
    fn test_other() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        assert_eq!(logand(&[258.into_obj(cx), 255.into_obj(cx)]), 2);
    }
}
