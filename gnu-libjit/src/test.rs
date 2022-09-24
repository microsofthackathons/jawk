#[cfg(test)]
use std::fmt::Debug;
use std::os::raw::c_void;
#[cfg(test)]
use gnu_libjit_sys::{jit_type_int, jit_type_float64, jit_type_ubyte};
#[cfg(test)]
use crate::{Abi, Context, Function, JitType, Label};

#[cfg(test)]
type TestT = Box<dyn Fn(&mut Function, &mut Context)>;

#[cfg(test)]
macro_rules! jit_int {
    () => { JitType::new(unsafe { jit_type_int } ) }
}

#[cfg(test)]
macro_rules! jit_ubyte {
    () => { JitType::new(unsafe { jit_type_ubyte } ) }
}

#[cfg(test)]
macro_rules! jit_double {
    () => { JitType::new(unsafe { jit_type_float64 } ) }
}

#[cfg(test)]
fn make_test<RetT>(test: TestT, expected: RetT, jit_type: JitType) where RetT: Debug + Default + PartialEq {
    let mut context = Context::new();
    context.build_start();
    let mut func = context.function(Abi::Cdecl, jit_type, vec![]).unwrap();
    test(&mut func, &mut context);
    println!("{}", func.dump().unwrap());
    func.compile();
    context.build_end();
    assert_eq!(func.to_closure::<fn() -> RetT>()(), expected);
}

#[test]
fn test_const() {
    use crate::{Function, Context};
    let test = |func: &mut Function, _context: &mut Context| {
        let zero = func.create_long_constant(0);
        func.insn_return(&zero);
    };
    make_test(Box::new(test), 0, jit_int!());
}

#[test]
fn test_and_falsy() {
    use crate::{Function, Context};
    let test = |func: &mut Function, _context: &mut Context| {
        let zero = func.create_int_constant(0);
        let one = func.create_int_constant(1);
        let res = func.insn_and(&zero, &one);
        func.insn_return(&res);
    };
    make_test(Box::new(test), 0, jit_int!())
}

#[test]
fn test_and_truthy() {
    use crate::{Function, Context};
    let test = |func: &mut Function, _context: &mut Context| {
        let one = func.create_int_constant(1);
        let res = func.insn_and(&one, &one);
        func.insn_return(&res);
    };
    make_test(Box::new(test), 1, jit_int!())
}

#[test]
fn test_and_bitwise() {
    use crate::{Function, Context};
    let test = |func: &mut Function, _context: &mut Context| {
        let a = func.create_int_constant(13);
        let b = func.create_int_constant(4);
        let res = func.insn_and(&a, &b);
        func.insn_return(&res);
    };
    make_test(Box::new(test), 4, jit_int!())
}

#[test]
fn test_or() {
    use crate::{Function, Context};
    let test = |func: &mut Function, _context: &mut Context| {
        let zero = func.create_int_constant(0);
        let one = func.create_int_constant(1);
        let res = func.insn_or(&zero, &one);
        func.insn_return(&res);
    };
    make_test(Box::new(test), 1, jit_int!())
}

#[test]
fn test_xor() {
    use crate::{Function, Context};
    let test = |func: &mut Function, _context: &mut Context| {
        let zero = func.create_int_constant(0);
        let one = func.create_int_constant(1);
        let res = func.insn_or(&zero, &one);
        func.insn_return(&res);
    };
    make_test(Box::new(test), 1, jit_int!())
}

#[test]
fn test_xor_equal() {
    use crate::{Function, Context};
    let test = |func: &mut Function, _context: &mut Context| {
        let a = func.create_int_constant(1);
        let b = func.create_int_constant(1);
        let res = func.insn_xor(&a, &b);
        func.insn_return(&res);
    };
    make_test(Box::new(test), 0, jit_int!())
}

#[test]
fn test_or_bitwise() {
    use crate::{Function, Context};
    let test = |func: &mut Function, _context: &mut Context| {
        let a = func.create_int_constant(1);
        let b = func.create_int_constant(2);
        let res = func.insn_or(&a, &b);
        func.insn_return(&res);
    };
    make_test(Box::new(test), 3, jit_int!())
}

#[test]
fn test_not() {
    use crate::{Function, Context};
    let test = |func: &mut Function, _context: &mut Context| {
        let a = func.create_ubyte_constant(0);
        let res = func.insn_not(&a);
        func.insn_return(&res);
    };
    make_test::<u8>(Box::new(test), 255, jit_ubyte!())
}

#[test]
fn test_not_striped() {
    use crate::{Function, Context};
    let test = |func: &mut Function, _context: &mut Context| {
        let a = func.create_ubyte_constant(0b01010101);
        let res = func.insn_not(&a);
        func.insn_return(&res);
    };
    make_test::<u8>(Box::new(test), 0b10101010, jit_ubyte!())
}


#[test]
fn test_add_int() {
    use crate::{Function, Context};
    let test = |func: &mut Function, _context: &mut Context| {
        let three = func.create_long_constant(3);
        let one = func.create_long_constant(1);
        let result = func.insn_add(&three, &one);
        func.insn_return(&result);
    };
    make_test(Box::new(test), 4, jit_int!());
}

#[test]
fn test_sub_int() {
    use crate::{Function, Context};
    let test = |func: &mut Function, _context: &mut Context| {
        let three = func.create_long_constant(3);
        let one = func.create_long_constant(1);
        let result = func.insn_sub(&one, &three);
        func.insn_return(&result);
    };
    make_test(Box::new(test), -2, jit_int!());
}

#[test]
fn test_mult_int() {
    use crate::{Function, Context};
    let test = |func: &mut Function, _context: &mut Context| {
        let a = func.create_long_constant(3);
        let b = func.create_long_constant(100);
        let result = func.insn_mult(&a, &b);
        func.insn_return(&result);
    };
    make_test(Box::new(test), 300, jit_int!());
}

#[test]
fn test_div_int() {
    use crate::{Function, Context};
    let test = |func: &mut Function, _context: &mut Context| {
        let a = func.create_long_constant(300);
        let b = func.create_long_constant(100);
        let result = func.insn_div(&a, &b);
        func.insn_return(&result);
    };
    make_test(Box::new(test), 3, jit_int!());
}


#[test]
fn test_add_double() {
    use crate::{Function, Context};
    let test = |func: &mut Function, _context: &mut Context| {
        let a = func.create_float64_constant(1.0);
        let b = func.create_float64_constant(1.0);
        let result = func.insn_add(&a, &b);
        func.insn_return(&result);
    };
    make_test(Box::new(test), 2.0, jit_double!());
}

#[test]
fn test_sub_double() {
    use crate::{Function, Context};
    let test = |func: &mut Function, _context: &mut Context| {
        let three = func.create_float64_constant(3.0);
        let one = func.create_float64_constant(1.0);
        let result = func.insn_sub(&one, &three);
        func.insn_return(&result);
    };
    make_test(Box::new(test), -2.0, jit_double!());
}

#[test]
fn test_mult_double() {
    use crate::{Function, Context};
    let test = |func: &mut Function, _context: &mut Context| {
        let a = func.create_float64_constant(3.0);
        let b = func.create_float64_constant(100.0);
        let result = func.insn_mult(&a, &b);
        func.insn_return(&result);
    };
    make_test(Box::new(test), 300.0, jit_double!());
}

#[test]
fn test_div_double() {
    use crate::{Function, Context};
    let test = |func: &mut Function, _context: &mut Context| {
        let a = func.create_float64_constant(300.0);
        let b = func.create_float64_constant(100.0);
        let result = func.insn_div(&a, &b);
        func.insn_return(&result);
    };
    make_test(Box::new(test), 3.0, jit_double!());
}

#[test]
fn test_le() {
    use crate::{Function, Context};
    let test = |func: &mut Function, _context: &mut Context| {
        let a = func.create_float64_constant(300.0);
        let b = func.create_float64_constant(100.0);
        let result = func.insn_lt(&a, &b);
        func.insn_return(&result);
    };
    make_test(Box::new(test), 0, jit_int!());
}

#[test]
fn test_le_2() {
    use crate::{Function, Context};
    let test = |func: &mut Function, _context: &mut Context| {
        let a = func.create_float64_constant(300.0);
        let b = func.create_float64_constant(300.0);
        let result = func.insn_le(&b, &a);
        func.insn_return(&result);
    };
    make_test(Box::new(test), 1, jit_int!());
}

#[test]
fn test_le_3() {
    use crate::{Function, Context};
    let test = |func: &mut Function, _context: &mut Context| {
        let a = func.create_float64_constant(300.0);
        let b = func.create_float64_constant(100.0);
        let result = func.insn_le(&b, &a);
        func.insn_return(&result);
    };
    make_test(Box::new(test), 1, jit_int!());
}

#[test]
fn test_lt() {
    use crate::{Function, Context};
    let test = |func: &mut Function, _context: &mut Context| {
        let a = func.create_float64_constant(300.0);
        let b = func.create_float64_constant(100.0);
        let result = func.insn_lt(&a, &b);
        func.insn_return(&result);
    };
    make_test(Box::new(test), 0, jit_int!());
}

#[test]
fn test_lt_2() {
    use crate::{Function, Context};
    let test = |func: &mut Function, _context: &mut Context| {
        let a = func.create_float64_constant(300.0);
        let b = func.create_float64_constant(300.0);
        let result = func.insn_lt(&b, &a);
        func.insn_return(&result);
    };
    make_test(Box::new(test), 0, jit_int!());
}

#[test]
fn test_lt_3() {
    use crate::{Function, Context};
    let test = |func: &mut Function, _context: &mut Context| {
        let a = func.create_float64_constant(300.0);
        let b = func.create_float64_constant(100.0);
        let result = func.insn_lt(&b, &a);
        func.insn_return(&result);
    };
    make_test(Box::new(test), 1, jit_int!());
}

#[test]
fn test_branching() {
    let mut context = Context::new();
    context.build_start();
    let float_type = Context::float64_type();
    let mut func = context.function(Abi::Cdecl, float_type, vec![float_type]).unwrap();

    // Return 1 if arg0 == 4
    // else return 0
    let is_four_result = func.create_float64_constant(1.0);
    let not_four_result = func.create_float64_constant(0.0);

    let x = func.arg(0).unwrap();
    let four = func.create_float64_constant(4.0);
    let mut label = Label::new();
    let eq_to_4 = func.insn_eq(&x, &four);
    func.insn_branch_if(&eq_to_4, &mut label);
    func.insn_return(&not_four_result);
    func.insn_label(&mut label);
    func.insn_return(&is_four_result);
    func.compile();
    context.build_end();
    let result: extern "C" fn(f64) -> f64 = func.to_closure();
    assert_eq!(result(4.0), 1.0);
    assert_eq!(result(4.1), 0.0);
    assert_eq!(result(-10004.1), 0.0);
}

#[test]
fn test_branching_on_u8() {
    let mut context = Context::new();
    context.build_start();
    let ubyte_type = Context::ubyte_type();
    let mut func = context.function(Abi::Cdecl, ubyte_type, vec![ubyte_type]).unwrap();

    // Return 10 if arg == 0
    // Return 20 if arg == 1
    // Return 30 if arg == 2
    // By branching not doing math
    let zero = func.create_ubyte_constant(0);
    let one = func.create_ubyte_constant(1);
    let n_10 = func.create_ubyte_constant(10);
    let n_20 = func.create_ubyte_constant(20);
    let n_30 = func.create_ubyte_constant(30);

    let arg1 = func.arg(0).unwrap();
    let is_zero = func.insn_eq(&zero, &arg1);
    let mut not_zero_lbl = Label::new();
    func.insn_branch_if_not(&is_zero, &mut not_zero_lbl);
    func.insn_return(&n_10);

    func.insn_label(&mut not_zero_lbl);
    let is_one = func.insn_eq(&one, &arg1);
    let mut not_one_lbl = Label::new();
    func.insn_branch_if_not(&is_one, &mut not_one_lbl);
    func.insn_return(&n_20);

    func.insn_label(&mut not_one_lbl);
    func.insn_return(&n_30);

    func.compile();
    // println!("{}",func.dump().unwrap());
    // context.build_end();

    let result: extern "C" fn(i8) -> i8 = func.to_closure();
    assert_eq!(result(0), 10);
    assert_eq!(result(1), 20);
    assert_eq!(result(2), 30);
}

#[cfg(test)]
extern "C" fn add_one_to_value(value: *mut i8) {
    unsafe {
        *value += 1
    }
}

#[test]
fn test_native_func_passing_a_ptr_over_ffi() {
    let mut value: i8 = 10;
    assert_eq!(value, 10);
    let ptr_to_value = (&mut value as *mut i8) as *mut libc::c_void;
    let mut context = Context::new();
    context.build_start();
    let ubyte_type = Context::ubyte_type();
    let mut func = context.function(Abi::Cdecl, ubyte_type, vec![ubyte_type]).unwrap();
    let ptr_constant = func.create_void_ptr_constant(ptr_to_value);
    let zero = func.create_ubyte_constant(0);
    func.insn_call_native(add_one_to_value as *mut libc::c_void, vec![ptr_constant], None, Abi::Cdecl);
    func.insn_return(&zero);
    func.compile();
    let result: extern "C" fn(i8) -> i8 = func.to_closure();
    result(0);
    assert_eq!(value, 11);
    result(0);
    assert_eq!(value, 12);
}


#[cfg(test)]
fn ret_f64() -> f64 {
    123.123
}

#[test]
fn test_native_with_ret_type() {
    let mut context = Context::new();
    context.build_start();
    let mut func = context.function(Abi::Cdecl, Context::float64_type(), vec![Context::float64_type()]).unwrap();
    let ret = func.insn_call_native(ret_f64 as *mut libc::c_void, vec![], Some(Context::float64_type()), Abi::Cdecl);
    func.insn_return(&ret);
    func.compile();
    context.build_end();
    let result: extern "C" fn() -> f64 = func.to_closure();
    assert_eq!(result(), 123.123);
}


#[test]
fn fn_test_load_and_store() {
    let mut context = Context::new();
    context.build_start();


    let float_type = Context::float64_type();
    let params = vec![float_type];
    let mut func = context.function(Abi::Cdecl, float_type, params).unwrap();

    let x = func.arg(0).unwrap();
    let float_ptr_1 = func.alloca(8);
    let float_ptr_2 = func.alloca(8);

    let const_dbl = func.create_float64_constant(123.0);
    func.insn_store(&float_ptr_2, &const_dbl);

    func.insn_store(&float_ptr_1, &x);
    let f1 = func.insn_load(&float_ptr_1);

    func.insn_store(&float_ptr_1, &f1);
    let f2 = func.insn_load(&float_ptr_1);

    func.insn_store(&float_ptr_1, &f2);
    let f3 = func.insn_load(&float_ptr_1);

    let const_dbl2 = func.insn_load(&float_ptr_2);

    let x_plus_123 = func.insn_add(&const_dbl2, &f3);

    func.insn_return(&x_plus_123);
    func.compile();
    context.build_end();

    let result: extern "C" fn(f64) -> f64 = func.to_closure();
    assert_eq!(result(1.0), 124.0);
}

#[test]
fn test_unconditional_branch() {
    let mut context = Context::new();
    context.build_start();
    let int_type = Context::int_type();
    let params = vec![];
    let mut func = context.function(Abi::Cdecl, int_type, params).unwrap();
    let mut lbl = Label::new();
    func.insn_branch(&mut lbl);
    let ten = func.create_int_constant(10);
    func.insn_return(&ten);
    func.insn_label(&mut lbl);
    let twenty = func.create_int_constant(20);
    func.insn_return(&twenty);
    func.compile();
    context.build_end();
    let result: extern "C" fn() -> i32 = func.to_closure();
    assert_eq!(result(), 20);
}

#[test]
fn test_load_relative() {
    let floats = vec![1.1, 2.2, 3.3];
    let ptr = floats.as_slice().as_ptr();

    let mut context = Context::new();
    context.build_start();
    let f64_t = Context::float64_type();
    let ptr_t = Context::void_ptr_type();
    let params = vec![ptr_t];
    let mut func = context.function(Abi::Cdecl, f64_t, params).unwrap();

    let arg0 = func.arg(0).unwrap();
    let loaded1 = func.insn_load_relative(&arg0, 8, &Context::float64_type());
    func.insn_return(&loaded1);

    func.compile();
    context.build_end();

    let result: extern "C" fn(*const f64) -> f64 = func.to_closure();
    assert_eq!(result(ptr), 2.2);
    assert_eq!(result(unsafe { ptr.offset(1) }), 3.3);
    assert_eq!(result(unsafe { ptr.offset(-1) }), 1.1);
}

#[test]
fn test_calls() {
    let mut context = Context::new();
    context.build_start();

    // This function multiplies int by 2
    let int_type = Context::int_type();
    let mut func_mult_by_2 = context.function(Abi::Cdecl, int_type, vec![int_type]).unwrap();
    let x = func_mult_by_2.arg(0).unwrap();
    let const_1 = func_mult_by_2.create_int_constant(2);
    let temp1 = func_mult_by_2.insn_mult(&x, &const_1);
    func_mult_by_2.insn_return(&temp1);
    func_mult_by_2.compile();

    // This main function has 1 arg, it adds 5 to the arg and calls func_mult_by_2 and returns that value.
    let mut func = context.function(Abi::Cdecl, int_type, vec![int_type]).unwrap();
    let arg0 = func.arg(0).unwrap();
    let five = func.create_int_constant(5);
    let arg0_plus_5 = func.insn_add(&arg0, &five);
    let res = func.insn_call(&func_mult_by_2, vec![arg0_plus_5]);
    func.insn_return(&res);
    func.compile();

    context.build_end();

    let result: extern "C" fn(i32) -> i32 = func.to_closure();
    assert_eq!(result(100), 210);
    assert_eq!(result(-5), 0);
    assert_eq!(result(-4), 2);
}

#[test]
fn test_exponential() {
    let mut context = Context::new();
    context.build_start();

    // This function multiplies int by 2
    let float_type = Context::float64_type();
    let mut func = context.function(Abi::Cdecl, float_type, vec![float_type, float_type]).unwrap();
    let arg1 = func.arg(0).unwrap();
    let arg2 = func.arg(1).unwrap();
    let ret = func.insn_pow(&arg1, &arg2);
    func.insn_return(&ret);
    func.compile();

    context.build_end();

    let result: extern "C" fn(f64, f64) -> f64 = func.to_closure();
    assert_eq!(result(3.0, 4.0), 81.0);
    assert_eq!(result(-5.0, 0.0), 1.0);
    assert_eq!(result(5.0, 2.0), 25.0);
}