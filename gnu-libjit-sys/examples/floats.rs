use gnu_libjit_sys::{_jit_type, jit_abi_t_jit_abi_cdecl, jit_context_build_end, jit_context_build_start, jit_context_create, jit_function_apply, jit_function_compile, jit_function_create, jit_insn_add, jit_insn_mul, jit_insn_rem, jit_insn_rethrow_unhandled, jit_insn_return, jit_type_create_signature, jit_type_float64, jit_type_free, jit_type_int, jit_type_t, jit_value_get_param};
use libc::{c_double, c_int};
use libc::c_void;

macro_rules! float_to_void {
    // Cast from &mut int to a pointer to a mut c_int
    // then to a *mut c_void
    ($int:ident) => {
        ((&mut $int as *mut libc::c_double) as *mut libc::c_void)
    }
}
fn main() {
    unsafe {
        let context = jit_context_create();
        jit_context_build_start(context);
        let mut params: &mut [*mut _jit_type; 3] = &mut [jit_type_float64, jit_type_float64, jit_type_float64];
        let signature = jit_type_create_signature(
            jit_abi_t_jit_abi_cdecl,
            jit_type_float64,
            params.as_mut_ptr(),
            3,
            1,
        );
        let function = jit_function_create(context, signature);
        jit_type_free(signature);

        let x = jit_value_get_param(function, 0);
        let y = jit_value_get_param(function, 1);
        let z = jit_value_get_param(function, 2);
        let temp1 = jit_insn_mul(function, x, y);
        let temp2 = jit_insn_add(function, temp1, z);
        jit_insn_return(function, temp2);
        jit_function_compile(function);
        jit_context_build_end(context);

        let mut a1: c_double = 1.0;
        let mut a2: c_double = 2.0;
        let mut a3: c_double = 3.0;
        let mut args = &mut [float_to_void!(a1), float_to_void!(a2), float_to_void!(a3)];
        let mut result: c_double = -1.8;
        jit_function_apply(function,
                           args.as_mut_ptr(),
                           float_to_void!(result));
        println!("1.0+2.0+3.0 = {}", result)
    }
}
