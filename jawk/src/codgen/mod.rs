mod scopes;
// mod runtime;
// mod subroutines;
pub mod variable_extract;

pub use value::{ValuePtrT, ValueT};

mod subroutines;
mod value;

use crate::codgen::scopes::Scopes;
use crate::codgen::subroutines::Subroutines;
use crate::lexer::{BinOp, LogicalOp, MathOp};
use crate::parser::{AwkT, Stmt, TypedExpr};
use crate::printable_error::PrintableError;
use crate::runtime::{LiveRuntime, Runtime, TestRuntime};
use crate::Expr;
use gnu_libjit::{Abi, Context, Function, Label, Value};
use std::collections::HashSet;
use std::os::raw::{c_char, c_long, c_void};
use std::rc::Rc;

/// ValueT is the jit values that make up a struct. It's not a tagged union
/// just a struct with only one other field being valid to read at a time based on the tag field.
///
/// ValueT {
///     tag: u8
///     float: f64
///     string: *mut c_void
/// }

pub const FLOAT_TAG: u8 = 0;
pub const STRING_TAG: u8 = 1;

// Entry point to run a program
pub fn compile_and_run(prog: Stmt, files: &[String]) -> Result<(), PrintableError> {
    let mut runtime = LiveRuntime::new(files.to_vec());
    let mut codegen = CodeGen::new(&mut runtime);
    codegen.compile(prog, false)?;
    codegen.run();
    Ok(())
}

// Entry point to run and debug/test a program. Use the test runtime.
pub fn compile_and_capture(prog: Stmt, files: &[String]) -> Result<TestRuntime, PrintableError> {
    let mut test_runtime = TestRuntime::new(files.to_vec());
    let mut codegen = CodeGen::new(&mut test_runtime);
    codegen.compile(prog, true)?;
    codegen.run();
    Ok(test_runtime)
}

struct CodeGen<'a, RuntimeT: Runtime> {
    // Core stuff
    pub(crate) function: Function,
    scopes: Scopes,
    // Stores the points to each global variable in the program
    pub(crate) context: Context,
    // The jit context
    runtime: &'a mut RuntimeT, // Runtime provides native functions and may be used for debugging.

    // Used for commonly reused snippets like string-truthyness etc.
    subroutines: Subroutines,

    // These are effectively stack variables that we use as scratch space.
    binop_scratch: ValuePtrT,
    // complete value
    binop_scratch_int: Value, // just an int (generally 0 or 1 used for insn_eq ne comparisons)

    // Used to init the pointer section of the value struct when it's undefined. Should never be dereferenced.
    zero_ptr: Value,
    // Used to init the float section of value. Safe to use but using it is a bug.
    zero_f: Value,

    // To avoid creating tons of constants just reuse the tags here
    float_tag: Value,
    string_tag: Value,
}

impl<'a, RuntimeT: Runtime> CodeGen<'a, RuntimeT> {
    fn new(runtime: &'a mut RuntimeT) -> Self {
        let mut context = Context::new();
        let mut function = context
            .function(Abi::Cdecl, Context::float64_type(), vec![])
            .expect("to create function");

        let zero_ptr = Box::into_raw(Box::new("".to_string())) as *mut c_void;
        let zero_ptr = function.create_void_ptr_constant(zero_ptr);
        let zero_f = function.create_float64_constant(0.0);
        let float_tag = function.create_sbyte_constant(FLOAT_TAG as c_char);
        let string_tag = function.create_sbyte_constant(STRING_TAG as c_char);

        let binop_scratch_int = function.create_value_int();
        let binop_scratch = ValueT::new(
            function.create_value_int(),
            function.create_value_float64(),
            function.create_value_void_ptr(),
        );
        let subroutines = Subroutines::new(&mut context, runtime);
        let codegen = CodeGen {
            function,
            scopes: Scopes::new(),
            context,
            runtime,
            subroutines,
            binop_scratch,
            binop_scratch_int,
            zero_ptr,
            zero_f,
            float_tag,
            string_tag,
        };
        codegen
    }

    fn run(&mut self) {
        let function: extern "C" fn() = self.function.to_closure();
        function();
    }

    fn compile(&mut self, prog: Stmt, dump: bool) -> Result<(), PrintableError> {
        let zero = self.function.create_float64_constant(0.0);
        let vars = self.define_all_vars(&prog)?;
        self.compile_stmt(&prog);

        // This is just so # strings allocated == # of strings freed which makes testing easier
        for var in vars {
            let var_ptrs = self.scopes.get(&var).clone();
            self.drop_if_string_ptr(&var_ptrs, AwkT::Variable);
        }

        self.function.insn_return(&zero);
        self.context.build_end();
        if dump {
            println!("{}", self.function.dump().unwrap());
        }
        self.function.compile();
        Ok(())
    }

    // Helpers for commonly used values
    fn float_tag(&self) -> Value {
        self.float_tag.clone()
    }
    fn string_tag(&self) -> Value {
        self.string_tag.clone()
    }
    fn zero_f(&self) -> Value {
        self.zero_f.clone()
    }

    fn define_all_vars(&mut self, prog: &Stmt) -> Result<HashSet<String>, PrintableError> {
        // All variables are init'ed to the empty string.
        let (vars, string_constants) = variable_extract::extract(prog);
        for var in &vars {
            let tag = self.function.create_value_int();
            self.function.insn_store(&tag, &self.string_tag);

            let ptr_value = self.function.create_value_void_ptr();
            let ptr = self.runtime.empty_string(&mut self.function);
            self.function.insn_store(&ptr_value, &ptr);

            let float_value = self.function.create_value_float64();
            self.function.insn_store(&float_value, &self.zero_f);

            let val = ValueT::new(tag, float_value, ptr);
            self.scopes.insert(var.clone(), val)?;
        }

        // All string constants like a in `print "a"`; are stored in a variable
        // the name of the variable is " a". Just a space in front to prevent collisions.
        for str_const in string_constants {
            let space_in_front = format!(" {}", str_const);
            let tag = self.function.create_value_int();
            self.function.insn_store(&tag, &self.string_tag);

            let ptr = Rc::into_raw(Rc::new(str_const)) as *mut c_void;
            let ptr = self.function.create_void_ptr_constant(ptr);
            let ptr_value = self.function.create_value_void_ptr();
            self.function.insn_store(&ptr_value, &ptr);

            let float_value = self.function.create_value_float64();
            self.function.insn_store(&float_value, &self.zero_f);

            let val = ValueT::new(tag, float_value, ptr);
            self.scopes.insert(space_in_front, val)?;
        }
        Ok(vars)
    }

    fn float_is_truthy_ret_int(&mut self, value: &Value) -> Value {
        let zero_f = self.function.create_float64_constant(0.0);
        self.function.insn_ne(&value, &zero_f)
    }

    fn to_float(&mut self, value: &ValueT, typ: AwkT) -> Value {
        if typ == AwkT::Float {
            return value.float.clone();
        }
        self.function.insn_call(
            &self.subroutines.to_float(&mut self.context, self.runtime),
            value.into(),
        )
    }

    fn to_string(&mut self, value: &ValueT, typ: AwkT) -> Value {
        if typ == AwkT::String {
            return value.pointer.clone();
        }

        let mut done_lbl = Label::new();
        self.function
            .insn_store(&self.binop_scratch.pointer, &value.pointer);
        let is_string = self.function.insn_eq(&value.tag, &self.string_tag);
        self.function.insn_branch_if(&is_string, &mut done_lbl);

        let new_string = self
            .runtime
            .number_to_string(&mut self.function, value.float.clone());
        self.function
            .insn_store(&self.binop_scratch.pointer, &new_string);

        self.function.insn_label(&mut done_lbl);
        let ptr = self.function.insn_load(&self.binop_scratch.pointer);
        ptr
    }

    // Free the value in the value pointer if it's a string
    fn drop_if_string_ptr(&mut self, value: &ValuePtrT, typ: AwkT) {
        if let AwkT::Float = typ {
            return;
        }
        let value = self.load(&value);
        self.drop_if_str(&value, typ)
    }

    // Free the value if it's a string
    fn drop_if_str(&mut self, value: &ValueT, typ: AwkT) {
        match typ {
            AwkT::String => {
                self.drop(&value.pointer);
            }
            AwkT::Variable => {
                let str_tag = self.string_tag();
                let mut done_lbl = Label::new();
                let is_string = self.function.insn_eq(&str_tag, &value.tag);
                self.function.insn_branch_if_not(&is_string, &mut done_lbl);
                self.drop(&value.pointer);
                self.function.insn_label(&mut done_lbl);
            }
            _ => {}
        };
    }

    fn drop(&mut self, value: &Value) {
        self.runtime.free_string(&mut self.function, value.clone());
    }

    // Take a value and return an int 0 or 1
    fn truthy_ret_integer(&mut self, value: &ValueT, typ: AwkT) -> Value {
        match typ {
            AwkT::String => {
                let string_len_offset =
                    std::mem::size_of::<usize>() + std::mem::size_of::<*const u8>();
                let string_len = self.function.insn_load_relative(
                    &value.pointer,
                    string_len_offset as c_long,
                    &Context::long_type(),
                );
                let zero_ulong = self.function.create_ulong_constant(0);
                self.function.insn_ne(&zero_ulong, &string_len)
            }
            AwkT::Float => self.float_is_truthy_ret_int(&value.float),
            AwkT::Variable => {
                let mut string_lbl = Label::new();
                let mut done_lbl = Label::new();

                let one_tag = self.string_tag();
                let tag_is_one = self.function.insn_eq(&value.tag, &one_tag);
                self.function.insn_branch_if(&tag_is_one, &mut string_lbl);

                // is float code
                let is_truthy_f = self.float_is_truthy_ret_int(&value.float);
                self.function
                    .insn_store(&self.binop_scratch_int, &is_truthy_f);
                self.function.insn_branch(&mut done_lbl);

                // is string code
                self.function.insn_label(&mut string_lbl);
                let is_truthy_str = self.function.insn_call(
                    self.subroutines
                        .string_truthy(&mut self.context, self.runtime),
                    vec![value.pointer.clone()],
                );
                self.function
                    .insn_store(&self.binop_scratch_int, &is_truthy_str);
                self.function.insn_label(&mut done_lbl);
                self.function.insn_load(&self.binop_scratch_int)
            }
        }
    }

    fn copy_if_string(&mut self, value: ValueT, typ: AwkT) -> ValueT {
        let zero = self.function.create_float64_constant(0.0);
        let str_tag = self.string_tag();
        match typ {
            AwkT::String => {
                // String all runtime
                let ptr = self.runtime.copy_string(&mut self.function, value.pointer);
                ValueT::new(str_tag, zero, ptr)
            }
            AwkT::Float => value, // Float copy is a no-op
            AwkT::Variable => {
                // If type unknown, check tag and call runtime if it's a string
                let mut done = Label::new();
                let is_string = self.function.insn_eq(&str_tag, &value.tag);
                self.function
                    .insn_store(&self.binop_scratch.pointer, &self.zero_ptr);
                self.function.insn_branch_if_not(&is_string, &mut done);
                let ptr = self.runtime.copy_string(&mut self.function, value.pointer);
                self.function.insn_store(&self.binop_scratch.pointer, &ptr);
                self.function.insn_label(&mut done);
                let string = self.function.insn_load(&self.binop_scratch.pointer);
                ValueT::new(value.tag, value.float, string)
            }
        }
    }

    fn compile_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(expr) => {
                let res = self.compile_expr(expr);
                self.drop_if_str(&res, expr.typ);
            }
            Stmt::Print(expr) => {
                let val = self.compile_expr(expr);
                // Optimize print based on static knowledge of type
                match expr.typ {
                    AwkT::String => {
                        self.runtime
                            .print_string(&mut self.function, val.pointer.clone());
                    }
                    AwkT::Float => {
                        self.runtime.print_float(&mut self.function, val.float);
                        return;
                    }
                    AwkT::Variable => {
                        let str = self.to_string(&val, expr.typ);
                        self.runtime.print_string(&mut self.function, str.clone());
                        self.runtime.free_string(&mut self.function, str);
                        return;
                    }
                }
                self.drop_if_str(&val, expr.typ);
            }
            Stmt::Group(group) => {
                for group in group {
                    self.compile_stmt(group)
                }
            }
            Stmt::If(test, if_so, if_not) => {
                if let Some(if_not) = if_not {
                    let test_value = self.compile_expr(test);
                    let bool_value = self.truthy_ret_integer(&test_value, test.typ);
                    self.drop_if_str(&test_value, test.typ);
                    let mut then_lbl = Label::new();
                    let mut done_lbl = Label::new();
                    self.function.insn_branch_if(&bool_value, &mut then_lbl);
                    self.compile_stmt(if_not);
                    self.function.insn_branch(&mut done_lbl);
                    self.function.insn_label(&mut then_lbl);
                    self.compile_stmt(if_so);
                    self.function.insn_label(&mut done_lbl);
                } else {
                    let test_value = self.compile_expr(test);
                    let bool_value = self.truthy_ret_integer(&test_value, test.typ);
                    self.drop_if_str(&test_value, test.typ);
                    let mut done_lbl = Label::new();
                    self.function.insn_branch_if_not(&bool_value, &mut done_lbl);
                    self.compile_stmt(if_so);
                    self.function.insn_label(&mut done_lbl);
                }
            }
            Stmt::While(test, body) => {
                let mut test_label = Label::new();
                let mut done_label = Label::new();
                self.function.insn_label(&mut test_label);
                let test_value = self.compile_expr(test);
                let bool_value = self.truthy_ret_integer(&test_value, test.typ);
                self.drop_if_str(&test_value, test.typ);
                self.function
                    .insn_branch_if_not(&bool_value, &mut done_label);
                self.compile_stmt(body);
                self.function.insn_branch(&mut test_label);
                self.function.insn_label(&mut done_label);
            }
        }
    }

    // When compile_expr returns a string the caller is responsible for freeing it
    fn compile_expr(&mut self, expr: &TypedExpr) -> ValueT {
        match &expr.expr {
            Expr::Assign(var, value) => {
                // BEGIN: Optimization
                // Optimization to allow reusing the string being assigned to by a string concat operation
                // a = "init"
                // a = a "abc" (We don't want to make a copy of a when we concat "abc" with it)
                // We first calculate a to be init and "abc" to "abc". This results in a copy being made
                // of "init" (increasing the reference count to 2). Then we drop a BEFORE actually doing the
                // concat.  Reference count returns to 1.
                // Now concat can re-use the original value since ref count is 1 it's safe to downgrade
                // from Rc -> Box
                if let Expr::Concatenation(vars) = &value.expr {
                    let var_ptrs = self.scopes.get(var).clone();
                    let strings_to_concat = self.compile_exprs_to_string(vars);
                    let old_value = self.load(&var_ptrs);
                    self.drop_if_str(&old_value, AwkT::Variable);
                    let new_value = self.concat_values(&strings_to_concat);
                    self.store(&var_ptrs, &new_value);
                    return self.copy_if_string(new_value, AwkT::Variable);
                }
                let new_value = self.compile_expr(value);
                let var_ptrs = self.scopes.get(var).clone();
                let old_value = self.load(&var_ptrs);
                self.drop_if_str(&old_value, AwkT::Variable);
                self.store(&var_ptrs, &new_value);
                self.copy_if_string(new_value, value.typ)
            }
            Expr::NumberF64(num) => ValueT::new(
                self.float_tag(),
                self.function.create_float64_constant(*num),
                self.zero_ptr.clone(),
            ),
            Expr::String(str) => {
                // Every string constant is stored in a variable with the name " name"
                // the space ensures we don't collide with normal variable names;
                let string_tag = self.string_tag();
                let var_ptr = self.scopes.get(&format!(" {}", str)).clone();
                let var = self.load(&var_ptr);
                let zero = self.function.create_float64_constant(0.0);
                let new_ptr = self.runtime.copy_string(&mut self.function, var.pointer);
                ValueT::new(string_tag, zero, new_ptr)
            }
            Expr::MathOp(left_expr, op, right_expr) => {
                // Convert left and right to floats if needed and perform the MathOp
                let mut left = self.compile_expr(left_expr);
                let mut right = self.compile_expr(right_expr);
                let zero = self.float_tag();

                if AwkT::Float != left_expr.typ {
                    let new_left = ValueT::new(
                        zero.clone(),
                        self.to_float(&left, left_expr.typ),
                        self.zero_ptr.clone(),
                    );
                    self.drop_if_str(&left, left_expr.typ);
                    left = new_left;
                }
                if AwkT::Float != right_expr.typ {
                    let new_right = ValueT::new(
                        zero.clone(),
                        self.to_float(&right, right_expr.typ),
                        self.zero_ptr.clone(),
                    );
                    self.drop_if_str(&right, right_expr.typ);
                    right = new_right;
                }
                let result = match op {
                    MathOp::Minus => self.function.insn_sub(&left.float, &right.float),
                    MathOp::Plus => self.function.insn_add(&left.float, &right.float),
                    MathOp::Slash => self.function.insn_div(&left.float, &right.float),
                    MathOp::Star => self.function.insn_mult(&left.float, &right.float),
                    MathOp::Modulus => self.function.insn_rem(&left.float, &right.float),
                    MathOp::Exponent => self.function.insn_pow(&left.float, &right.float),
                };
                ValueT::new(zero, result, self.zero_ptr.clone())
            }
            Expr::BinOp(left_expr, op, right_expr) => {
                let left = self.compile_expr(left_expr);
                let right = self.compile_expr(right_expr);
                let tag = self.float_tag();

                // Optimize the case where we know both are floats
                match (left_expr.typ, right_expr.typ) {
                    (AwkT::Float, AwkT::Float) => {
                        return ValueT::new(
                            tag,
                            self.float_binop(&left.float, &right.float, *op),
                            self.zero_ptr.clone(),
                        );
                    }
                    _ => {}
                }

                let left_is_float = self.function.insn_eq(&tag, &left.tag);
                let right_is_float = self.function.insn_eq(&tag, &right.tag);
                let mut both_float_lbl = Label::new();
                let mut done_lbl = Label::new();
                let both_float = self.function.insn_and(&left_is_float, &right_is_float);
                self.function
                    .insn_branch_if(&both_float, &mut both_float_lbl);

                // String/Float Float/String String/String case
                let left_as_string = self.to_string(&left, left_expr.typ);
                let right_as_string = self.to_string(&right, right_expr.typ);
                let res = self.runtime.binop(
                    &mut self.function,
                    left_as_string.clone(),
                    right_as_string.clone(),
                    *op,
                );
                let result = ValueT::new(self.float_tag.clone(), res, self.zero_ptr.clone());
                self.store(&self.binop_scratch.clone(), &result);
                self.drop(&left_as_string);
                self.drop(&right_as_string);
                self.function.insn_branch(&mut done_lbl);

                // Float/Float case
                self.function.insn_label(&mut both_float_lbl);
                let float_val = self.float_binop(&left.float, &right.float, *op);
                let value = ValueT::new(tag, float_val, self.zero_ptr.clone());
                self.store(&self.binop_scratch.clone(), &value);

                // Done load the result from scratch
                self.function.insn_label(&mut done_lbl);
                self.load(&self.binop_scratch.clone())
            }
            Expr::LogicalOp(left, op, right) => {
                let float_1 = self.function.create_float64_constant(1.0);
                let float_0 = self.function.create_float64_constant(0.0);
                // Short circuiting and and or operators.
                // Gotta be careful to free values appropriately and only when they are actually created.
                let res = match op {
                    LogicalOp::And => {
                        let mut ret_false = Label::new();
                        let mut done = Label::new();
                        let left_val = self.compile_expr(left);
                        let l = self.truthy_ret_integer(&left_val, left.typ);
                        self.drop_if_str(&left_val, left.typ);
                        self.function.insn_branch_if_not(&l, &mut ret_false);
                        let right_val = self.compile_expr(right);
                        let r = self.truthy_ret_integer(&right_val, right.typ);
                        self.drop_if_str(&right_val, right.typ);
                        self.function.insn_branch_if_not(&r, &mut ret_false);
                        self.function
                            .insn_store(&self.binop_scratch.float, &float_1);
                        self.function.insn_branch(&mut done);
                        self.function.insn_label(&mut ret_false);
                        self.function
                            .insn_store(&self.binop_scratch.float, &float_0);
                        self.function.insn_branch(&mut done);
                        self.function.insn_label(&mut done);
                        let tag = self.float_tag();
                        let result_f = self.function.insn_load(&self.binop_scratch.float);
                        ValueT::new(tag, result_f, self.zero_ptr.clone())
                    }
                    LogicalOp::Or => {
                        let mut done = Label::new();
                        let mut return_true = Label::new();
                        let left_val = self.compile_expr(left);
                        let l = self.truthy_ret_integer(&left_val, left.typ);
                        self.drop_if_str(&left_val, left.typ);
                        self.function.insn_branch_if(&l, &mut return_true);
                        let right_val = self.compile_expr(right);
                        let r = self.truthy_ret_integer(&right_val, right.typ);
                        self.drop_if_str(&right_val, right.typ);
                        self.function.insn_branch_if(&r, &mut return_true);
                        self.function
                            .insn_store(&self.binop_scratch.float, &float_0);
                        self.function.insn_branch(&mut done);
                        self.function.insn_label(&mut return_true);
                        self.function
                            .insn_store(&self.binop_scratch.float, &float_1);
                        self.function.insn_label(&mut done);
                        let tag = self.float_tag();
                        let result_f = self.function.insn_load(&self.binop_scratch.float);
                        ValueT::new(tag, result_f, self.zero_ptr.clone())
                    }
                };
                res
            }
            Expr::Variable(var) => {
                // compile_expr returns a string/float that is 'owned' by the caller.
                // If it's a string we need to call copy_string to update the reference count.
                // If it's a float no-op.
                // If type is unknown we check tag then copy_string if needed.
                let var_ptr = self.scopes.get(var).clone();
                let string_tag = self.string_tag();
                match expr.typ {
                    AwkT::String => {
                        let var = self.load(&var_ptr);
                        let zero = self.function.create_float64_constant(0.0);
                        let new_ptr = self.runtime.copy_string(&mut self.function, var.pointer);
                        ValueT::new(string_tag, zero, new_ptr)
                    }
                    AwkT::Variable => {
                        // If it's a string variable copy it and store that pointer in self.binop_scratch.pointer
                        // otherwise store zero self.binop_scratch.pointer. After this load self.binop_scratch.pointer
                        // and make a new value with the old tag/float + new string pointer.
                        let var = self.load(&var_ptr);
                        let is_not_str = self.function.insn_eq(&string_tag, &var.tag);
                        let mut done_lbl = Label::new();
                        let mut is_not_str_lbl = Label::new();
                        self.function
                            .insn_branch_if_not(&is_not_str, &mut is_not_str_lbl);
                        let new_ptr = self.runtime.copy_string(&mut self.function, var.pointer);
                        self.function
                            .insn_store(&self.binop_scratch.pointer, &new_ptr);
                        self.function.insn_branch(&mut done_lbl);

                        self.function.insn_label(&mut is_not_str_lbl);
                        self.function
                            .insn_store(&self.binop_scratch.pointer, &self.zero_ptr);

                        self.function.insn_label(&mut done_lbl);
                        let str_ptr = self.function.insn_load(&self.binop_scratch.pointer);
                        ValueT::new(var.tag, var.float, str_ptr)
                    }
                    AwkT::Float => self.load(&var_ptr),
                }
            }
            Expr::Column(col) => {
                let column = self.compile_expr(col);
                let val = self.runtime.column(
                    &mut self.function,
                    column.tag.clone(),
                    column.float.clone(),
                    column.pointer.clone(),
                );
                let tag = self.string_tag();
                self.drop_if_str(&column, col.typ);
                ValueT::new(tag, self.function.create_float64_constant(0.0), val)
            }
            Expr::Call => {
                // Ask runtime if there is a next line. Returns a float 0 or 1
                let one = self.float_tag();
                let next_line_exists = self.runtime.call_next_line(&mut self.function);
                ValueT::new(one, next_line_exists, self.zero_ptr.clone())
            }
            Expr::Concatenation(vars) => {
                // Eg: a = "a" "b" "c"
                let compiled = self.compile_exprs_to_string(vars);
                self.concat_values(&compiled)
            }
            Expr::Ternary(cond, expr1, expr2) => {
                let mut done_lbl = Label::new();
                let mut truthy_lbl = Label::new();

                let result = self.compile_expr(cond);
                let bool_value = self.truthy_ret_integer(&result, cond.typ);

                self.function.insn_branch_if(&bool_value, &mut truthy_lbl);

                let falsy_result = self.compile_expr(expr2);
                self.store(&self.binop_scratch.clone(), &falsy_result);
                self.function.insn_branch(&mut done_lbl);

                self.function.insn_label(&mut truthy_lbl);

                let truthy_result = self.compile_expr(expr1);
                self.store(&self.binop_scratch.clone(), &truthy_result);

                self.function.insn_label(&mut done_lbl);

                self.load(&self.binop_scratch.clone())
            }
        }
    }

    fn float_binop(&mut self, a: &Value, b: &Value, op: BinOp) -> Value {
        let bool = match op {
            BinOp::Greater => self.function.insn_gt(a, b),
            BinOp::GreaterEq => self.function.insn_ge(a, b),
            BinOp::Less => self.function.insn_lt(a, b),
            BinOp::LessEq => self.function.insn_le(a, b),
            BinOp::BangEq => self.function.insn_ne(a, b),
            BinOp::EqEq => self.function.insn_eq(a, b),
            BinOp::MatchedBy => todo!("regex for float??"),
            BinOp::NotMatchedBy => todo!("regex for float??"),
        };
        let one = self.function.create_float64_constant(1.0);
        let zero = self.function.create_float64_constant(0.0);
        let mut true_lbl = Label::new();
        let mut done_lbl = Label::new();
        self.function.insn_branch_if(&bool, &mut true_lbl);
        self.function.insn_store(&self.binop_scratch.float, &zero);
        self.function.insn_branch(&mut done_lbl);
        self.function.insn_label(&mut true_lbl);
        self.function.insn_store(&self.binop_scratch.float, &one);

        self.function.insn_label(&mut done_lbl);
        self.function.insn_load(&self.binop_scratch.float)
    }

    fn compile_exprs_to_string(&mut self, exprs: &Vec<TypedExpr>) -> Vec<Value> {
        exprs
            .iter()
            .map(|v| {
                let val = self.compile_expr(v);
                self.to_string(&val, v.typ)
            })
            .collect::<Vec<Value>>()
    }

    // Call runtime and combine values. All values MUST be strings.
    fn concat_values(&mut self, compiled: &Vec<Value>) -> ValueT {
        let mut result = self.runtime.concat(
            &mut self.function,
            compiled.get(0).unwrap().clone(),
            compiled.get(1).unwrap().clone(),
        );
        if compiled.len() >= 3 {
            for var in &compiled[2..] {
                result = self.runtime.concat(&mut self.function, result, var.clone());
            }
        }
        ValueT::new(self.string_tag(), self.zero_f(), result)
    }

    fn load(&mut self, ptr: &ValuePtrT) -> ValueT {
        let tag = self.function.insn_load(&ptr.tag);
        let val = self.function.insn_load(&ptr.float);
        let ptr = self.function.insn_load(&ptr.pointer);
        ValueT::new(tag, val, ptr)
    }

    fn store(&mut self, ptr: &ValuePtrT, value: &ValueT) {
        self.function.insn_store(&ptr.tag, &value.tag);
        self.function.insn_store(&ptr.float, &value.float);
        self.function.insn_store(&ptr.pointer, &value.pointer);
    }
}
