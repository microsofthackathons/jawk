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
use crate::codgen::{CodeGen, ValuePtrT, ValueT, variable_extract};

impl<'a, RuntimeT: Runtime> CodeGen<'a, RuntimeT> {
    // Helpers for commonly used values
    pub fn float_tag(&self) -> Value {
        self.float_tag.clone()
    }
    pub fn string_tag(&self) -> Value {
        self.string_tag.clone()
    }
    pub fn zero_f(&self) -> Value {
        self.zero_f.clone()
    }

    pub fn define_all_vars(&mut self, prog: &Stmt) -> Result<HashSet<String>, PrintableError> {
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

    pub fn float_is_truthy_ret_int(&mut self, value: &Value) -> Value {
        let zero_f = self.function.create_float64_constant(0.0);
        self.function.insn_ne(&value, &zero_f)
    }

    pub fn to_float(&mut self, value: &ValueT, typ: AwkT) -> Value {
        if typ == AwkT::Float {
            return value.float.clone();
        }
        self.function.insn_call(
            &self.subroutines.to_float(&mut self.context, self.runtime),
            value.into(),
        )
    }

    pub fn to_string(&mut self, value: &ValueT, typ: AwkT) -> Value {
        if typ == AwkT::String {
            return value.pointer.clone();
        }
        let mut done_lbl = Label::new();
        self.function.insn_store(&self.binop_scratch.pointer, &value.pointer);
        let is_string = self.function.insn_eq(&value.tag, &self.string_tag);
        self.function.insn_branch_if(&is_string, &mut done_lbl);

        let new_string = self.runtime.number_to_string(&mut self.function, value.float.clone());
        self.function.insn_store(&self.binop_scratch.pointer, &new_string);

        self.function.insn_label(&mut done_lbl);
        let ptr = self.function.insn_load(&self.binop_scratch.pointer);
        ptr
    }

    // Free the value in the value pointer if it's a string
    pub fn drop_if_string_ptr(&mut self, value: &ValuePtrT, typ: AwkT) {
        if let AwkT::Float = typ {
            return;
        }
        let value = self.load(&value);
        self.drop_if_str(&value, typ)
    }

    // Free the value if it's a string
    pub fn drop_if_str(&mut self, value: &ValueT, typ: AwkT) {
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

    pub fn drop(&mut self, value: &Value) {
        self.runtime.free_string(&mut self.function, value.clone());
    }

    // Take a value and return an int 0 or 1
    pub fn truthy_ret_integer(&mut self, value: &ValueT, typ: AwkT) -> Value {
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

    pub fn copy_if_string(&mut self, value: ValueT, typ: AwkT) -> ValueT {
        let zero = self.function.create_float64_constant(0.0);
        let str_tag = self.string_tag();
        match typ {
            AwkT::String => {
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

    pub fn float_binop(&mut self, a: &Value, b: &Value, op: BinOp) -> Value {
        let bool = match op {
            BinOp::Greater => self.function.insn_gt(a, b),
            BinOp::GreaterEq => self.function.insn_ge(a, b),
            BinOp::Less => self.function.insn_lt(a, b),
            BinOp::LessEq => self.function.insn_le(a, b),
            BinOp::BangEq => self.function.insn_ne(a, b),
            BinOp::EqEq => self.function.insn_eq(a, b),
            BinOp::MatchedBy | BinOp::NotMatchedBy => {
                let astr = self.runtime.number_to_string(&mut self.function, a.clone());
                let bstr = self.runtime.number_to_string(&mut self.function, b.clone());
                return self.runtime.binop(&mut self.function, astr, bstr, op);
            },
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

    pub fn compile_exprs_to_string(&mut self, exprs: &Vec<TypedExpr>) -> Vec<Value> {
        exprs
            .iter()
            .map(|v| {
                let val = self.compile_expr(v);
                self.to_string(&val, v.typ)
            })
            .collect::<Vec<Value>>()
    }

    // Call runtime and combine values. All values MUST be strings.
    pub fn concat_values(&mut self, compiled: &Vec<Value>) -> ValueT {
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

    pub fn load(&mut self, ptr: &ValuePtrT) -> ValueT {
        let tag = self.function.insn_load(&ptr.tag);
        let val = self.function.insn_load(&ptr.float);
        let ptr = self.function.insn_load(&ptr.pointer);
        ValueT::new(tag, val, ptr)
    }

    pub fn store(&mut self, ptr: &ValuePtrT, value: &ValueT) {
        self.function.insn_store(&ptr.tag, &value.tag);
        self.function.insn_store(&ptr.float, &value.float);
        self.function.insn_store(&ptr.pointer, &value.pointer);
    }
}