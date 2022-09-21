mod call_log;
mod live;
mod testing;

use crate::lexer::BinOp;
use gnu_libjit::{Context, Function, Value};
pub use live::LiveRuntime;
use std::ffi::c_void;
pub use testing::TestRuntime;

pub trait Runtime {
    fn new(files: Vec<String>) -> Self;
    fn call_next_line(&mut self, func: &mut Function) -> Value;
    fn column(&mut self, func: &mut Function, tag: Value, float: Value, ptr: Value) -> Value;
    fn free_string(&mut self, func: &mut Function, ptr: Value) -> Value;
    fn string_to_number(&mut self, func: &mut Function, ptr: Value) -> Value;
    fn copy_string(&mut self, func: &mut Function, ptr: Value) -> Value;
    fn number_to_string(&mut self, func: &mut Function, number: Value) -> Value;
    fn print_string(&mut self, func: &mut Function, ptr: Value);
    fn print_float(&mut self, func: &mut Function, number: Value);
    fn concat(&mut self, func: &mut Function, ptr1: Value, ptr2: Value) -> Value;
    fn empty_string(&mut self, func: &mut Function) -> Value;
    fn binop(&mut self, func: &mut Function, ptr1: Value, ptr2: Value, binop: BinOp) -> Value;
}