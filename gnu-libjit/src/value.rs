use gnu_libjit_sys::{jit_value_get_type, jit_value_t};
use crate::JitType;

#[derive(Clone)]
pub struct Value {
    pub(crate) value: jit_value_t,
}

impl Value {
    pub(crate) fn new(value: jit_value_t) -> Value {
        Value { value }
    }
    pub fn value_type(&self) -> JitType {
        JitType::new(unsafe {
            jit_value_get_type(self.value)
        })
    }
}