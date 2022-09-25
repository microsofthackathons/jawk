use gnu_libjit_sys::{jit_value_get_type, jit_value_set_addressable, jit_value_t};
use crate::JitType;

#[derive(Clone)]
pub struct Value {
    pub(crate) value: jit_value_t,
    pub(crate) addressable: bool,
}

impl Value {
    pub(crate) fn new(value: jit_value_t) -> Value {
        Value { value, addressable: false }
    }
    pub fn value_type(&self) -> JitType {
        JitType::new(unsafe {
            jit_value_get_type(self.value)
        })
    }
    pub fn set_addressable(&mut self) {
        unsafe {
            jit_value_set_addressable(self.value)
        }
        self.addressable = true;
    }
}