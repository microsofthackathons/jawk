use gnu_libjit_sys::{jit_type_create_pointer, jit_type_t};

#[derive(Clone, Copy, Debug)]
pub struct JitType {
    pub(crate) inner: jit_type_t,
}
impl JitType {
    pub(crate) fn new(inner: jit_type_t) -> JitType {
        JitType { inner }
    }

    // For input type T returns a *T type.
    pub fn type_create_pointer(&self) -> JitType {
        let ptr_type = unsafe { jit_type_create_pointer(self.inner, 1) };
        JitType::new(ptr_type)
    }
}
