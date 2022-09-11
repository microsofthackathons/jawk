use gnu_libjit_sys::jit_type_t;

#[derive(Clone, Copy, Debug)]
pub struct JitType {
    pub(crate) inner: jit_type_t,
}
impl JitType {
    pub fn new(inner: jit_type_t) -> JitType {
        JitType { inner }
    }
}