use gnu_libjit_sys::{jit_label_t};

pub struct Label {
    pub(crate) inner: jit_label_t,
}
impl Label {
    // Creates a new label that undefined. Will not be placed into into
    // the function until function.insn_label is called. You may use this as a branch
    // target before placing it.
    pub fn new() -> Label {
        let value: ::std::os::raw::c_ulong = 1;
        let mut bytes = value.to_le_bytes();
        for i in 0..4 {
            bytes[i] = 0xFF; // TODO: is jit_undefined_label 2^32-1 on all systems?
        }
        let value = ::std::os::raw::c_ulong::from_le_bytes(bytes);
        Label {
            inner: value
        }
    }
}