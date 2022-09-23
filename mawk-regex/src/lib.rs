extern crate core;

use std::os::raw::c_char;
use mawk_regex_sys::{REtest, REcompile, REmatch, PTR, REdestroy};

pub struct Regex {
    ptr: PTR,
}

impl Regex {
    pub fn new(str: &str) -> Self {
        unsafe {
            Regex { ptr: REcompile(str.as_ptr() as *mut c_char, str.len() as ::std::os::raw::c_ulong) }
        }
    }
    pub fn matches(&self, str: &str) -> bool {
        unsafe {
            REtest(str.as_ptr() as *mut c_char, str.len() as ::std::os::raw::c_ulong, self.ptr) != 0
        }
    }
}

impl Drop for Regex {
    fn drop(&mut self) {
        unsafe {
            REdestroy(self.ptr)
        }
    }
}
