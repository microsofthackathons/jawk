use crate::codgen::FLOAT_TAG;
use crate::columns::Columns;
use crate::lexer::BinOp;
use crate::runtime::Runtime;
use gnu_libjit::{Context, Function, Value};
use std::ffi::c_void;
use std::fmt::{Write as FmtWrite};
use std::io::{BufWriter, StdoutLock, Write};
use std::rc::Rc;
use regex::Regex;

// Live runtime used by most programs.
// A pointer to the runtime data is provided for all calls but only used for some.
// Its mainly here for the test runtime.

pub extern "C" fn print_string(data: *mut c_void, value: *mut String) {
    let data = cast_to_runtime_data(data);
    let str = unsafe { Rc::from_raw(value) };
    if str.ends_with("\n") {
        data.stdout.write_all(str.as_bytes()).expect("failed to write to stdout")
    } else {
        data.stdout.write_all(str.as_bytes()).expect("failed to write to stdout");
        data.stdout.write_all("\n".as_bytes()).expect("failed to write to stdout");
    }
    Rc::into_raw(str);
}

pub extern "C" fn print_float(data: *mut c_void, value: f64) {
    let data = cast_to_runtime_data(data);

    data.stdout.write_fmt( format_args!("{}\n", value)).unwrap();
    // data.stdout.write_all( value.to_string().as_str().as_bytes()).unwrap();

    // data.stdout.write_all(value.to_string().as_bytes()).expect("failed to write to stdout");
    // data.stdout.write_all("\n".as_bytes()).expect("failed to write to stdout");
}

extern "C" fn next_line(data: *mut c_void) -> f64 {
    let data = cast_to_runtime_data(data);
    if data.columns.next_line() {
        1.0
    } else {
        0.0
    }
}

extern "C" fn column(
    data_ptr: *mut c_void,
    tag: u8,
    value: f64,
    pointer: *const String,
) -> *mut String {
    let data = cast_to_runtime_data(data_ptr);
    let idx = if tag == FLOAT_TAG {
        value
    } else {
        string_to_number(data_ptr, pointer)
    };
    let idx = idx.round() as usize;
    Rc::into_raw(Rc::new(data.columns.get(idx))) as *mut String
}

extern "C" fn free_string(_data: *mut c_void, string: *mut String) -> f64 {
    unsafe { Rc::from_raw(string) };
    0.0
}

extern "C" fn concat(
    _data_ptr: *mut c_void,
    left: *const String,
    right: *const String,
) -> *const String {
    let lhs = unsafe { Rc::from_raw(left) };
    let rhs = unsafe { Rc::from_raw(right) };
    let mut lhs: String = match Rc::try_unwrap(lhs) {
        Ok(str) => str,
        Err(rc) => (*rc).clone(),
    };
    lhs.push_str(&rhs);
    Rc::into_raw(Rc::new(lhs))
}

extern "C" fn empty_string(_data_ptr: *mut c_void) -> *const String {
    Rc::into_raw(Rc::new("".to_string()))
}

extern "C" fn binop(
    _dat: *mut c_void,
    l_ptr: *const String,
    r_ptr: *const String,
    binop: BinOp,
) -> std::os::raw::c_double {
    let left = unsafe { Rc::from_raw(l_ptr) };
    let right = unsafe { Rc::from_raw(r_ptr) };

    let res = match binop {
        BinOp::Greater => left > right,
        BinOp::GreaterEq => left >= right,
        BinOp::Less => left < right,
        BinOp::LessEq => left <= right,
        BinOp::BangEq => left != right,
        BinOp::EqEq => left == right,
        BinOp::MatchedBy => {
            // use regex::Regex;
            // let re = Regex::new(r"^\d{4}-\d{2}-\d{2}$").unwrap();
            // assert!(re.is_match("2014-01-01"));  
            let RE = Regex::new(&right).unwrap();
            RE.is_match(&left)
        },
        BinOp::NotMatchedBy => todo!("regex"),
    };
    let res = if res { 1.0 } else { 0.0 };
    Rc::into_raw(left);
    Rc::into_raw(right);
    res
}

extern "C" fn string_to_number(_data: *mut c_void, ptr: *const String) -> f64 {
    let string = unsafe { Rc::from_raw(ptr) };
    let res = if string.len() == 0 {
        0.0
    } else {
        // TODO: Support scientific notation
        string
            .parse()
            .expect(&format!("couldn't convert string to number {}", string))
    };
    Rc::into_raw(string);
    res
}

extern "C" fn number_to_string(_data: *mut c_void, value: f64) -> *const String {
    let value = if value.fract() == 0.0 {
        value.floor()
    } else {
        value
    };
    Rc::into_raw(Rc::new(value.to_string()))
}

extern "C" fn copy_string(_data: *mut c_void, ptr: *mut String) -> *const String {
    let original = unsafe { Rc::from_raw(ptr) };
    let copy = original.clone();
    Rc::into_raw(original);
    Rc::into_raw(copy)
}

extern "C" fn malloc(_data: *mut std::os::raw::c_void, num_bytes: usize) -> *mut c_void {
    unsafe { libc::malloc(num_bytes) as *mut c_void }
}

extern "C" fn realloc(
    _data: *mut std::os::raw::c_void,
    ptr: *mut libc::c_void,
    num_bytes: usize,
) -> *mut c_void {
    unsafe { libc::realloc(ptr, num_bytes) as *mut c_void }
}

extern "C" fn free(_data_ptr: *mut libc::c_void, ptr: *mut libc::c_void) {
    unsafe { libc::free(ptr) };
}

extern "C" fn helper(_data_ptr: *mut std::os::raw::c_void, _str: usize, _size: usize) -> f64 {
    return 1.1;
}

pub struct LiveRuntime {
    runtime_data_constant: Option<Value>,
    runtime_data: *mut RuntimeData,
    pub next_line: *mut c_void,
    pub column: *mut c_void,
    pub free_string: *mut c_void,
    pub string_to_number: *mut c_void,
    pub number_to_string: *mut c_void,
    pub print_string: *mut c_void,
    pub print_float: *mut c_void,
    pub copy_string: *mut c_void,
    pub concat: *mut c_void,
    pub binop: *mut c_void,
    pub empty_string: *mut c_void,
}

impl Drop for LiveRuntime {
    fn drop(&mut self) {
        unsafe {
            (*self.runtime_data).stdout.flush().expect("could not flush stdout");
        }
    }
}

// Pointer to this is passed in with every call. The reason we require it for every call instead of making it
// a rust global is so we can easily run tests fully independently of each other.
pub struct RuntimeData {
    columns: Columns,
    buffer: String,
    stdout: BufWriter<StdoutLock<'static>>
}

impl RuntimeData {
    pub fn new(files: Vec<String>) -> RuntimeData {
        RuntimeData {
            buffer: String::with_capacity(1000),
            columns: Columns::new(files),
            stdout: BufWriter::new(std::io::stdout().lock()),
        }
    }
}

impl LiveRuntime {
    fn data_ptr(&mut self, func: &mut Function) -> Value {
        if let Some(val) = &self.runtime_data_constant {
            val.clone()
        } else {
            let val = func.create_void_ptr_constant(self.runtime_data as *mut c_void);
            self.runtime_data_constant.insert(val.clone());
            val
        }
    }
}

impl Runtime for LiveRuntime {
    fn new(files: Vec<String>) -> LiveRuntime {
        let data = Box::new(RuntimeData::new(files));
        let ptr = Box::leak(data);
        LiveRuntime {
            runtime_data_constant: None,
            runtime_data: ptr as *mut RuntimeData,
            next_line: next_line as *mut c_void,
            column: column as *mut c_void,
            free_string: free_string as *mut c_void,
            string_to_number: string_to_number as *mut c_void,
            copy_string: copy_string as *mut c_void,
            number_to_string: number_to_string as *mut c_void,
            print_string: print_string as *mut c_void,
            concat: concat as *mut c_void,
            print_float: print_float as *mut c_void,
            empty_string: empty_string as *mut c_void,
            binop: binop as *mut c_void,
        }
    }

    fn call_next_line(&mut self, func: &mut Function) -> Value {
        let data_ptr = self.data_ptr(func);
        func.insn_call_native(
            self.next_line,
            vec![data_ptr],
            Some(Context::float64_type()),
        )
    }

    fn column(&mut self, func: &mut Function, tag: Value, float: Value, pointer: Value) -> Value {
        let data_ptr = self.data_ptr(func);
        func.insn_call_native(
            self.column,
            vec![data_ptr, tag, float, pointer],
            Some(Context::void_ptr_type()),
        )
    }

    fn free_string(&mut self, func: &mut Function, ptr: Value) -> Value {
        let data_ptr = self.data_ptr(func);
        func.insn_call_native(self.free_string, vec![data_ptr, ptr], None)
    }

    fn string_to_number(&mut self, func: &mut Function, ptr: Value) -> Value {
        let data_ptr = self.data_ptr(func);
        func.insn_call_native(
            self.string_to_number,
            vec![data_ptr, ptr],
            Some(Context::float64_type()),
        )
    }

    fn copy_string(&mut self, func: &mut Function, ptr: Value) -> Value {
        let data_ptr = self.data_ptr(func);
        func.insn_call_native(
            self.copy_string,
            vec![data_ptr, ptr],
            Some(Context::void_ptr_type()),
        )
    }

    fn number_to_string(&mut self, func: &mut Function, number: Value) -> Value {
        let data_ptr = self.data_ptr(func);
        func.insn_call_native(
            self.number_to_string,
            vec![data_ptr, number],
            Some(Context::void_ptr_type()),
        )
    }

    fn print_string(&mut self, func: &mut Function, ptr: Value) {
        let data_ptr = self.data_ptr(func);
        func.insn_call_native(self.print_string, vec![data_ptr, ptr], None);
    }

    fn print_float(&mut self, func: &mut Function, number: Value) {
        let data_ptr = self.data_ptr(func);
        func.insn_call_native(self.print_float, vec![data_ptr, number], None);
    }

    fn concat(&mut self, func: &mut Function, ptr1: Value, ptr2: Value) -> Value {
        let data_ptr = self.data_ptr(func);
        func.insn_call_native(
            self.concat,
            vec![data_ptr, ptr1, ptr2],
            Some(Context::void_ptr_type()),
        )
    }

    fn empty_string(&mut self, func: &mut Function) -> Value {
        let data_ptr = self.data_ptr(func);
        func.insn_call_native(
            self.empty_string,
            vec![data_ptr],
            Some(Context::void_ptr_type()),
        )
    }

    fn binop(&mut self, func: &mut Function, ptr1: Value, ptr2: Value, binop: BinOp) -> Value {
        let binop = func.create_sbyte_constant(binop as i8);
        let data_ptr = self.data_ptr(func);
        func.insn_call_native(
            self.binop,
            vec![data_ptr, ptr1, ptr2, binop],
            Some(Context::float64_type()),
        )
    }
}

pub fn cast_to_runtime_data(data: *mut c_void) -> &'static mut RuntimeData {
    unsafe {
        let data = data as *mut RuntimeData;
        let d = &mut *data;
        d
    }
}
