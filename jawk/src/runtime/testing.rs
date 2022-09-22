use crate::codgen::FLOAT_TAG;
use crate::columns::Columns;
use crate::lexer::BinOp;
use crate::runtime::call_log::{Call, CallLog};
use crate::runtime::{ErrorCode, Runtime};
use gnu_libjit::{Context, Function, Value};
use std::ffi::c_void;
use std::rc::Rc;
use regex::{Error, Regex};

pub const CANARY: &str = "this is the canary!";

pub extern "C" fn print_string(data: *mut c_void, value: *mut String) {
    let data = cast_to_runtime_data(data);
    data.calls.log(Call::PrintString);
    let str = unsafe { Rc::from_raw(value) };

    let res = if str.ends_with("\n") {
        format!("{}", str)
    } else {
        format!("{}\n", str)
    };
    data.output.push_str(&res);
    println!("{}", str);
    Rc::into_raw(str);
}

pub extern "C" fn print_float(data: *mut c_void, value: f64) {
    let data = cast_to_runtime_data(data);
    data.calls.log(Call::PrintFloat);
    let res = format!("{}\n", value);
    data.output.push_str(&res);
    println!("{}", value);
}

extern "C" fn next_line(data: *mut c_void) -> f64 {
    let data = cast_to_runtime_data(data);
    data.calls.log(Call::NextLine);
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
) -> *const String {
    let data = cast_to_runtime_data(data_ptr);
    let idx_f = if tag == FLOAT_TAG {
        value
    } else {
        string_to_number(data_ptr, pointer)
    };
    let idx = idx_f.round() as usize;
    let str = data.columns.get(idx);
    data.calls.log(Call::Column(idx_f, str.clone()));
    println!(
        "\tgetting column tag:{} float:{} ptr:{:?}",
        tag, value, pointer
    );
    data.string_out("column");
    Rc::into_raw(Rc::new(str))
}

extern "C" fn free_string(data_ptr: *mut c_void, ptr: *mut String) -> f64 {
    let data = cast_to_runtime_data(data_ptr);
    data.calls.log(Call::FreeString);
    data.string_in(&format!("free_string {:?}", ptr));

    let data = unsafe { Rc::from_raw(ptr) };
    println!(
        "\tstring is: '{}' count is now: {}",
        data,
        Rc::strong_count(&data) - 1
    );
    0.0
}

extern "C" fn concat(
    data: *mut c_void,
    left: *const String,
    right: *const String,
) -> *const String {
    let data = cast_to_runtime_data(data);
    data.calls.log(Call::Concat);
    println!("\t{:?}, {:?}", left, right);
    let lhs = unsafe { Rc::from_raw(left) };
    let rhs = unsafe { Rc::from_raw(right) };

    let mut lhs: String = match Rc::try_unwrap(lhs) {
        Ok(str) => {
            println!("\tDowngraded RC into box for string {}", str);
            str
        }
        Err(rc) => (*rc).clone(),
    };
    data.string_in(&format!("concat lhs {}", lhs));
    data.string_in(&format!("concat rhs {}", rhs));

    lhs.push_str(&rhs);
    println!("\tResult: '{}'", lhs);
    data.string_out("concat result");
    Rc::into_raw(Rc::new(lhs))
}

extern "C" fn empty_string(data: *mut c_void) -> *const String {
    let data = cast_to_runtime_data(data);
    data.calls.log(Call::EmptyString);
    let rc = Rc::new("".to_string());
    let ptr = Rc::into_raw(rc);
    data.string_out(&format!("empty string {:?}", ptr));
    ptr
}

extern "C" fn string_to_number(data_ptr: *mut c_void, ptr: *const String) -> f64 {
    let data = cast_to_runtime_data(data_ptr);
    data.calls.log(Call::StringToNumber);

    let string = unsafe { Rc::from_raw(ptr) };
    println!("\tstring_to_number {:?} '{}'", ptr, string);
    let res = if string.len() == 0 {
        0.0
    } else {
        string
            .parse()
            .expect(&format!("couldn't convert string to number {}", string))
    };
    Rc::into_raw(string);
    println!("\tret {}", res);
    res
}

extern "C" fn number_to_string(data_ptr: *mut c_void, value: f64) -> *const String {
    let data = cast_to_runtime_data(data_ptr);
    data.calls.log(Call::NumberToString);
    data.string_out("number_to_string");
    println!("\tnum: {}", value);
    let value = if value.fract() == 0.0 {
        value.floor()
    } else {
        value
    };

    let heap_alloc_string = Rc::new(value.to_string());

    let str = (*heap_alloc_string).clone();
    let ptr = Rc::into_raw(heap_alloc_string);
    println!("String is {:?} {}", ptr, str);
    ptr
}

extern "C" fn copy_string(data_ptr: *mut c_void, ptr: *mut String) -> *const String {
    let data = cast_to_runtime_data(data_ptr);
    data.calls.log(Call::CopyString);
    data.string_out("copy_string");

    let original = unsafe { Rc::from_raw(ptr as *mut String) };
    println!(
        "\tCopying string {:?} '{}' count is {}",
        ptr,
        original,
        Rc::strong_count(&original)
    );
    let copy = original.clone();
    Rc::into_raw(original);

    println!("\tNew count is {}", Rc::strong_count(&copy));
    let copy = Rc::into_raw(copy);
    println!("\tCopy is: {:?}", copy);
    copy
}

extern "C" fn binop(
    data: *mut c_void,
    l_ptr: *const String,
    r_ptr: *const String,
    binop: BinOp,
) -> std::os::raw::c_double {
    let data = cast_to_runtime_data(data);
    data.calls.log(Call::BinOp);
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
            let RE = Regex::new(&right).unwrap();
            RE.is_match(&left)
        },
        BinOp::NotMatchedBy => {
            let RE = Regex::new(&right).unwrap();
            !RE.is_match(&left)
        },
    };
    let res = if res { 1.0 } else { 0.0 };
    println!(
        "\tBinop called: '{}' {:?} '{}' == {}",
        left, binop, right, res
    );
    Rc::into_raw(left);
    Rc::into_raw(right);
    res
}

extern "C" fn print_error(data: *mut std::os::raw::c_void, code: ErrorCode) {
    eprintln!("error {:?}", code)
}

extern "C" fn malloc(data: *mut std::os::raw::c_void, num_bytes: usize) -> *mut c_void {
    let data = cast_to_runtime_data(data);
    data.string_out("malloc");
    data.calls.log(Call::Malloc);
    unsafe { libc::malloc(num_bytes) as *mut c_void }
}

extern "C" fn realloc(
    data: *mut std::os::raw::c_void,
    ptr: *mut libc::c_void,
    num_bytes: usize,
) -> *mut c_void {
    let data = cast_to_runtime_data(data);
    data.calls.log(Call::Realloc);
    unsafe { libc::realloc(ptr, num_bytes) as *mut c_void }
}

extern "C" fn free(data: *mut std::os::raw::c_void, ptr: *mut libc::c_void) {
    let data = cast_to_runtime_data(data);
    data.calls.log(Call::Free);
    data.string_in("free");
    unsafe { libc::free(ptr) };
}

extern "C" fn helper(_data: std::os::raw::c_int) -> f64 {
    return 1.1;
}

pub struct TestRuntime {
    runtime_data_constant: Option<Value>,
    runtime_data: *mut c_void,
    next_line: *mut c_void,
    column: *mut c_void,
    free_string: *mut c_void,
    string_to_number: *mut c_void,
    number_to_string: *mut c_void,
    print_string: *mut c_void,
    print_float: *mut c_void,
    concat: *mut c_void,
    copy_string: *mut c_void,
    binop: *mut c_void,
    empty_string: *mut c_void,
    malloc: *mut c_void,
    realloc: *mut c_void,
    free: *mut c_void,
    helper: *mut c_void,
    print_error: *mut c_void,
}

pub struct RuntimeData {
    columns: Columns,
    canary: String,
    output: String,
    calls: CallLog,
    string_out: usize,
    strings_in: usize,
}

impl RuntimeData {
    pub fn string_out(&mut self, src: &str) {
        println!("\t===> {} (string out)", src);
        self.string_out += 1;
    }
    pub fn string_in(&mut self, src: &str) {
        println!("\t<=== {} (string in)", src);
        self.strings_in += 1;
    }
    pub fn new(files: Vec<String>) -> RuntimeData {
        RuntimeData {
            canary: String::from(CANARY),
            columns: Columns::new(files),
            output: String::new(),
            calls: CallLog::new(),
            string_out: 0,
            strings_in: 0,
        }
    }
}

impl TestRuntime {
    #[allow(dead_code)]
    pub fn output(&self) -> String {
        cast_to_runtime_data(self.runtime_data).output.clone()
    }
    #[allow(dead_code)]
    pub fn strings_in(&self) -> usize {
        cast_to_runtime_data(self.runtime_data).strings_in
    }
    #[allow(dead_code)]
    pub fn strings_out(&self) -> usize {
        cast_to_runtime_data(self.runtime_data).string_out
    }

    #[allow(dead_code)]
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

impl Runtime for TestRuntime {
    fn new(files: Vec<String>) -> TestRuntime {
        let data = Box::new(RuntimeData::new(files));
        let runtime_data = (Box::leak(data) as *mut RuntimeData) as *mut c_void;
        let rt = TestRuntime {
            runtime_data,
            runtime_data_constant: None,
            next_line: next_line as *mut c_void,
            column: column as *mut c_void,
            free_string: free_string as *mut c_void,
            string_to_number: string_to_number as *mut c_void,
            copy_string: copy_string as *mut c_void,
            number_to_string: number_to_string as *mut c_void,
            print_string: print_string as *mut c_void,
            print_float: print_float as *mut c_void,
            concat: concat as *mut c_void,
            empty_string: empty_string as *mut c_void,
            binop: binop as *mut c_void,
            malloc: malloc as *mut c_void,
            realloc: realloc as *mut c_void,
            free: free as *mut c_void,
            helper: helper as *mut c_void,
            print_error: print_error as *mut c_void,
        };
        println!("binop {:?}", rt.binop);
        rt
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

    fn print_error(&mut self, func: &mut Function, error: ErrorCode)  {
        let binop = func.create_sbyte_constant(error as i8);
        let data_ptr = self.data_ptr(func);
        func.insn_call_native(
            self.print_error,
            vec![data_ptr, binop],
            None,
        );
    }
}

pub fn cast_to_runtime_data(data: *mut c_void) -> &'static mut RuntimeData {
    unsafe {
        let data = data as *mut RuntimeData;
        let d = &mut *data;
        if d.canary != CANARY {
            eprintln!("RUNTIME DATA LOADED WRONG. CANARY MISSING");
            std::process::exit(-1);
        }
        d
    }
}
