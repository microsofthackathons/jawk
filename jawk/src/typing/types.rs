use std::borrow::{Borrow, BorrowMut};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::ops::Deref;
use std::rc::Rc;
use immutable_chunkmap::map::Map;
use crate::parser::{ArgT, Function, Program, ScalarType};

#[derive(Clone, Debug)]
enum VarType {
    Float,
    String,
    Array,
    Variable,
}

impl Into<VarType> for ScalarType {
    fn into(self) -> VarType {
        match self {
            ScalarType::String => VarType::String,
            ScalarType::Float => VarType::Float,
            ScalarType::Variable => VarType::Variable,
        }
    }
}

pub type MapT = Map<String, ScalarType, 1000>;

#[derive(Clone, Debug, PartialEq)]
pub struct AnalysisResults {
    pub global_scalars: HashSet<String>,
    pub global_arrays: HashMap<String, i32>,
    pub str_consts: HashSet<String>,
}

impl AnalysisResults {
    pub fn new() -> Self {
        Self {
            global_scalars: Default::default(),
            global_arrays: Default::default(),
            str_consts: Default::default(),
        }
    }
}

pub struct Call {
    target: String,
    args: Vec<CallArg>,
}

pub struct CallArg {
    typ: Option<ArgT>,
    is_arg: Option<String>,
}

impl CallArg {
    pub fn new<T: Into<String>>(typ: Option<ArgT>, arg: T) -> Self {
        CallArg { typ, is_arg: Some(arg.into()) }
    }
    pub fn new_expr(typ: Option<ArgT>) -> Self {
        CallArg { typ, is_arg: None }
    }
}

impl Call {
    pub fn new<T: Into<String>>(target: T, args: Vec<CallArg>) -> Self {
        Self { target: target.into(), args }
    }
}

pub struct TypedFunc {
    pub func: Function,
    pub callers: HashSet<TypedFunc>,
    pub calls: Vec<Call>,
}

impl TypedFunc {
    pub fn new(func: Function, calls: Vec<Call>) -> Self {
        let len = func.args.len();
        Self {
            func,
            callers: HashSet::new(),
            calls,
        }
    }
    pub fn done(self) -> Function {
        self.func
    }
}

pub struct TypedProgram {
    pub functions: HashMap<String, TypedFunc>,
    pub global_analysis: AnalysisResults,
}

impl TypedProgram {
    pub fn new(functions: HashMap<String, TypedFunc>, results: AnalysisResults) -> Self {
        Self { functions, global_analysis: results }
    }
    pub fn done(self) -> Program {
        Program {
            global_analysis: self.global_analysis,
            functions: self.functions.into_iter()
                .map(|(name, func)| (name, func.func))
                .collect(),
        }
    }
}