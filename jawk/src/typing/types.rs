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


#[derive(Clone)]
pub struct TypedFunc {
    inner: Rc<RefCell<TypedFuncI>>,
}

impl TypedFunc {
    pub fn new(func: Function) -> Self {
        Self { inner: Rc::new(RefCell::new(TypedFuncI::new(func))) }
    }
    pub fn done(mut self) -> Function {
        let guard = RefCell::borrow(&self.inner);
        guard.func.clone()
    }
}

struct CallI {
    target: TypedFunc,
    args: Option<ArgT>,
}

type Call = Rc<RefCell<CallI>>;

struct TypedFuncI {
    pub func: Function,
    callers: HashSet<TypedFunc>,
    calls: Vec<Call>,
}

impl TypedFuncI {
    pub fn new(func: Function) -> Self {
        Self {
            func,
            callers: HashSet::new(),
            calls: vec![],
        }
    }
}

pub struct TypedProgram {
    pub main: TypedFunc,
    pub functions: Vec<TypedFunc>,
    pub global_analysis: AnalysisResults,
}

impl TypedProgram {
    pub fn new(main: TypedFunc, functions: Vec<TypedFunc>, results: AnalysisResults) -> Self {
        Self { main, functions, global_analysis: results }
    }
    pub fn done(self) -> Program {
        Program {
            global_analysis: self.global_analysis,
            main: self.main.done(),
            functions: self.functions.into_iter().map(|func| func.done()).collect(),
        }
    }
}