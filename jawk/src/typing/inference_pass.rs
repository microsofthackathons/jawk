use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use crate::{PrintableError};
use crate::parser::{Arg, ArgT, Function, Program, ScalarType, Stmt, TypedExpr, Expr};
use crate::typing::types::{AnalysisResults, MapT};

// pub fn variable_inference(prog: &Program, results: &AnalysisResults) -> TypedPro {
//     let mut typed_funcs = HashMap::new();
//     for func in prog.functions {
//         typed_funcs.insert(func.name.clone(), func)
//     }
//
// }

