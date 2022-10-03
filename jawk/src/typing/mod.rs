mod inference_pass;
mod function_pass;
mod types;
mod test;

pub use crate::typing::types::{TypedProgram, AnalysisResults, TypedFunc};

use std::collections::{HashMap, HashSet};
use std::process::id;
use crate::parser::{Arg, ArgT, Function, Program, ScalarType, Stmt, TypedExpr};
use crate::Expr;
use immutable_chunkmap::map::Map;
use libc::{glob, proc_bsdinfo};
use crate::printable_error::PrintableError;
use crate::typing::types::{MapT};
use crate::typing::function_pass::FunctionAnalysis;

pub fn analyze(mut stmt: Program) -> Result<Program, PrintableError> {
    let mut func_analysis = FunctionAnalysis::new();
    let typed_program = func_analysis.analyze_program(stmt)?;
    let prog = typed_program.done();
    Ok(prog)
}