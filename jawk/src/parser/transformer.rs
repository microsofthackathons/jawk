use std::collections::{HashMap, HashSet};
use crate::parser::{Function, PatternAction, Stmt, TypedExpr};
use crate::{parser, Expr};
use crate::lexer::Token::Print;
use crate::printable_error::PrintableError;

// Turn a program into just a single Stmt
pub fn transform(begins: Vec<Stmt>, ends: Vec<Stmt>, pas: Vec<PatternAction>) -> Stmt {
    let mut prog = begins;
    let mut every_line_stms = vec![];
    for pattern in pas {
        let stmt = if let Some(test) = pattern.pattern {
            Stmt::If(test, Box::new(pattern.action), None)
        } else {
            pattern.action
        };
        every_line_stms.push(stmt)
    }
    if every_line_stms.len() > 0 {
        let line_loop = Stmt::While(
            TypedExpr::new(Expr::Call),
            Box::new(Stmt::Group(every_line_stms)),
        );
        prog.push(line_loop);
    }

    for end in ends {
        prog.push(end);
    }
    Stmt::Group(prog)
}


struct FunctionTransformer {
    args: Vec<String>,
    name: String,
}