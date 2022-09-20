use crate::parser::{Stmt, TypedExpr};
use crate::Expr;
use std::collections::HashSet;

// Returns 0. the list of all variables  1. All string constants
pub fn extract(prog: &Stmt) -> (HashSet<String>, HashSet<String>) {
    let mut vars = HashSet::new();
    let mut string_constants = HashSet::new();
    extract_stmt(prog, &mut vars, &mut string_constants);
    (vars, string_constants)
}

fn extract_stmt(stmt: &Stmt, vars: &mut HashSet<String>, consts: &mut HashSet<String>) {
    match stmt {
        Stmt::Expr(expr) => extract_expr(expr, vars, consts),
        Stmt::Print(expr) => extract_expr(expr, vars, consts),
        Stmt::Group(group) => {
            for elem in group {
                extract_stmt(elem, vars, consts);
            }
        }
        Stmt::If(test, if_block, else_block) => {
            extract_expr(test, vars, consts);
            extract_stmt(if_block, vars, consts);
            if let Some(else_block) = else_block {
                extract_stmt(else_block, vars, consts);
            }
        }
        Stmt::While(test, body) => {
            extract_expr(test, vars, consts);
            extract_stmt(body, vars, consts);
        }
    }
}

fn extract_expr(expr: &TypedExpr, vars: &mut HashSet<String>, consts: &mut HashSet<String>) {
    match &expr.expr {
        Expr::Variable(var) => {
            vars.insert(var.clone());
        }
        Expr::String(str) => {
            consts.insert(str.to_string());
        }
        Expr::NumberF64(_n) => {}
        Expr::BinOp(left, _op, right) => {
            extract_expr(left, vars, consts);
            extract_expr(right, vars, consts);
        }
        Expr::MathOp(left, _op, right) => {
            extract_expr(left, vars, consts);
            extract_expr(right, vars, consts);
        }
        Expr::LogicalOp(left, _op, right) => {
            extract_expr(left, vars, consts);
            extract_expr(right, vars, consts);
        }
        Expr::Column(col) => extract_expr(col, vars, consts),
        Expr::Call => {}
        Expr::Assign(var, value) => {
            vars.insert(var.clone());
            extract_expr(value, vars, consts);
        }
        Expr::Concatenation(vals) => {
            for val in vals {
                extract_expr(val, vars, consts);
            }
        }
        Expr::Ternary(cond, expr1, expr2) => {
	    extract_expr(cond, vars, consts);
	    extract_expr(expr1, vars, consts);
	    extract_expr(expr2, vars, consts);
        }
    }
}
