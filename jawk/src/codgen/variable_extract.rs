use crate::parser::{Stmt, TypedExpr};
use crate::Expr;
use std::collections::{HashMap, HashSet};

// Returns 0. the list of all variables  1. All string constants
pub struct ExtractResults {
    pub vars: HashSet<String>,
    pub str_consts: HashSet<String>,
    // Give all arrays a numeric identifier that the runtime will use to track them. This is cheaper than using strings at runtime.
    pub arrays: HashMap<String, i32>,
}

struct Extractor {
    results: ExtractResults,
}

pub fn extract(prog: &Stmt) -> ExtractResults {
    let results = ExtractResults { vars: HashSet::default(), str_consts: HashSet::default(), arrays: HashMap::default() };
    let mut extractor = Extractor { results };
    extractor.extract_stmt(prog);
    extractor.results
}

impl Extractor {

    fn add_array(&mut self, name: String) {
        if !self.results.arrays.contains_key(&name) {
            self.results.arrays.insert(name, self.results.arrays.len() as i32);
        }
    }
    fn extract_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Break => {},
            Stmt::Expr(expr) => self.extract_expr(expr),
            Stmt::Print(expr) => self.extract_expr(expr),
            Stmt::Group(group) => {
                for elem in group {
                    self.extract_stmt(elem);
                }
            }
            Stmt::If(test, if_block, else_block) => {
                self.extract_expr(test);
                self.extract_stmt(if_block);
                if let Some(else_block) = else_block {
                    self.extract_stmt(else_block);
                }
            }
            Stmt::While(test, body) => {
                self.extract_expr(test);
                self.extract_stmt(body);
            }
        }
    }

    fn extract_expr(&mut self, expr: &TypedExpr) {
        match &expr.expr {
            Expr::Variable(var) => {
                self.results.vars.insert(var.clone());
            }
            Expr::String(str) => {
                self.results.str_consts.insert(str.to_string());
            }
            Expr::Regex(str) => {
                self.results.str_consts.insert(str.to_string());
            }
            Expr::NumberF64(_n) => {}
            Expr::BinOp(left, _op, right) => {
                self.extract_expr(left);
                self.extract_expr(right);
            }
            Expr::MathOp(left, _op, right) => {
                self.extract_expr(left);
                self.extract_expr(right);
            }
            Expr::LogicalOp(left, _op, right) => {
                self.extract_expr(left);
                self.extract_expr(right);
            }
            Expr::Column(col) => self.extract_expr(col),
            Expr::Call => {}
            Expr::ScalarAssign(var, value) => {
                self.results.vars.insert(var.clone());
                self.extract_expr(value);
            }
            Expr::Concatenation(vals) => {
                for val in vals {
                    self.extract_expr(val);
                }
            }
            Expr::Ternary(cond, expr1, expr2) => {
                self.extract_expr(cond);
                self.extract_expr(expr1);
                self.extract_expr(expr2);
            }
            Expr::ArrayIndex { name, indices } => {
                self.add_array(name.clone());
                for idx in indices {
                    self.extract_expr(idx);
                }
            }
            Expr::InArray { indices, name } => {
                self.add_array(name.clone());
                for idx in indices {
                    self.extract_expr(idx);
                }
            }
            Expr::ArrayAssign { name, indices, value } => {
                self.add_array(name.clone());
                for idx in indices {
                    self.extract_expr(idx);
                }
                self.extract_expr(value);
            }
        }
    }
}