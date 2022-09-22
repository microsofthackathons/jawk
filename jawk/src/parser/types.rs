use crate::lexer::{BinOp, LogicalOp, MathOp};
use std::fmt::{Display, Formatter};

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum ScalarType {
    String,
    Float,
    Variable,
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Expr(TypedExpr),
    Print(TypedExpr),
    Group(Vec<Stmt>),
    If(TypedExpr, Box<Stmt>, Option<Box<Stmt>>),
    While(TypedExpr, Box<Stmt>),
}

impl Display for Stmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Expr(expr) => write!(f, "{}", expr)?,
            Stmt::Print(expr) => write!(f, "print {}", expr)?,
            Stmt::Group(group) => {
                for elem in group {
                    write!(f, "{}", elem)?;
                }
            }
            Stmt::If(test, if_so, if_not) => {
                write!(f, "if {} {{{}}}", test, if_so)?;
                if let Some(else_case) = if_not {
                    write!(f, "else {{ {} }}", else_case)?;
                }
            }
            Stmt::While(test, body) => {
                write!(f, "while {} {{{}}} ", test, body)?;
            }
        };
        write!(f, "\n")
    }
}

#[derive(Debug, PartialEq)]
pub struct PatternAction {
    pub pattern: Option<TypedExpr>,
    pub action: Stmt,
}

impl PatternAction {
    pub fn new<ExprT: Into<Option<TypedExpr>>>(pattern: ExprT, action: Stmt) -> Self {
        Self { pattern: pattern.into(), action }
    }
    pub fn new_pattern_only(test: TypedExpr) -> PatternAction {
        PatternAction::new(
            Some(test),
            Stmt::Print(Expr::Column(Box::new(
                Expr::NumberF64(0.0).into()),
            ).into()),
        )
    }
    pub fn new_action_only(body: Stmt) -> PatternAction {
        PatternAction::new(None, body)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedExpr {
    pub typ: ScalarType,
    pub expr: Expr,
}

impl TypedExpr {
    pub fn new(expr: Expr) -> TypedExpr {
        TypedExpr {
            typ: ScalarType::Variable,
            expr,
        }
    }
}

impl Into<TypedExpr> for Expr {
    fn into(self) -> TypedExpr {
        TypedExpr::new(self)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Assign(String, Box<TypedExpr>),
    NumberF64(f64),
    String(String),
    Concatenation(Vec<TypedExpr>),
    BinOp(Box<TypedExpr>, BinOp, Box<TypedExpr>),
    MathOp(Box<TypedExpr>, MathOp, Box<TypedExpr>),
    LogicalOp(Box<TypedExpr>, LogicalOp, Box<TypedExpr>),
    Variable(String),
    Column(Box<TypedExpr>),
    Call,
    Ternary(Box<TypedExpr>, Box<TypedExpr>, Box<TypedExpr>),
    Regex(String),
}

impl Display for TypedExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.typ {
            ScalarType::String => write!(f, "(s {})", self.expr),
            ScalarType::Float => write!(f, "(f {})", self.expr),
            ScalarType::Variable => write!(f, "(v {})", self.expr),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Assign(var, expr) => write!(f, "{} = {}", var, expr),
            Expr::Call => write!(f, "check_if_there_is_another_line"),
            Expr::Variable(n) => write!(f, "{}", n),
            Expr::String(str) => write!(f, "\"{}\"", str),
            Expr::NumberF64(n) => write!(f, "{}", n),
            Expr::BinOp(left, op, right) => write!(f, "{}{}{}", left, op, right),
            Expr::Ternary(cond, expr1, expr2) => write!(f, "{} ? {} : {}", cond, expr1, expr2),
            Expr::MathOp(left, op, right) => write!(f, "{}{}{}", left, op, right),
            Expr::LogicalOp(left, op, right) => write!(f, "{}{}{}", left, op, right),
            Expr::Column(col) => write!(f, "${}", col),
            Expr::Concatenation(vals) => {
                let vals = vals
                    .iter()
                    .map(|v| format!("{}", v))
                    .collect::<Vec<String>>();
                let str = vals.join(" ");
                write!(f, "{}", str)
            }, 
            Expr::Regex(str) => write!(f, "\"{}\"", str)
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub begins: Vec<Stmt>,
    pub ends: Vec<Stmt>,
    pub pattern_actions: Vec<PatternAction>,
}

impl Program {
    pub fn new(begins: Vec<Stmt>, ends: Vec<Stmt>, pattern_actions: Vec<PatternAction>) -> Program {
        Program {
            begins,
            ends,
            pattern_actions,
        }
    }
    #[allow(dead_code)]
    pub fn new_action_only(stmt: Stmt) -> Program {
        Program {
            begins: vec![],
            ends: vec![],
            pattern_actions: vec![PatternAction::new_action_only(stmt)],
        }
    }
}
