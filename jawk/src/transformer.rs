use crate::parser::{Stmt, TypedExpr};
use crate::{parser, Expr};

// Turn a program into just a single Stmt
// Also convert any concatenations
//  like a = a b into  a c= b (an append Expr aka concat equals)
pub fn transform(program: parser::Program) -> Stmt {
    let mut prog = program.begins;

    let mut every_line_stms = vec![];
    for pattern in program.pattern_actions {
        let stmt = if let Some(test) = pattern.pattern {
            Stmt::If(test, Box::new(pattern.action), None)
        } else {
            pattern.action
        };
        every_line_stms.push(stmt)
    }
    if every_line_stms.len() > 0 {
        let line_loop = Stmt::While(
            TypedExpr::new_num(Expr::Call),
            Box::new(Stmt::Group(every_line_stms)),
        );
        prog.push(line_loop);
    }

    for end in program.ends {
        prog.push(end);
    }
    Stmt::Group(prog)
}
