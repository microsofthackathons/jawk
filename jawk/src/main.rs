use crate::args::AwkArgs;
use crate::lexer::lex;
use crate::parser::{Expr, parse};
use crate::typing::analyze;

mod args;
mod codgen;
mod columns;
mod lexer;
mod parser;
mod printable_error;
mod runtime;
#[allow(dead_code)]
mod test;
mod typing;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let args = match AwkArgs::new(args) {
        Ok(args) => args,
        Err(_) => return,
    };
    let program = match args.program.load() {
        Ok(program) => program,
        Err(e) => {
            eprintln!("{}", e);
            return;
        }
    };
    // 1. Lex into token
    // 2. Parse into tree
    // 3. Type checking pass
    // 4. Run it

    // 1,2,3
    let mut ast = parse(lex(&program).unwrap());

    // 4
    if let Err(err) = analyze(&mut ast) {
        eprintln!("{}", err);
    }

    if args.debug {
        println!("{:?}", ast.main.body);
        println!("{}", ast.main.body);
    }

    // 5
    if args.debug {
        if let Err(err) = codgen::compile_and_capture(ast.main.body, &args.files) {
            eprintln!("{}", err);
        }
    } else {
        if let Err(err) = codgen::compile_and_run(ast.main.body, &args.files) {
            eprintln!("{}", err);
        }
    }
}
