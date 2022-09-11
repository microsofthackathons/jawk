use crate::args::AwkArgs;
use crate::lexer::lex;
use crate::parser::{parse, Expr};
use crate::transformer::transform;
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
mod transformer;
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
    // 3. Transform the program with its patterns and actions into a singular Stmt
    // 4. Type checking pass
    // 5. Run it

    // 1,2,3
    let mut ast = transform(parse(lex(&program).unwrap()));

    // 4
    analyze(&mut ast);

    if args.debug {
        println!("{:?}", ast);
        println!("{}", ast);
    }

    // 5
    if args.debug {
        if let Err(err) = codgen::compile_and_capture(ast, &args.files) {
            eprintln!("{}", err);
        }
    } else {
        if let Err(err) = codgen::compile_and_run(ast, &args.files) {
            eprintln!("{}", err);
        }
    }
}
