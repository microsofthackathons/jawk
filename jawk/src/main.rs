extern crate core;

use crate::args::AwkArgs;
use crate::lexer::lex;
use crate::parser::{Expr, parse};
use crate::printable_error::PrintableError;
use crate::typing::{AnalysisResults, analyze};

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
    let source = match args.program.load() {
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
    let mut ast = analyze(parse(lex(&source).unwrap()));

    // 4
    let program = match ast {
        Ok(results) => results,
        Err(err) => {
            eprintln!("{}", err);
            return;
        }
    };

    if args.debug {
        println!("{:?}", program);
        println!("{}", program);
    }

    // 5
    if args.debug {
        if let Err(err) = codgen::compile_and_capture(program, &args.files) {
            eprintln!("{}", err);
        }
    } else {
        if let Err(err) = codgen::compile_and_run(program, &args.files) {
            eprintln!("{}", err);
        }
    }
}
