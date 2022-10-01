use std::collections::HashSet;
use crate::codgen::variable_extract;
use crate::parser::{Arg, ArgT, Function, Program, ScalarType, Stmt, TypedExpr};
use crate::Expr;
use immutable_chunkmap::map::Map;
use crate::printable_error::PrintableError;

#[derive(Clone, Debug)]
enum VarType {
    Float,
    String,
    Array,
    Variable,
}

impl Into<VarType> for ScalarType {
    fn into(self) -> VarType {
        match self {
            ScalarType::String => VarType::String,
            ScalarType::Float => VarType::Float,
            ScalarType::Variable => VarType::Variable,
        }
    }
}

pub type MapT = Map<String, ScalarType, 1000>;

pub fn analyze(stmt: &mut Program) -> Result<(), PrintableError> {
    TypeAnalysis {
        func_names: HashSet::new(),
        global_scalars: MapT::new(),
        global_arrays: HashSet::new(),
    }.analyze_program(stmt)
}

struct TypeAnalysis {
    global_scalars: MapT,
    global_arrays: HashSet<String>,
    func_names: HashSet<String>,
}

fn get_arg<'a>(func_state: &'a mut FuncState, name: &str) -> Option<&'a mut Arg> {
    if let Some(arg) = func_state.args.iter_mut().find(|a| a.name == name) {
        Some(arg)
    } else {
        None
    }
}

struct FuncState<'a> {
    args: &'a mut [Arg],
    return_type: &'a mut ScalarType,
}

impl TypeAnalysis {
    fn analyze_program(&mut self, prog: &mut Program) -> Result<(), PrintableError> {
        let mut state = FuncState { args: &mut prog.main.args, return_type: &mut prog.main.return_type };
        for func in &prog.functions {
            self.func_names.insert(func.name.clone());
        }
        self.analyze_stmt(&mut prog.main.body, &mut state)?;
        for func in &mut prog.functions {
            let mut fstate = FuncState { args: &mut func.args, return_type: &mut func.return_type };
            self.analyze_stmt(&mut func.body, &mut fstate)?;
        }
        Ok(())
    }
    fn use_as_scalar(&mut self, var: &str, typ: ScalarType, func_state: &mut FuncState) -> Result<(), PrintableError> {
        if let Some(arg) = get_arg(func_state, var) {
            match arg.typ {
                ArgT::Unused => arg.typ = { ArgT::Scalar },
                ArgT::Scalar => {} // scalar arg used as scalar, lgtm
                ArgT::Array => return Err(PrintableError::new(format!("fatal: attempt to use array `{}` in a scalar context", var)))
            }
            return Ok(());
        }
        if self.func_names.contains(var) {
            return Err(PrintableError::new(format!("fatal: attempt to use function `{}` in a scalar context", var)));
        }
        if self.global_arrays.contains(var) {
            return Err(PrintableError::new(format!("fatal: attempt to use array `{}` in a scalar context", var)));
        }
        self.global_scalars = self.global_scalars.insert(var.to_string(), typ).0;
        Ok(())
    }
    fn use_as_array(&mut self, var: &str, func_state: &mut FuncState) -> Result<(), PrintableError> {
        if let Some(arg) = get_arg(func_state, var) {
            match arg.typ {
                ArgT::Unused => arg.typ = { ArgT::Array },
                ArgT::Scalar => return Err(PrintableError::new(format!("fatal: attempt to use scalar `{}` in a array context", var))),
                ArgT::Array => {}
            }
            return Ok(());
        }
        if self.func_names.contains(var) {
            return Err(PrintableError::new(format!("fatal: attempt to use function `{}` in a scalar context", var)));
        }
        if let Some(_type) = self.global_scalars.get(var) {
            return Err(PrintableError::new(format!("fatal: attempt to scalar `{}` in an array context", var)));
        }
        self.global_arrays.insert(var.to_string());
        Ok(())
    }

    fn analyze_stmt(&mut self, stmt: &mut Stmt, func_state: &mut FuncState) -> Result<(), PrintableError> {
        match stmt {
            Stmt::Return(ret) => {
                if let Some(ret_value) = ret {
                    self.analyze_expr(ret_value, func_state, true)?;
                    let mut new_type = TypeAnalysis::merge_types(func_state.return_type, &ret_value.typ);
                    std::mem::swap(&mut new_type, func_state.return_type);
                } else {
                    std::mem::swap(&mut ScalarType::Variable, func_state.return_type);
                }
            }
            Stmt::Printf { args: printf_args, fstring } => {
                for arg in printf_args {
                    self.analyze_expr(arg, func_state, false)?;
                }
                self.analyze_expr(fstring, func_state, false)?;
            }
            Stmt::Break => {}
            Stmt::Expr(expr) => self.analyze_expr(expr, func_state, false)?,
            Stmt::Print(expr) => self.analyze_expr(expr, func_state, false)?,
            Stmt::Group(grouping) => {
                for stmt in grouping {
                    self.analyze_stmt(stmt, func_state)?;
                }
            }
            Stmt::If(test, if_so, if_not) => {
                self.analyze_expr(test, func_state, false)?;
                let mut if_so_map = self.global_scalars.clone();
                let mut if_not_map = self.global_scalars.clone();
                std::mem::swap(&mut if_so_map, &mut self.global_scalars);

                self.analyze_stmt(if_so, func_state)?;
                std::mem::swap(&mut if_so_map, &mut self.global_scalars);
                std::mem::swap(&mut if_not_map, &mut self.global_scalars);
                if let Some(else_case) = if_not {
                    self.analyze_stmt(else_case, func_state)?
                }
                std::mem::swap(&mut if_not_map, &mut self.global_scalars);
                self.global_scalars = TypeAnalysis::merge_maps(&[&if_so_map, &if_not_map]);
            }
            Stmt::While(test, body) => {
                self.analyze_expr(test, func_state, false)?;

                let after_test_map = self.global_scalars.clone();

                self.analyze_stmt(body, func_state)?;

                let after_body_map = self.global_scalars.clone();

                self.global_scalars = TypeAnalysis::merge_maps(&[&after_test_map, &after_body_map]);

                self.analyze_expr(test, func_state, false)?;

                let after_test_map = self.global_scalars.clone();
                self.analyze_stmt(body, func_state)?;
                let after_body_map = self.global_scalars.clone();
                self.global_scalars = TypeAnalysis::merge_maps(&[&after_test_map, &after_body_map]);
            }
        }
        Ok(())
    }
    fn analyze_expr(&mut self, expr: &mut TypedExpr, func_state: &mut FuncState, is_returned: bool) -> Result<(), PrintableError> {
        match &mut expr.expr {
            Expr::Call { args, target: _target } => {
                for arg in args {
                    self.analyze_expr(arg, func_state, false)?;
                    expr.typ = ScalarType::Variable
                }
            }
            Expr::NumberF64(_) => {
                expr.typ = ScalarType::Float;
            }
            Expr::String(_) => {
                expr.typ = ScalarType::String;
            }
            Expr::BinOp(left, _op, right) => {
                self.analyze_expr(left, func_state, false)?;
                self.analyze_expr(right, func_state, false)?;
                expr.typ = ScalarType::Float;
            }
            Expr::MathOp(left, _op, right) => {
                self.analyze_expr(left, func_state, false)?;
                self.analyze_expr(right, func_state, false)?;
                expr.typ = ScalarType::Float;
            }
            Expr::LogicalOp(left, _op, right) => {
                self.analyze_expr(left, func_state, false)?;
                self.analyze_expr(right, func_state, false)?;
                expr.typ = ScalarType::Float;
            }
            Expr::ScalarAssign(var, value) => {
                self.analyze_expr(value, func_state, false)?;
                self.use_as_scalar(var, value.typ, func_state)?;
                expr.typ = value.typ;
            }
            Expr::Regex(_) => {
                expr.typ = ScalarType::String;
            }
            Expr::Ternary(cond, expr1, expr2) => {
                self.analyze_expr(cond, func_state, false)?;
                let mut if_so_map = self.global_scalars.clone();
                let mut if_not_map = self.global_scalars.clone();
                std::mem::swap(&mut if_so_map, &mut self.global_scalars);

                self.analyze_expr(expr1, func_state, false)?;
                std::mem::swap(&mut if_so_map, &mut self.global_scalars);
                std::mem::swap(&mut if_not_map, &mut self.global_scalars);
                self.analyze_expr(expr2, func_state, false)?;
                std::mem::swap(&mut if_not_map, &mut self.global_scalars);
                self.global_scalars = TypeAnalysis::merge_maps(&[&if_so_map, &if_not_map]);
                expr.typ = Self::merge_types(&expr1.typ, &expr2.typ);
            }
            Expr::Variable(var) => {
                if let Some(arg) = func_state.args.iter().find(|arg| *arg.name == *var) {
                    if arg.typ == ArgT::Array && is_returned {
                        return Err(PrintableError::new(format!("fatal: attempted to use array {} in scalar context", var)));
                    }
                    expr.typ = ScalarType::Variable;
                } else if self.global_arrays.contains(var) && is_returned {
                    return Err(PrintableError::new(format!("fatal: attempted to use array {} in scalar context", var)));
                } else if let Some(typ) = self.global_scalars.get(var) {
                    expr.typ = *typ;
                } else {
                    expr.typ = ScalarType::String;
                    self.use_as_scalar(var, ScalarType::Variable, func_state)?;
                }
            }
            Expr::Column(col) => {
                expr.typ = ScalarType::String;
                self.analyze_expr(col, func_state, false)?;
            }
            Expr::NextLine => expr.typ = ScalarType::Float,
            Expr::Concatenation(vals) => {
                expr.typ = ScalarType::String;
                for val in vals {
                    self.analyze_expr(val, func_state, false)?;
                }
            }
            Expr::ArrayIndex { indices, name } => {
                self.use_as_array(name, func_state)?;
                for idx in indices {
                    self.analyze_expr(idx, func_state, false)?;
                }
            }
            Expr::InArray { indices, name } => {
                self.use_as_array(name, func_state)?;
                for idx in indices {
                    self.analyze_expr(idx, func_state, false)?;
                }
            }
            Expr::ArrayAssign { indices, name, value } => {
                self.use_as_array(name, func_state)?;
                for idx in indices {
                    self.analyze_expr(idx, func_state, false)?;
                }
                self.analyze_expr(value, func_state, false)?;
                expr.typ = value.typ;
            }
        };
        Ok(())
    }

    fn merge_maps(children: &[&MapT]) -> MapT {
        let mut merged = MapT::new();
        for map in children {
            for (name, var_type) in map.into_iter() {
                if let Some(existing_type) = merged.get(name) {
                    merged = merged
                        .insert(
                            name.clone(),
                            TypeAnalysis::merge_types(existing_type, var_type),
                        )
                        .0;
                } else {
                    merged = merged.insert(name.clone(), *var_type).0;
                }
            }
        }
        merged
    }
    fn merge_types(a: &ScalarType, b: &ScalarType) -> ScalarType {
        match (a, b) {
            (ScalarType::Float, ScalarType::Float) => ScalarType::Float,
            (ScalarType::String, ScalarType::String) => ScalarType::String,
            _ => ScalarType::Variable,
        }
    }
}

#[cfg(test)]
fn test_exception(program: &str, error_includes_msg: &str) {
    use crate::{lex, parse};
    let mut ast = parse(lex(program).unwrap());
    let result = analyze(&mut ast);
    if let Err(err) = result {
        println!("Error msg: `{}\nShould include: `{}`", err.msg, error_includes_msg);
        assert!(err.msg.contains(error_includes_msg));
    } else {
        assert!(false, "type check should have failed with {}", error_includes_msg)
    }
}

#[cfg(test)]
fn test_it(program: &str, expected: &str) {
    fn strip(data: &str) -> String {
        data.replace("\n", "")
            .replace(" ", "")
            .replace("\t", "")
            .replace(";", "")
    }

    use crate::{lex, parse};
    let mut ast = parse(lex(program).unwrap());
    analyze(&mut ast).unwrap();
    println!("prog: {:?}", ast.main);
    let result_clean = strip(&format!("{}", ast));
    let expected_clean = strip(expected);
    if result_clean != expected_clean {
        println!("Got: \n{}", format!("{}", ast));
        println!("Expected: \n{}", expected);
    }
    assert_eq!(result_clean, expected_clean);
}

#[test]
fn test_typing_basic() {
    test_it("BEGIN { print \"a\" }", "print (s \"a\")");
}

#[test]
fn test_typing_basic2() {
    test_it("BEGIN { print 123 }", "print (f 123)");
}

#[test]
fn test_if_basic() {
    test_it(
        "BEGIN { a = 1; print a; if($1) { print a } } ",
        "(f a = (f 1)); print (f a); if (s $(f 1)) { print (f a) }",
    );
}

#[test]
fn test_if_polluting() {
    test_it(
        "BEGIN { a = 1; print a; if($1) { a = \"a\"; } print a; print a;    } ",
        "(f a = (f 1)); print (f a); if (s $(f 1)) { (s a = (s \"a\")); } print (v a); print (v a)",
    );
}

#[test]
fn test_if_nonpolluting() {
    test_it(
        "BEGIN { a = 1; print a; if($1) { a = 5; } print a; } ",
        "(f a = (f 1)); print (f a); if (s $(f 1)) { (f a = (f 5)); } print (f a);",
    );
}

#[test]
fn test_ifelse_polluting() {
    test_it("BEGIN { a = 1; print a; if($1) { a = 5; } else { a = \"a\" } print a; } ",
            "(f a = (f 1)); print (f a); if (s $(f 1)) { (f a = (f 5)); } else { (s a = (s \"a\")) } print (v a);");
}

#[test]
fn test_ifelse_swapping() {
    test_it("BEGIN { a = 1; print a; if($1) { a = \"a\"; } else { a = \"a\" } print a; } ",
            "(f a = (f 1)); print (f a); if (s $(f 1)) { (s a = (s \"a\")); } else { (s a = (s \"a\")) } print (s a);");
}

#[test]
fn test_ifelse_swapping_2() {
    test_it("BEGIN { a = \"a\"; print a; if($1) { a = 3; } else { a = 4 } print a; } ",
            "(s a = (s \"a\")); print (s a); if (s $(f 1)) { (f a = ( f 3)); } else { (f a = (f 4)) } print (f a);");
}

#[test]
fn test_if_else_polluting() {
    test_it("BEGIN { a = 1; print a; if($1) { a = \"a\"; } else { a = \"a\" } print a; } ",
            "(f a = (f 1)); print (f a); if (s $(f 1)) { (s a = (s \"a\"); ) } else { (s a = (s \"a\")); } print (s a)");
}

#[test]
fn test_concat_loop() {
    test_it(
        "{ a = a $1 } END { print a; }",
        "while (f check_if_there_is_another_line) { (s a = (s (s a) (s$(f 1)))) }; print (s a);",
    );
}

#[test]
fn test_while_loop() {
    test_it(
        "BEGIN { while(123) { a = \"bb\"}; print a;}",
        "while (f 123) { (s a = (s \"bb\")) }; print (s a);",
    );
}

#[test]
fn test_assignment() {
    test_it("BEGIN { x = 0; print x; }", "(f x = (f 0 )); print (f x);");
}

#[test]
fn test_assignment_col() {
    test_it(
        "{ x = $0; } END { print x; }",
        "while(fcheck_if_there_is_another_line){ (s x = (s$(f 0) ))}; print (s x);",
    );
}


#[test]
fn test_ternary() {
    test_it("\
    BEGIN { x = \"a\"; x ? (x=1) : (x=2); print x; }",
            "(s x = (s \"a\")); \n(f (s x) ? (f x = (f 1)) : (f x = (f 2))); \nprint (f x)");
}

#[test]
fn test_ternary_2() {
    test_it("\
    BEGIN { x = \"a\"; x ? (x=1) : (x=\"a\"); print x; }",
            "(s x = (s \"a\")); \n(v (s x) ? (f x = (f 1)) : (s x = (s \"a\"))); \nprint (v x)");
}

#[test]
fn test_ternary_3() {
    test_it("\
    BEGIN { x ? (x=1) : (x=\"a\"); print x; }",
            "(v (s x) ? (f x = (f 1)) : (s x = (s \"a\"))); \nprint (v x)");
}

#[test]
fn test_ternary_4() {
    test_it("\
    BEGIN { x ? (x=1) : (x=4); print x; }",
            "(f (s x) ? (f x = (f 1)) : (f x = (f 4)));\nprint (f x)");
}

#[test]
fn test_fails() {
    use crate::{lex, parse};
    let mut ast = parse(lex("BEGIN { a = 0; a[0] = 1; }").unwrap());
    let res = analyze(&mut ast);
    assert!(res.is_err());
}

#[test]
fn test_fails_2() {
    use crate::{lex, parse};
    let mut ast = parse(lex("BEGIN { a[0] = 1; a = 0;  }").unwrap());
    let res = analyze(&mut ast);
    assert!(res.is_err());
}

#[test]
fn test_fails_3() {
    use crate::{lex, parse};
    let mut ast = parse(lex("BEGIN { if(x) { a[0] = 1; } a = 0;  }").unwrap());
    let res = analyze(&mut ast);
    assert!(res.is_err());
}

#[test]
fn test_typing_scalar_function() {
    test_it("function a() { return 1; } BEGIN { print 1; }",
            "function a() { return (f 1); } print (f 1);");
}

#[test]
fn test_arr_typing() {
    test_it("BEGIN { b[0] = d; }",
            "(s b[(f 0)] = (s d))");
}

#[test]
fn test_func_arg_typing() {
    test_it("function a(b,c,d) { b[0] = 1; c[0] = 1; return d } BEGIN { }",
            "function a((ab), (ac), (ud)) {     (f b[(f0)]=(f1)) (f c[(f0)]= (f1)) return(vd)  }");
}

#[test]
fn test_typing_array_fails_mixed_ret() {
    test_exception("function a(arg) { if(arg) { return 1; } b[0] = 2; return b } BEGIN { print 0; }", "attempted to use")
}

#[test]
fn test_typing_array_fails_no_ret() {
    test_exception("function a(arg) { if(arg) { b[0] = 1; return b; } } BEGIN { print 0; }", "attempt to use")
}

#[test]
fn mixed_func_array() {
    test_exception("function a() { } BEGIN { a[0] = 1; }", "attempt to use")
}

#[test]
fn mixed_func_scalar() {
    test_exception("function a() { } BEGIN { a = 1; }", "attempt to use")
}

#[test]
fn mixed_scalar_array() {
    test_exception("BEGIN { a[0] = 1; a = 5; }", "attempt to use")
}