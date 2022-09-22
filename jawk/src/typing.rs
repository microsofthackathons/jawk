use crate::codgen::variable_extract;
use crate::parser::{ScalarType, Stmt, TypedExpr};
use crate::Expr;
use immutable_chunkmap::map::Map;

pub type MapT = Map<String, ScalarType, 1000>;

pub fn analyze(stmt: &mut Stmt) {
    let mut map = MapT::new();
    let (vars, _) = variable_extract::extract(stmt);
    for var in vars {
        map = map.insert(var, ScalarType::String).0;
    }
    TypeAnalysis { map }.analyze_stmt(stmt)
}

struct TypeAnalysis {
    map: MapT,
}

impl TypeAnalysis {
    pub fn analyze_stmt(&mut self, stmt: &mut Stmt) {
        match stmt {
            Stmt::Expr(expr) => self.analyze_expr(expr),
            Stmt::Print(expr) => self.analyze_expr(expr),
            Stmt::Group(grouping) => {
                for stmt in grouping {
                    self.analyze_stmt(stmt);
                }
            }
            Stmt::If(test, if_so, if_not) => {
                self.analyze_expr(test);
                let mut if_so_map = self.map.clone();
                let mut if_not_map = self.map.clone();
                std::mem::swap(&mut if_so_map, &mut self.map);

                self.analyze_stmt(if_so);
                std::mem::swap(&mut if_so_map, &mut self.map);
                std::mem::swap(&mut if_not_map, &mut self.map);
                if let Some(else_case) = if_not {
                    self.analyze_stmt(else_case)
                }
                std::mem::swap(&mut if_not_map, &mut self.map);
                self.map = TypeAnalysis::merge_maps(&[&if_so_map, &if_not_map]);
            }
            Stmt::While(test, body) => {
                self.analyze_expr(test);

                let after_test_map = self.map.clone();

                self.analyze_stmt(body);

                let after_body_map = self.map.clone();

                self.map = TypeAnalysis::merge_maps(&[&after_test_map, &after_body_map]);

                self.analyze_expr(test);

                let after_test_map = self.map.clone();
                self.analyze_stmt(body);
                let after_body_map = self.map.clone();
                self.map = TypeAnalysis::merge_maps(&[&after_test_map, &after_body_map]);
            }
        }
    }

    pub fn analyze_expr(&mut self, expr: &mut TypedExpr) {
        match &mut expr.expr {
            Expr::NumberF64(_) => {
                expr.typ = ScalarType::Float;
            }
            Expr::String(_) => {
                expr.typ = ScalarType::String;
            }
            Expr::BinOp(left, _op, right) => {
                self.analyze_expr(left);
                self.analyze_expr(right);
                expr.typ = ScalarType::Float;
            }
            Expr::MathOp(left, _op, right) => {
                self.analyze_expr(left);
                self.analyze_expr(right);
                expr.typ = ScalarType::Float;
            }
            Expr::LogicalOp(left, _op, right) => {
                self.analyze_expr(left);
                self.analyze_expr(right);
                expr.typ = ScalarType::Float;
            }
            Expr::Assign(var, value) => {
                self.analyze_expr(value);
                self.map = self.map.insert(var.clone(), value.typ).0;
                expr.typ = value.typ;
            }
            Expr::Regex(_) => {
                expr.typ = ScalarType::String;
            }
            Expr::Ternary(cond, expr1, expr2) => {

                self.analyze_expr(cond);
                let mut if_so_map = self.map.clone();
                let mut if_not_map = self.map.clone();
                std::mem::swap(&mut if_so_map, &mut self.map);

                self.analyze_expr(expr1);
                std::mem::swap(&mut if_so_map, &mut self.map);
                std::mem::swap(&mut if_not_map, &mut self.map);
                self.analyze_expr(expr2);
                std::mem::swap(&mut if_not_map, &mut self.map);
                self.map = TypeAnalysis::merge_maps(&[&if_so_map, &if_not_map]);
                expr.typ = Self::merge_types(&expr1.typ, &expr2.typ);
            }
            Expr::Variable(var) => {
                if let Some(typ) = self.map.get(var) {
                    expr.typ = *typ;
                } else {
                    expr.typ = ScalarType::String;
                }
            }
            Expr::Column(col) => {
                expr.typ = ScalarType::String;
                self.analyze_expr(col);
            }
            Expr::Call => expr.typ = ScalarType::Float,
            Expr::Concatenation(vals) => {
                expr.typ = ScalarType::String;
                for val in vals {
                    self.analyze_expr(val);
                }
            }
            Expr::Index { .. } => {todo!("array typing")}
            Expr::InArray { .. } => {todo!("array typing")}
        }
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
fn test_it(program: &str, expected: &str) {
    fn strip(data: &str) -> String {
        data.replace("\n", "")
            .replace(" ", "")
            .replace("\t", "")
            .replace(";", "")
    }

    use crate::{lex, parse, transform};
    let mut ast = transform(parse(lex(program).unwrap()));
    analyze(&mut ast);
    println!("prog: {:?}", ast);
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