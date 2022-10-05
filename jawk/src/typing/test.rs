use crate::analyze;

#[cfg(test)]
fn test_exception(program: &str, error_includes_msg: &str) {
    use crate::{lex, parse};
    let mut ast_result = analyze(parse(lex(program).unwrap()));
    if let Err(err) = ast_result {
        println!("Error msg: `{}\nShould include: `{}`", err.msg, error_includes_msg);
        assert!(err.msg.contains(error_includes_msg));
    } else {
        assert!(false, "type check should have failed with {}", error_includes_msg)
    }
}

#[cfg(test)]
fn strip(data: &str) -> String {
    let data: String = data.replace("\n", "")
        .replace(" ", "")
        .replace("\t", "")
        .replace(";", "")
        .replace("\n", "");
    println!("data1: {}", data);
    if let Some(rest) = data.strip_prefix("functionmainfunction(){") {
        return rest.strip_suffix("}").unwrap().to_string();
    }
    data
}
#[cfg(test)]
fn test_it(program: &str, expected: &str) {
    use crate::{lex, parse};
    let mut ast = analyze(parse(lex(program).unwrap())).unwrap();
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

#[test]
fn test_fails() {
    use crate::{lex, parse};
    let mut res = analyze(parse(lex("BEGIN { a = 0; a[0] = 1; }").unwrap()));
    assert!(res.is_err());
}

#[test]
fn test_fails_2() {
    use crate::{lex, parse};
    let mut ast = analyze(parse(lex("BEGIN { a[0] = 1; a = 0;  }").unwrap()));
    assert!(ast.is_err());
}

#[test]
fn test_fails_3() {
    use crate::{lex, parse};
    let mut ast = analyze(parse(lex("BEGIN { if(x) { a[0] = 1; } a = 0;  }").unwrap()));
    assert!(ast.is_err());
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

#[test]
fn call_func_with_arr_args() {
    test_it("function a(b) { b[0] = 1; } BEGIN { a(d) }",
            "function a((ab)) {     (f b[(f0)]=(f1)) } BEGIN { a(a-d) ");
}