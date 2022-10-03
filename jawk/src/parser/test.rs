use crate::parser::{Program, parse, PatternAction, Stmt, TypedExpr, Expr, Function};
use crate::lex;
use crate::lexer::{MathOp, BinOp, LogicalOp};

#[cfg(test)]
macro_rules! num {
    ($value:expr) => {
        texpr!(Expr::NumberF64($value))
    };
}

#[cfg(test)]
macro_rules! bnum {
    ($value:expr) => {
        Box::new(texpr!(Expr::NumberF64($value)))
    };
}

#[cfg(test)]
macro_rules! btexpr {
    ($value:expr) => {
        Box::new(texpr!($value))
    };
}

#[cfg(test)]
macro_rules! texpr {
    ($value:expr) => {
        TypedExpr::new($value)
    };
}

#[cfg(test)]
macro_rules! mathop {
    ($a:expr, $op:expr, $b:expr) => {
        texpr!(Expr::MathOp($a, $op, $b))
    };
}

#[cfg(test)]
macro_rules! binop {
    ($a:expr, $op:expr, $b:expr) => {
        texpr!(Expr::BinOp($a, $op, $b))
    };
}

#[cfg(test)]
macro_rules! sprogram {
    ($body:expr) => {
        Program::new(vec![], vec![], vec![PatternAction::new_action_only($body)], vec![])
    };
}

#[cfg(test)]
macro_rules! actual {
    ($name:ident, $body:expr) => {
        use crate::lexer::lex;
        let $name = parse(lex($body).unwrap());
    };
}

#[test]
fn test_ast_number() {
    use crate::lexer::lex;

    let prog = Program::new(vec![], vec![], vec![PatternAction::new_action_only(Stmt::Expr(mathop!(
                bnum!(1.0),
                MathOp::Plus,
                bnum!(2.0)
            )))], vec![]);
    assert_eq!(
        parse(lex("{1 + 2;}").unwrap()),
        prog
    );
}


#[test]
fn test_ast_oop() {
    use crate::lexer::lex;
    let left = bnum!(1.0);
    let right = Box::new(mathop!(bnum!(3.0), MathOp::Star, bnum!(2.0)));
    let expected = Program::new_action_only(Stmt::Expr(mathop!(left, MathOp::Plus, right)));
    let actual = parse(lex("{1 + 3 * 2;}").unwrap());
    assert_eq!(
        actual,
        expected, "\nactual {} expected {}", actual, expected
    );
}

#[test]
fn test_ast_oop_2() {
    use crate::lexer::lex;
    let left = Box::new(num!(2.0));
    let right = Box::new(texpr!(Expr::MathOp(
        Box::new(num!(1.0)),
        MathOp::Star,
        Box::new(num!(3.0))
    )));
    let mult = Stmt::Expr(texpr!(Expr::MathOp(right, MathOp::Plus, left)));
    assert_eq!(
        parse(lex("{1 * 3 + 2;}").unwrap()),
        Program::new_action_only(mult)
    );
}

#[test]
fn test_ast_assign() {
    use crate::lexer::lex;
    let stmt = Stmt::Expr(texpr!(Expr::ScalarAssign(format!("abc"), bnum!(2.0))));
    assert_eq!(
        parse(lex("{abc = 2.0; }").unwrap()),
        Program::new_action_only(stmt)
    );
}

#[test]
fn test_mathop_exponent() {
    use crate::lexer::lex;

    assert_eq!(
        parse(lex("{2 ^ 2;}").unwrap()),
        Program::new(
            vec![],
            vec![],
            vec![PatternAction::new_action_only(Stmt::Expr(mathop!(
                bnum!(2.0),
                MathOp::Exponent,
                bnum!(2.0)
            )))], vec![],
        )
    );
}

#[test]
fn test_mathop_exponent_2() {
    use crate::lexer::lex;
    let right = Box::new(num!(3.0));
    let left = Box::new(texpr!(Expr::MathOp(
        Box::new(num!(2.0)),
        MathOp::Exponent,
        Box::new(num!(2.0))
    )));
    let expo = Stmt::Expr(texpr!(Expr::MathOp(left, MathOp::Star, right)));

    assert_eq!(
        parse(lex("{2 ^ 2 * 3;}").unwrap()),
        Program::new_action_only(expo)
    );
}

#[test]
fn test_unary_op() {
    use crate::lexer::lex;
    let initial = Box::new(num!(1.0));
    let first = Box::new(texpr!(Expr::MathOp(
        Box::new(num!(0.0)),
        MathOp::Plus,
        initial
    )));
    let second = Box::new(texpr!(Expr::MathOp(
        Box::new(num!(0.0)),
        MathOp::Minus,
        first
    )));
    let third = Box::new(texpr!(Expr::MathOp(
        Box::new(num!(0.0)),
        MathOp::Plus,
        second
    )));

    let fourth = Stmt::Expr(texpr!(Expr::MathOp(
        Box::new(num!(0.0)),
        MathOp::Minus,
        third
    )));

    assert_eq!(
        parse(lex("{-+-+1;}").unwrap()),
        Program::new_action_only(fourth)
    );
}

#[test]
fn test_unary_op2() {
    use crate::lexer::lex;
    let initial = Box::new(num!(1.0));
    let first = Box::new(texpr!(Expr::BinOp(
        Box::new(num!(1.0)),
        BinOp::BangEq,
        initial
    )));
    let second = Box::new(texpr!(Expr::MathOp(
        Box::new(num!(0.0)),
        MathOp::Plus,
        first
    )));
    let third = Box::new(texpr!(Expr::BinOp(
        Box::new(num!(1.0)),
        BinOp::BangEq,
        second
    )));

    let fourth = Stmt::Expr(texpr!(Expr::MathOp(
        Box::new(num!(0.0)),
        MathOp::Minus,
        third
    )));

    let expected = parse(lex("{-!+!1;}").unwrap());
    let actual = Program::new_action_only(fourth);
    assert_eq!(actual, expected);
}

#[test]
fn test_if_else() {
    use crate::lexer::lex;
    let str = "{ if (1) { print 2; } else { print 3; }}";
    let actual = parse(lex(str).unwrap());
    assert_eq!(
        actual,
        Program::new_action_only(Stmt::If(
            num!(1.0),
            Box::new(Stmt::Print(num!(2.0))),
            Some(Box::new(Stmt::Print(num!(3.0)))),
        ))
    );
}

#[test]
fn test_if_only() {
    use crate::lexer::lex;
    let str = "{if (1) { print 2; }}";
    assert_eq!(
        parse(lex(str).unwrap()),
        Program::new_action_only(Stmt::If(num!(1.0), Box::new(Stmt::Print(num!(2.0))), None))
    );
}

#[test]
fn test_print() {
    use crate::lexer::lex;
    let str = "{print 1;}";
    assert_eq!(
        parse(lex(str).unwrap()),
        Program::new_action_only(Stmt::Print(num!(1.0)))
    );
}

#[test]
fn test_group() {
    use crate::lexer::lex;
    let str = "{{print 1; print 2;}}";
    assert_eq!(
        parse(lex(str).unwrap()),
        Program::new_action_only(Stmt::Group(vec![
            Stmt::Print(num!(1.0)),
            Stmt::Print(num!(2.0)),
        ]))
    );
}

#[test]
fn test_if_else_continues() {
    use crate::lexer::lex;
    let str = "{if (1) { print 2; } else { print 3; } 4.0;}";
    let actual = parse(lex(str).unwrap());
    assert_eq!(
        actual,
        Program::new_action_only(Stmt::Group(vec![
            Stmt::If(
                num!(1.0),
                Box::new(Stmt::Print(num!(2.0))),
                Some(Box::new(Stmt::Print(num!(3.0)))),
            ),
            Stmt::Expr(num!(4.0)),
        ]))
    );
}

#[test]
fn test_paser_begin_end() {
    use crate::lexer::lex;
    let str =
        "a { print 5; } BEGIN { print 1; } begin { print 2; } END { print 3; } end { print 4; }";
    let actual = parse(lex(str).unwrap());
    let begins = vec![Stmt::Print(num!(1.0)), Stmt::Print(num!(2.0))];
    let ends = vec![Stmt::Print(num!(3.0)), Stmt::Print(num!(4.0))];
    let generic = PatternAction::new(
        Some(texpr!(Expr::Variable("a".to_string()))),
        Stmt::Print(num!(5.0)),
    );
    assert_eq!(actual, Program::new(begins, ends, vec![generic], vec![]));
}

#[test]
fn test_pattern_only() {
    use crate::lexer::lex;
    let str = "test";
    let actual = parse(lex(str).unwrap());
    assert_eq!(
        actual,
        Program::new(
            vec![],
            vec![],
            vec![PatternAction::new_pattern_only(texpr!(Expr::Variable(
                "test".to_string()
            )))], vec![],
        )
    );
}

#[test]
fn test_print_no_semicolon() {
    use crate::lexer::lex;
    let str = "{ print 1 }";
    let actual = parse(lex(str).unwrap());
    assert_eq!(
        actual,
        Program::new(
            vec![],
            vec![],
            vec![PatternAction::new_action_only(Stmt::Print(num!(1.0)))], vec![])
    );
}

#[test]
fn test_column() {
    use crate::lexer::lex;
    let str = "$0+2 { print a; }";
    let actual = parse(lex(str).unwrap());
    let body = Stmt::Print(texpr!(Expr::Variable("a".to_string())));

    let col = Expr::Column(bnum!(0.0));
    let binop = texpr!(Expr::MathOp(btexpr!(col), MathOp::Plus, bnum!(2.0)));

    let pa = PatternAction::new(Some(binop), body);
    assert_eq!(actual, Program::new(vec![], vec![], vec![pa], vec![]));
}

#[test]
fn test_nested_column() {
    use crate::lexer::lex;
    let str = "$$0 { print a; }";
    let actual = parse(lex(str).unwrap());
    let body = Stmt::Print(texpr!(Expr::Variable("a".to_string())));

    let col = Expr::Column(bnum!(0.0));
    let col = Expr::Column(btexpr!(col));

    let pa = PatternAction::new(Some(texpr!(col)), body);
    assert_eq!(actual, Program::new(vec![], vec![], vec![pa], vec![]));
}

#[test]
fn test_while_l00p() {
    use crate::lexer::lex;
    let str = "{ while (123) { print 1; } }";
    let actual = parse(lex(str).unwrap());
    let body = Stmt::While(num!(123.0), Box::new(Stmt::Print(num!(1.0))));
    assert_eq!(
        actual,
        Program::new(vec![], vec![], vec![PatternAction::new_action_only(body)], vec![])
    );
}

#[test]
fn test_lt() {
    actual!(actual, "{ 1 < 3 }");
    let body = Stmt::Expr(texpr!(Expr::BinOp(bnum!(1.0), BinOp::Less, bnum!(3.0))));
    assert_eq!(actual, sprogram!(body));
}

#[test]
fn test_gt() {
    actual!(actual, "{ 1 > 3 }");
    let body = Stmt::Expr(texpr!(Expr::BinOp(bnum!(1.0), BinOp::Greater, bnum!(3.0))));
    assert_eq!(actual, sprogram!(body));
}

// test lteq
#[test]
fn test_lteq() {
    actual!(actual, "{ 1 <= 3 }");
    let body = Stmt::Expr(texpr!(Expr::BinOp(bnum!(1.0), BinOp::LessEq, bnum!(3.0))));
    assert_eq!(actual, sprogram!(body));
}

#[test]
fn test_gteq() {
    actual!(actual, "{ 1 >= 3 }");
    let body = Stmt::Expr(texpr!(Expr::BinOp(
        bnum!(1.0),
        BinOp::GreaterEq,
        bnum!(3.0)
    )));
    assert_eq!(actual, sprogram!(body));
}

#[test]
fn test_eqeq() {
    actual!(actual, "{ 1 == 3 }");
    let body = Stmt::Expr(texpr!(Expr::BinOp(bnum!(1.0), BinOp::EqEq, bnum!(3.0))));
    assert_eq!(actual, sprogram!(body));
}

#[test]
fn test_bangeq() {
    actual!(actual, "{ 1 != 3 }");
    let body = Stmt::Expr(texpr!(Expr::BinOp(bnum!(1.0), BinOp::BangEq, bnum!(3.0))));
    assert_eq!(actual, sprogram!(body));
}

#[test]
fn test_bangeq_oo() {
    actual!(actual, "{ 1 != 3*4 }");
    let body = Stmt::Expr(texpr!(Expr::BinOp(
        bnum!(1.0),
        BinOp::BangEq,
        Box::new(texpr!(Expr::MathOp(bnum!(3.0), MathOp::Star, bnum!(4.0))))
    )));
    assert_eq!(actual, sprogram!(body));
}

#[test]
fn test_cmp_oop1() {
    actual!(actual, "{ 3*3 == 9 }");
    let left = mathop!(bnum!(3.0), MathOp::Star, bnum!(3.0));
    let body = Stmt::Expr(binop!(Box::new(left), BinOp::EqEq, bnum!(9.0)));
    assert_eq!(actual, sprogram!(body));
}

#[test]
fn test_cmp_oop2() {
    actual!(actual, "{ a = 1*3 == 4 }");

    let left = texpr!(Expr::MathOp(bnum!(1.0), MathOp::Star, bnum!(3.0)));
    let body = btexpr!(Expr::BinOp(Box::new(left), BinOp::EqEq, bnum!(4.0)));
    let stmt = Stmt::Expr(texpr!(Expr::ScalarAssign(format!("a"), body)));
    assert_eq!(actual, sprogram!(stmt));
}

#[test]
fn test_for_loop() {
    actual!(actual, "{ for (a = 0; a < 1000; a = a + 1) { print a; } }");
    let a = format!("a");
    let init = texpr!(Expr::ScalarAssign(a.clone(), btexpr!(Expr::NumberF64(0.0))));
    let test = texpr!(Expr::BinOp(
        btexpr!(Expr::Variable(a.clone())),
        BinOp::Less,
        bnum!(1000.0)
    ));
    let incr = texpr!(Expr::ScalarAssign(
        a.clone(),
        btexpr!(Expr::MathOp(
            btexpr!(Expr::Variable(a.clone())),
            MathOp::Plus,
            btexpr!(Expr::NumberF64(1.0))
        ))
    ));
    let body = Stmt::Print(texpr!(Expr::Variable(a.clone())));
    let expected = Stmt::Group(vec![
        Stmt::Expr(init),
        Stmt::While(test, Box::new(Stmt::Group(vec![body, Stmt::Expr(incr)]))),
    ]);
    assert_eq!(actual, sprogram!(expected))
}

#[test]
fn test_logical_and() {
    actual!(actual, "{ a && b && c }");
    let a = btexpr!(Expr::Variable("a".to_string()));
    let b = btexpr!(Expr::Variable("b".to_string()));
    let c = btexpr!(Expr::Variable("c".to_string()));
    let a_and_b = btexpr!(Expr::LogicalOp(a, LogicalOp::And, b));
    let expected = Stmt::Expr(texpr!(Expr::LogicalOp(a_and_b, LogicalOp::And, c)));
    assert_eq!(actual, sprogram!(expected))
}

#[test]
fn test_logical_or() {
    actual!(actual, "{ a || b || c }");
    let a = btexpr!(Expr::Variable("a".to_string()));
    let b = btexpr!(Expr::Variable("b".to_string()));
    let c = btexpr!(Expr::Variable("c".to_string()));
    let a_and_b = btexpr!(Expr::LogicalOp(a, LogicalOp::Or, b));
    let expected = Stmt::Expr(texpr!(Expr::LogicalOp(a_and_b, LogicalOp::Or, c)));
    assert_eq!(actual, sprogram!(expected))
}

#[test]
fn string_concat() {
    actual!(actual, "{ print (a b) } ");
    let a = texpr!(Expr::Variable("a".to_string()));
    let b = texpr!(Expr::Variable("b".to_string()));
    let print = Stmt::Print(texpr!(Expr::Concatenation(vec![a, b])));
    assert_eq!(actual, sprogram!(print));
}

#[test]
fn string_concat2() {
    actual!(actual, "{ print (\"a\" \"b\") } ");
    let a = texpr!(Expr::String("a".to_string()));
    let b = texpr!(Expr::String("b".to_string()));
    let print = Stmt::Print(texpr!(Expr::Concatenation(vec![a, b])));
    assert_eq!(actual, sprogram!(print));
}

#[test]
fn string_concat_ooo() {
    actual!(actual, "{ print (a b - c) } ");
    let a = texpr!(Expr::Variable("a".to_string()));
    let b = btexpr!(Expr::Variable("b".to_string()));
    let c = btexpr!(Expr::Variable("c".to_string()));
    let b_minus_c = texpr!(Expr::MathOp(b, MathOp::Minus, c));
    let expected = Stmt::Print(texpr!(Expr::Concatenation(vec![a, b_minus_c])));
    assert_eq!(actual, sprogram!(expected));
}

#[test]
fn string_concat_ooo_2() {
    actual!(actual, "{ print (a - c b ) } ");
    let a = btexpr!(Expr::Variable("a".to_string()));
    let b = texpr!(Expr::Variable("b".to_string()));
    let c = btexpr!(Expr::Variable("c".to_string()));
    let a_minus_c = texpr!(Expr::MathOp(a, MathOp::Minus, c));
    let expected = Stmt::Print(texpr!(Expr::Concatenation(vec![a_minus_c, b])));
    assert_eq!(actual, sprogram!(expected));
}

#[test]
fn string_concat_ooo_3() {
    actual!(actual, "{ print (a < b c ) } ");
    let a = btexpr!(Expr::Variable("a".to_string()));
    let b = texpr!(Expr::Variable("b".to_string()));
    let c = texpr!(Expr::Variable("c".to_string()));
    let b_concat_c = btexpr!(Expr::Concatenation(vec![b, c]));
    let expected = Stmt::Print(texpr!(Expr::BinOp(a, BinOp::Less, b_concat_c)));
    assert_eq!(actual, sprogram!(expected));
}

#[test]
fn string_concat_ooo_4() {
    actual!(actual, "{ print (a b < c ) } ");
    let a = texpr!(Expr::Variable("a".to_string()));
    let b = texpr!(Expr::Variable("b".to_string()));
    let c = btexpr!(Expr::Variable("c".to_string()));
    let a_concat_b = btexpr!(Expr::Concatenation(vec![a, b]));
    let expected = Stmt::Print(texpr!(Expr::BinOp(a_concat_b, BinOp::Less, c)));
    assert_eq!(actual, sprogram!(expected));
}

#[test]
fn string_concat_two_cols() {
    actual!(actual, "{ print $1 $2 } ");
    let one = texpr!(Expr::Column(bnum!(1.0)));
    let two = texpr!(Expr::Column(bnum!(2.0)));
    let concat = texpr!(Expr::Concatenation(vec![one, two]));
    let print = Stmt::Print(concat);
    assert_eq!(actual, sprogram!(print));
}


#[test]
fn array_membership() {
    actual!(actual, "{ 1 in a } ");
    let expr = texpr!(Expr::InArray{name: "a".to_string(),  indices: vec![num!(1.0)]});
    let print = Stmt::Expr(expr);
    assert_eq!(actual, sprogram!(print));
}

#[test]
fn multi_dim_array_membership() {
    actual!(actual, "{ (1,2,3) in a } ");
    let expr = texpr!(Expr::InArray{name: "a".to_string(),  indices: vec![num!(1.0),num!(2.0),num!(3.0)]});
    let print = Stmt::Expr(expr);
    assert_eq!(actual, sprogram!(print));
}

#[test]
fn multi_multi_dim_array_membership() {
    actual!(actual, "{ (1,2,3) in a in b} ");
    let expr = texpr!(
        Expr::InArray{name: "b".to_string(),
            indices: vec![
                Expr::InArray{name: "a".to_string(),  indices: vec![num!(1.0),num!(2.0),num!(3.0)]}.into()]});
    let print = Stmt::Expr(expr);
    assert_eq!(actual, sprogram!(print));
}

#[test]
fn array_access() {
    actual!(actual, "{ a[0] }");
    let expr = texpr!(Expr::ArrayIndex{name: "a".to_string(),indices: vec![Expr::NumberF64(0.0).into()]});
    let stmt = Stmt::Expr(expr);
    assert_eq!(actual, sprogram!(stmt));
}


#[test]
fn array_access_multi() {
    actual!(actual, "{ a[0,1,2,3] }");
    let expr = texpr!(Expr::ArrayIndex{name: "a".to_string(),indices: vec![num!(0.0), num!(1.0),num!(2.0),num!(3.0)]});
    let stmt = Stmt::Expr(expr);
    assert_eq!(actual, sprogram!(stmt));
}

#[test]
fn array_access_multi_expr() {
    actual!(actual, "{ a[0+1] }");
    let zero = bnum!(0.0);
    let one = bnum!(1.0);
    let op = Expr::MathOp(zero, MathOp::Plus, one).into();
    let expr = texpr!(Expr::ArrayIndex{name: "a".to_string(),indices: vec![op]});
    let stmt = Stmt::Expr(expr);
    assert_eq!(actual, sprogram!(stmt));
}

#[test]
fn array_access_nested() {
    actual!(actual, "{ a[a[0]] }");
    let expr = texpr!(Expr::ArrayIndex{name: "a".to_string(),indices: vec![Expr::NumberF64(0.0).into()]});
    let outer = texpr!(Expr::ArrayIndex {name: "a".to_string(), indices: vec![expr]});
    assert_eq!(actual, sprogram!(Stmt::Expr(outer)));
}

#[test]
fn array_access_assign() {
    actual!(actual, "{ a[0] = 1 }");
    let expr = texpr!(Expr::ArrayAssign{name: "a".to_string(),indices: vec![Expr::NumberF64(0.0).into()], value: bnum!(1.0)});
    assert_eq!(actual, sprogram!(Stmt::Expr(expr)));
}


#[test]
fn array_access_assign_multi_dim() {
    actual!(actual, "{ a[0,2] = 1 }");
    let expr = Expr::ArrayAssign { name: "a".to_string(), indices: vec![num!(0.0), num!(2.0)], value: Box::new(num!(1.0)) }.into();
    assert_eq!(actual, sprogram!(Stmt::Expr(expr)));
}

#[test]
fn test_expr_call_nonary() {
    actual!(actual, "{ a() }");
    let expr = Expr::Call { target: "a".to_string(), args: vec![] };
    assert_eq!(actual, sprogram!(Stmt::Expr(expr.into())));
}

#[test]
fn test_expr_call_unary() {
    actual!(actual, "{ a(1) }");
    let expr = Expr::Call { target: "a".to_string(), args: vec![num!(1.0)] };
    assert_eq!(actual, sprogram!(Stmt::Expr(expr.into())));
}

#[test]
fn test_expr_call_many() {
    actual!(actual, "{ a(1,3,5) }");
    let expr = Expr::Call { target: "a".to_string(), args: vec![num!(1.0), num!(3.0), num!(5.0)] }.into();
    assert_eq!(actual, sprogram!(Stmt::Expr(expr)));
}

#[test]
fn array_assign_multi_expr() {
    actual!(actual, "{ a[0+1, a[0]] }");
    let zero = bnum!(0.0);
    let one = bnum!(1.0);
    let op = Expr::MathOp(zero, MathOp::Plus, one).into();
    let a_zero = Expr::ArrayIndex { name: "a".to_string(), indices: vec![num!(0.0)] }.into();
    let expr = texpr!(Expr::ArrayIndex{name: "a".to_string(),indices: vec![op, a_zero]});
    let stmt = Stmt::Expr(expr);
    assert_eq!(actual, sprogram!(stmt));
}

#[test]
fn test_printf_simple() {
    actual!(actual, "{ printf 1 }");
    let stmt = Stmt::Printf { fstring: num!(1.0), args: vec![] }.into();
    assert_eq!(actual, sprogram!(stmt));
}

#[test]
fn test_printf_multi() {
    actual!(actual, "{ printf \"%s%s%s\", 1, 2, 3 }");
    let stmt = Stmt::Printf { fstring: Expr::String("%s%s%s".to_string()).into(), args: vec![num!(1.0), num!(2.0), num!(3.0)] }.into();
    assert_eq!(actual, sprogram!(stmt));
}

#[test]
fn test_function() {
    actual!(actual, "function abc(a,b,c) { print 1; } BEGIN { print 1 }");
    let body = Stmt::Print(Expr::NumberF64(1.0).into());
    let function = Function::new("abc".to_string(), vec!["a".to_string(), "b".to_string(), "c".to_string()], body);
    let begin = Stmt::Print(Expr::NumberF64(1.0).into());
    assert_eq!(actual, Program::new(vec![begin], vec![], vec![], vec![function]))
}

#[test]
fn test_call() {
    actual!(actual, "BEGIN { a(1,\"2\"); }");
    let args = vec![
        Expr::NumberF64(1.0).into(),
        Expr::String("2".to_string()).into(),
    ];
    let begin = Stmt::Expr(Expr::Call { target: "a".to_string(), args }.into());
    assert_eq!(actual, Program::new(vec![begin], vec![], vec![], vec![]))
}

