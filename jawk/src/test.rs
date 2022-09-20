use crate::codgen::compile_and_capture;
use crate::{analyze, lex, parse, transform};
use std::path::PathBuf;
use std::time::{Duration, Instant};
use tempfile::tempdir;

const ONE_LINE: &'static str = "1 2 3\n";
const REDIRECT: &'static str = "2 3 4 5\n";
const NUMBERS: &'static str = "1 2 3\n4 5 6\n7 8 9";
const NUMBERS2: &'static str = "1 2 3 4\n4 5 6 5 \n7 8 9 4";
const FLOAT_NUMBERS: &'static str = "1.1 2.2 3.3\n4.4 5.5 6.6\n7.7 8.8 9.9";
const NUMERIC_STRING: &'static str = "1 2 3\n04 005 6\n07 8 9";

fn test_once(interpreter: &str, prog: &str, file: &PathBuf) -> (String, Duration) {
    // Run a single awk once and capture the output
    let start = Instant::now();
    let output = std::process::Command::new(interpreter)
        .args(vec![prog, file.to_str().unwrap()])
        .output()
        .unwrap();
    let dir = start.elapsed();
    (
        String::from_utf8(output.stdout).expect("cannot convert output to utf8"),
        dir,
    )
}

fn long_number_file() -> String {
    let mut string = String::new();
    let mut counter: i16 = 0;
    for _ in 0..1_000 {
        for idx in 0..1_000 {
            counter = counter.wrapping_add(idx);
            string.push_str(&format!("{} ", counter));
        }
        string.push('\n');
    }
    string
}
fn test_against(interpreter: &str, prog: &str, file: &PathBuf) {
    match std::process::Command::new(interpreter).output() {
        Ok(_) => {}
        Err(err) => return, // this interpreter doesn't exist
    }
    let mut ast = transform(parse(lex(prog).unwrap()));
    analyze(&mut ast);

    let files = vec![file.to_str().unwrap().to_string()];
    let ours = compile_and_capture(ast, files.as_slice()).unwrap();

    let output = test_once(interpreter, prog, file).0;
    assert_eq!(
        ours.output(),
        output,
        "LEFT jawk, RIGHT {} stdout didnt match for {}",
        interpreter,
        interpreter
    );

    // let mut ours_us = 0;
    // let mut theirs_us = 0;
    // for _ in 0..PERF_RUNS {
    //     ours_us += test_once("./target/release/jawk", prog, file).1.as_micros();
    //     theirs_us += test_once(interpreter, prog, file).1.as_micros();
    // }
    //
    // let ours_us_avg = Duration::from_micros((ours_us / 10) as u64).as_micros() / PERF_RUNS;
    // let theirs_us_avg = Duration::from_micros((theirs_us / 10) as u64).as_micros() / PERF_RUNS;
    //
    // println!(
    //     "{}us jawk vs {}us {}",
    //     ours_us_avg, theirs_us_avg, interpreter
    // );
    //
    // if ours_us_avg < 2000 && theirs_us_avg < 2000 {
    //     return;
    // }
    //
    // assert!(
    //     ours_us < theirs_us,
    //     "{} was faster! {}us vs {}us avg for 10 runs",
    //     interpreter,
    //     ours_us_avg,
    //     theirs_us_avg
    // );
}

fn test_it<S: AsRef<str>>(prog: &str, file: S, _expected_output: &str, _status: i32) {
    println!("Program:\n{}", prog);
    let mut ast = transform(parse(lex(&prog).unwrap()));
    analyze(&mut ast);
    println!("Ast:\n{}", ast);

    let temp_dir = tempdir().unwrap();
    let file_path = temp_dir.path().join("tmp");
    std::fs::write(file_path.clone(), file.as_ref()).unwrap();

    let file_path_string = file_path.to_str().unwrap().to_string();
    let res = compile_and_capture(ast, &[file_path_string]).unwrap();
    let string_in = res.strings_in();
    let string_out = res.strings_out();
    assert_eq!(
        string_in, string_out,
        "runtime strings_in didn't match string_out. Possible mem leak {} vs {}",
        string_in, string_out
    );

    test_against("awk", prog, &file_path);
    test_against("mawk", prog, &file_path);
    test_against("goawk", prog, &file_path);
    test_against("onetrueawk", prog, &file_path);
}

macro_rules! test {
    ($name:ident,$prog:expr,$file:expr,$stdout:expr,$status:expr) => {
        #[test]
        fn $name() {
            test_it($prog, $file, $stdout, $status);
        }
    };
}

test!(test_print_int, "{print 1;}", ONE_LINE, "1\n", 0);
test!(test_print_str, "{print \"abc\";}", ONE_LINE, "abc\n", 0);
test!(test_just_begin, "BEGIN { print 1; }", ONE_LINE, "1\n", 0);
test!(
    test_assign_to_undef,
    "BEGIN { print (x = x + 1); }",
    ONE_LINE,
    "1\n",
    0
);
test!(
    test_simple_exponential,
    "BEGIN { print (x = 2 ^ 2); }",
    ONE_LINE,
    "4\n",
    0
);
test!(
    test_simple_exponential_order_op_pre,
    "BEGIN { print (x = 3 * 2 ^ 2); }",
    ONE_LINE,
    "12\n",
    0
);
test!(
    test_simple_exponential_order_op_post,
    "BEGIN { print (x = 2 ^ 2 * 3); }",
    ONE_LINE,
    "12\n",
    0
);
test!(
    test_e2e_begin_end,
    "BEGIN { print 1; } END { print 3; } END { print 4; }",
    ONE_LINE,
    "1\n3\n4\n",
    0
);
test!(
    test_oo_beg_end,
    "END { print 3; } { print 2; } BEGIN {print 1;}",
    ONE_LINE,
    "1\n2\n3\n",
    0
);
test!(test_4_assgn, "{x = 4; print x }", ONE_LINE, "4\n", 0);
test!(test_cmpop2, "BEGIN { print (3 < 5) }", ONE_LINE, "1\n", 1);
test!(test_cmpop1, "BEGIN { print (5 < 3) }", ONE_LINE, "0\n", 0);
test!(
    test_dup_beg_end,
    "END { print 4; } END { print 3; } { print 2; } BEGIN { print 0; } BEGIN {print 1;} ",
    ONE_LINE,
    "0\n1\n2\n4\n3\n",
    0
);
test!(
    test_simple_assignment,
    "{x = 0; print x;}",
    ONE_LINE,
    "0\n",
    0
);
test!(test_simple_assgn, "{x = 0; print x }", ONE_LINE, "0\n", 0);
test!(
    test_assignment_in_ifs0,
    "{x = 0; if (1) { x = 1 }; print x }",
    ONE_LINE,
    "1\n",
    0
);
test!(
    test_assignment_in_ifs,
    "{x = 0; if (1) { x = 1 } else { x = 2.2 }; print x }",
    ONE_LINE,
    "1\n",
    0
);
test!(
    test_nested_if_assignment,
    "{x = 0; if (0) { x = 1 } else { x = 2.2 }; print x }",
    ONE_LINE,
    "2.2\n",
    0
);
test!(
    test_mixed_int_float_assignment,
    "{x = 0; if (x) { x = 1 } else { x = 2.2 }; print x }",
    ONE_LINE,
    "2.2\n",
    0
);
test!(test_deeply_nested_mixed_assignment, "{x = 0; if (1) { if (1) { x = 1 } else { x = 2.2 } } else { if (1) { x = 1 } else { x = 4.2 } }; print x }", ONE_LINE, "1\n", 0);
test!(test_deeply_nested_mixed_assignment2, "{x = 0; if (1) { if (1) { x = 1 } else { x = 2.2 } } else { if (1) { x = 1 } else { x = 4.2 } }; { x = 4; x=5; x=5.5; print x; } }", ONE_LINE, "5.5\n", 0);
test!(test_int_plus_float, "{print 1 + 1.1}", ONE_LINE, "2.1\n", 0);
test!(test_float_plus_int, "{print 1.1 + 1}", ONE_LINE, "2.1\n", 0);
test!(
    test_grouping,
    "{print (1.1 + 3.3) + 1}",
    ONE_LINE,
    "5.4\n",
    0
);
test!(test_float_add, "{print (1.0 + 2.0)}", ONE_LINE, "3\n", 0);
test!(
    test_column_access_1_line,
    "{print $1; print $2; print $3; print $0}",
    ONE_LINE,
    "1\n2\n3\n1 2 3\n",
    0
);
test!(
    test_column_access_many_line,
    "{print $1; print $2; print $3; print $0}",
    NUMBERS,
    "1\n2\n3\n1 2 3\n4\n5\n6\n4 5 6\n7\n8\n9\n7 8 9\n",
    0
);

test!(
    test_if_no_else_truthy,
    "{if (1) { print 123; }}",
    ONE_LINE,
    "123\n",
    0
);
test!(
    test_float_truthyness,
    "{if (0) { print \"abc\" } else { print \"cde\" }}",
    ONE_LINE,
    "cde\n",
    0
);
test!(
    test_float_truthyness2,
    "{if (1) { print \"abc\" } else { print \"cde\" }}",
    ONE_LINE,
    "abc\n",
    0
);
test!(
    test_float_truthyness3,
    "{if (100) { print \"abc\" } else { print \"cde\" }}",
    ONE_LINE,
    "abc\n",
    0
);
test!(
    test_float_truthyness4,
    "{if (1000) { print \"abc\" } else { print \"cde\" }}",
    ONE_LINE,
    "abc\n",
    0
);

test!(
    test_str_truthyness0,
    "{a = \"\"; if (a) { print 5 } }",
    ONE_LINE,
    "",
    0
);
test!(
    test_str_truthyness1,
    "{if (\"\") { print \"abc\" } else { print \"cde\" }}",
    ONE_LINE,
    "cde\n",
    0
);
test!(
    test_str_truthyness2,
    "{if (\"a\") { print \"abc\" } else { print \"cde\" }}",
    ONE_LINE,
    "abc\n",
    0
);
test!(
    test_str_truthyness3,
    "{if (\"aaaaklasdjksfdakljfadskljafsdkljfas\") { print \"abc\" } else { print \"cde\" }}",
    ONE_LINE,
    "abc\n",
    0
);
test!(test_str_truthyness4, "{if (\"aaaklasdjksfdakljfadskljafsdkljfasaaaaklasdjksfdakljfadskljafsdkljfasaaaaklasdjksfdakljfadskljafsdkljfasaaaaklasdjksfdakljfadskljafsdkljfasaaaaklasdjksfdakljfadskljafsdkljfasaaaaklasdjksfdakljfadskljafsdkljfasaaaaklasdjksfdakljfadskljafsdkljfasaaaaklasdjksfdakljfadskljafsdkljfasaaaaklasdjksfdakljfadskljafsdkljfasaaaaklasdjksfdakljfadskljafsdkljfasaaaaklasdjksfdakljfadskljafsdkljfas\") { print \"abc\" } else { print \"cde\" }}", ONE_LINE, "abc\n", 0);

test!(
    test_assign_then_print,
    "{ a = 1.1; print a }",
    ONE_LINE,
    "1.1\n",
    0
);
test!(
    test_assign_then_print_sep,
    "{ a = 1.1 } { print a }",
    ONE_LINE,
    "1.1\n",
    0
);
test!(
    test_assign_then_end,
    "{ a = 1.1 } END { print a }",
    ONE_LINE,
    "1.1\n",
    0
);
test!(
    test_print_col0,
    "{ a = $0 } END { print a }",
    NUMBERS,
    "7 8 9\n",
    0
);
test!(
    test_print_col1,
    "{ a = $1 } END { print a }",
    NUMBERS,
    "7\n",
    0
);
test!(
    test_print_col2,
    "{ a = $2 } END { print a }",
    NUMBERS,
    "8\n",
    0
);
test!(
    test_print_col3,
    "{ a = $3 } END { print a }",
    NUMBERS,
    "9\n",
    0
);
test!(
    test_print_col_big,
    "{ a = $44 } END { print a }",
    NUMBERS,
    "\n",
    0
);
test!(
    test_eqeq_true,
    "{ if (0==0) { print 123; } else {print 456;} }",
    ONE_LINE,
    "123\n",
    0
);
test!(
    test_eqeq_false,
    "{ if (0==1) { print 123; } else {print 456;} }",
    ONE_LINE,
    "456\n",
    0
);
test!(
    test_bangeq_true,
    "{ if (0!=0) { print 123; } else {print 456;} }",
    ONE_LINE,
    "456\n",
    0
);
test!(
    test_bangeq_false,
    "{ if (0!=1) { print 123; } else {print 456;} }",
    ONE_LINE,
    "123\n",
    0
);
test!(
    test_lt_true,
    "{ if (0 < 123) { print 123; } else {print 456;} }",
    ONE_LINE,
    "123\n",
    0
);
test!(
    test_lt_false,
    "{ if (123 < 12) { print 123; } else {print 456;} }",
    ONE_LINE,
    "456\n",
    0
);
test!(
    test_lteq_true,
    "{ if (0 <= 1) { print 123; } else {print 123;} }",
    ONE_LINE,
    "123\n",
    0
);
test!(
    test_lteq_false,
    "{ if (1 <= 0) { print 123; } else {print 456;} }",
    ONE_LINE,
    "456\n",
    0
);
test!(
    test_gt_true,
    "{ if (1 > 0) { print 123; } else {print 456;} }",
    ONE_LINE,
    "123\n",
    0
);
test!(
    test_gt_false,
    "{ if (0 > 1) { print 123; } else {print 456;} }",
    ONE_LINE,
    "456\n",
    0
);
test!(
    test_gteq_true,
    "{ if (1 >= 0) { print 123; } else {print 456;} }",
    ONE_LINE,
    "123\n",
    0
);
test!(
    test_gteq_false,
    "{ if (0 >= 1) { print 123; } else {print 456;} }",
    ONE_LINE,
    "456\n",
    0
);
test!(
    test_while,
    "{ while (x < 4) { x = x + 1; print x; } print 555; }",
    ONE_LINE,
    "1\n2\n3\n4\n555\n",
    0
);
test!(
    test_long_loop,
    "{ x = 0; while (x<50) { x = x + 1; } print x; }",
    ONE_LINE,
    "50\n",
    0
);
test!(
    test_if_no_else_truthy_str,
    "{if (1) { print \"truthy\"; }}",
    ONE_LINE,
    "truthy\n",
    0
);
test!(
    test_mixed_logical0,
    "BEGIN { x = 0; x = x && \"123\"; print x; }",
    ONE_LINE,
    "0\n",
    0
);
test!(
    test_mixed_logical1,
    "BEGIN { x = 1; x = x && \"123\"; print x; }",
    ONE_LINE,
    "1\n",
    0
);
test!(
    test_mixed_logical2,
    "BEGIN { x = 1; x = \"123\" && x; print x; }",
    ONE_LINE,
    "1\n",
    0
);
test!(
    test_mixed_logical3,
    "BEGIN { x = 1; x = x || \"123\"; print x; }",
    ONE_LINE,
    "1\n",
    0
);
test!(
    test_mixed_logical4,
    "BEGIN { x = 0; x = x || \"123\"; print x; }",
    ONE_LINE,
    "1\n",
    0
);
test!(
    test_mixed_logical5,
    "BEGIN { x = 0; x = x || \"\"; print x; }",
    ONE_LINE,
    "0\n",
    0
);
test!(
    test_mixed_logical6,
    "BEGIN { x = 1; x = \"123\" && x; print x; }",
    ONE_LINE,
    "1\n",
    0
);
test!(
    test_mixed_addition0,
    "BEGIN { x = x + \"123\"; print x; }",
    ONE_LINE,
    "123\n",
    0
);
test!(
    test_mixed_addition1,
    "BEGIN { x = 0; x = x + \"123\"; print x; }",
    ONE_LINE,
    "123\n",
    0
);
test!(
    test_mixed_addition2,
    "BEGIN { x = 0; x = x + \"123\"; x = x + 5; print x; }",
    ONE_LINE,
    "128\n",
    0
);
test!(
    test_mixed_addition3,
    "BEGIN { x = 0; x = x + (\"123\" + 44 + \"33\"); x = x + 5; print x; }",
    ONE_LINE,
    "205\n",
    0
);
test!(
    test_mixed_addition4,
    "BEGIN { x = 0; x = x + (\"1\" + 2); print x; }",
    ONE_LINE,
    "3\n",
    0
);
test!(
    test_assignment_expr,
    "BEGIN { x = (y = 123); print x}",
    ONE_LINE,
    "123\n",
    0
);
test!(
    test_assignment_expr2,
    "BEGIN { x = ((y = 123) + (z = 4)); print x}",
    ONE_LINE,
    "127\n",
    0
);
test!(
    test_nested_assignment,
    "BEGIN { a = b = c = d = e = f = 4 < 10; print d; print a; }",
    ONE_LINE,
    "1\n1\n",
    0
);
test!(
    test_short_circuit_or,
    "BEGIN { print (4 || ((4)/0)) }",
    ONE_LINE,
    "1\n",
    0
);
test!(
    test_short_circuit_or2,
    "BEGIN { print (4 || ((4)/0) || ((4)/0) )}",
    ONE_LINE,
    "1\n",
    0
);
test!(
    test_short_circuit_or3,
    "BEGIN { print (0 || 4) }",
    ONE_LINE,
    "1\n",
    0
);
test!(
    test_short_circuit_and,
    "BEGIN { print (0 && ((4)/0)) }",
    ONE_LINE,
    "0\n",
    0
);
test!(
    test_short_circuit_and2,
    "BEGIN { print (123 && 5) }",
    ONE_LINE,
    "1\n",
    0
);
test!(
    test_short_circuit_and3,
    "BEGIN { print (123 && 0) }",
    ONE_LINE,
    "0\n",
    0
);
test!(test_nested_column, "{ print ($$$$1) }", REDIRECT, "5\n", 0);
test!(
    test_nested_column_oop,
    "{ print ($$$$1 + 100) }",
    REDIRECT,
    "105\n",
    0
);
test!(
    test_concat,
    "BEGIN { print (\"a\" \"b\") }",
    REDIRECT,
    "ab\n",
    0
);
test!(
    test_concat2,
    "BEGIN { print (\"a\" \"b\" \"cccc\" \"ddd\") }",
    REDIRECT,
    "abccccddd\n",
    0
);
test!(
    test_concat3,
    "BEGIN { a = \"a\"; print (a \"b\") }",
    REDIRECT,
    "ab\n",
    0
);
test!(
    test_concat_cols,
    "BEGIN { a = \"a\"; print (a) }",
    ONE_LINE,
    "a\n",
    0
);
test!(test_concat_cols2, "{ print ($1 $2) }", ONE_LINE, "12\n", 0);
test!(
    test_concat_cols3,
    "{ print ($1 $2 $3) }",
    ONE_LINE,
    "123\n",
    0
);
test!(
    test_concat_multiline,
    "{ a = a $1;} END{ print a}",
    NUMBERS,
    "147\n",
    0
);
test!(
    test_concat_multiline_intermed,
    "{ a = a $1; print a}",
    NUMBERS,
    "1\n14\n147\n",
    0
);

test!(
    test_binop_1,
    "BEGIN { print (\"a\" < \"a\") }",
    ONE_LINE,
    "0\n",
    0
);
test!(
    test_binop_2,
    "BEGIN { print (\"a\" < \"aa\") }",
    ONE_LINE,
    "1\n",
    0
);
test!(
    test_binop_3,
    "BEGIN { print (\"a\" > \"a\") }",
    ONE_LINE,
    "0\n",
    0
);
test!(
    test_binop_4,
    "BEGIN { print (\"a\" > \"aa\") }",
    ONE_LINE,
    "0\n",
    0
);
test!(
    test_binop_5,
    "BEGIN { print (\"aaaa\" > \"aa\") }",
    ONE_LINE,
    "1\n",
    0
);
test!(
    test_binop_6,
    "BEGIN { print (\"a\" <= \"a\") }",
    ONE_LINE,
    "1\n",
    0
);
test!(
    test_binop_7,
    "BEGIN { print (\"a\" >= \"a\") }",
    ONE_LINE,
    "1\n",
    0
);
test!(
    test_binop_8,
    "BEGIN { print (\"a\" >= \"aaa\") }",
    ONE_LINE,
    "0\n",
    0
);
test!(
    test_binop_9,
    "BEGIN { print (\"aaaaaaaa\" >= \"aaa\") }",
    ONE_LINE,
    "1\n",
    0
);
test!(
    test_binop_10,
    "BEGIN { print (\"aaa\" == \"aaa\") }",
    ONE_LINE,
    "1\n",
    0
);
test!(
    test_binop_11,
    "BEGIN { print (\"aaa\" == \"aafa\") }",
    ONE_LINE,
    "0\n",
    0
);
test!(
    test_binop_12,
    "BEGIN { print (\"aaa\" != \"aaa\") }",
    ONE_LINE,
    "0\n",
    0
);
test!(
    test_binop_13,
    "BEGIN { print (\"aaa3\" != \"aaa\") }",
    ONE_LINE,
    "1\n",
    0
);
test!(
    test_assign_ops,
    "BEGIN { a = 1; b = 3; a += b += 4; print a; print b; }",
    ONE_LINE,
    "8\n7\n",
    0
);
test!(test_assign_ops_2, "BEGIN { a = 1; b = 3; c = 5; d = 7; a += b +=c -= d ^= 3; print a; print b; print c; print d  }", ONE_LINE, "-334\n-335\n-338\n343\n", 0);
test!(test_looping_concat, "BEGIN { a = \"\"; b = \"\"; x = 0; while (x < 50) {a = a \"a\"; b = b \"b\"; x += 1; } print a; print b; print x; }", ONE_LINE, "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\nbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\n50\n", 0);
test!(
    test_concat_undef,
    "BEGIN { a = a \"a\"; print a; }",
    ONE_LINE,
    "a\n",
    0
);

test!(test_loop_concat_long1, "BEGIN {a = \"\";        b = \"\";        x = 0;        while (x < 100) { a = a \"a\";                b = b \"a\";                x = x + 1;                if (a > b) {print \"a is not eq to b\";                }}print x;        print \"done\";}", ONE_LINE, ".", 0);
test!(test_loop_concat_long2, "BEGIN {a = \"\";        b = \"\";        x = 0;        while (x < 100) { a = a \"a\";                b = b \"a\";                x = x + 1;                if (a != b) {print \"a is not eq to b\";                }}print x;        print \"done\";}", ONE_LINE, ".", 0);

test!(test_pattern_only_1_4, "$1 == $4", NUMBERS, ".", 0);

// TODO: Efficient IO
// test!(test_pattern_long, "$1 == $4", long_number_file(), ".", 0);

// TODO: Numeric strings
// test!(
//     test_numeric_string1,
//     "{ print ($1 > 2 ) }",
//     NUMERIC_STRING,
//     ".",
//     0
// );

test!(
    test_numeric_string2,
    "{ print ($0 < $1 ) }",
    NUMERIC_STRING,
    ".",
    0
);
test!(
    test_numeric_string3,
    "{ print (\"04\" > \"005\") }",
    NUMERIC_STRING,
    ".",
    0
);
test!(
    test_numeric_string4,
    "{ print (\"04\" >= \"005\") }",
    NUMERIC_STRING,
    ".",
    0
);
test!(
    test_post_increment,
    "BEGIN { a = 4; print a++ + a++}",
    NUMERIC_STRING,
    "9\n",
    0
);
test!(
    test_post_decrement,
    "BEGIN { a = 4; print a-- - a--}",
    NUMERIC_STRING,
    "1\n",
    0
);
test!(
    test_post_decrement_and_increment,
    "BEGIN { a = 4; print a++ - a--}",
    NUMERIC_STRING,
    "-1\n",
    0
);
test!(
    test_exp_post_increment,
    "BEGIN { a = 3; print 2 ^ a++; print a }",
    NUMERIC_STRING,
    "8\n4\n",
    1
);
test!(
    test_post_increment_exp,
    "BEGIN { a = 3; print a++ ^ 2; print a}",
    NUMERIC_STRING,
    "9\n4\n",
    1
);
test!(
    test_pre_increment,
    "BEGIN { a = 3; print ++a; print a}",
    NUMERIC_STRING,
    "4\n4\n",
    1
);
test!(
    test_pre_decrement,
    "BEGIN { a = 3; print --a; print a}",
    NUMERIC_STRING,
    "2\n2\n",
    1
);
test!(
    test_post_pre_increment,
    "BEGIN { a = 3; print a++ + ++a; print a}",
    NUMERIC_STRING,
    "7\n5\n",
    1
);

test!(
    test_post_pre_decrement,
    "BEGIN { a = 3; print a-- + --a; print a}",
    NUMERIC_STRING,
    "4\n1\n",
    1
);
test!(
    test_mod_2,
    "BEGIN { print (3 % 2) }",
    NUMERIC_STRING,
    ".",
    1
);
test!(
    test_ternary_false,
    "BEGIN { print 0 ? 1 : 2 }",
    NUMERIC_STRING,
    "2\n",
    1
);
test!(
    test_ternary_true,
    "BEGIN { print 1 ? 1 : 2 }",
    NUMERIC_STRING,
    "1\n",
    1
);
test!(
    test_ternary_arith,
    "BEGIN { print 1 ? 1+1 : 2+2 }",
    NUMERIC_STRING,
    "2\n",
    1
);


test!(
    test_ternary_nested,
    "BEGIN { x = 2; y = 3; print x ? ( y ? \"true\" : 3 ) : 4 }",
    ONE_LINE,
    "true\n",
    0
);


test!(test_ternary_nested_flat1, "BEGIN { x = 3; y = 0; print x ? y ? 33 : 44 : 55; }", ONE_LINE, "44\n", 0);
test!(test_ternary_nested_flat2, "BEGIN { x = 0; y = 0; print x ? y ? 33 : 44 : 55; }", ONE_LINE, "55\n", 0);
test!(test_ternary_nested_flat3, "BEGIN { x = 0; z = 3; print x ? y : z ? 2 : 3 }", ONE_LINE, "2\n", 0);
test!(test_ternary_nested_flat4, "BEGIN { x = 0; z = 3; y = 5; print (x ? 0 : 2) ? y : z ? 2 : 3 }", ONE_LINE, "5\n", 0);
