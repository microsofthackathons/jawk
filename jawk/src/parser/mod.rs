mod types;

use crate::lexer::{BinOp, LogicalOp, MathOp, Token, TokenType};
pub use crate::parser::types::PatternAction;
pub use types::{ScalarType, Expr, Program, Stmt, TypedExpr};

// Pattern Action Type
// Normal eg: $1 == "a" { doSomething() }
// Begin 'BEGIN { ... }'
// End  'END { .... }'
enum PAType {
    Normal(PatternAction),
    Begin(Stmt),
    End(Stmt),
}

pub fn parse(tokens: Vec<Token>) -> Program {
    let mut parser = Parser { tokens, current: 0 };
    parser.parse()
}

struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    fn parse(&mut self) -> Program {
        let mut begin = vec![];
        let mut end = vec![];
        let mut generic = vec![];
        while !self.is_at_end() {
            match self.pattern_action() {
                PAType::Normal(pa) => generic.push(pa),
                PAType::Begin(pa) => begin.push(pa),
                PAType::End(pa) => end.push(pa),
            }
        }
        Program::new(begin, end, generic)
    }

    fn check(&mut self, typ: TokenType) -> bool {
        if self.is_at_end() {
            false
        } else {
            typ == self.peek().ttype()
        }
    }

    fn consume(&mut self, typ: TokenType, message: &str) -> Token {
        if self.check(typ.clone()) {
            return self.advance();
        }
        panic!(
            "{} - didn't find a {} as expected. Found a {} {:?}",
            message,
            TokenType::name(typ),
            TokenType::name(self.peek().ttype()),
            self.peek()
        );
    }

    fn matches(&mut self, tokens: Vec<TokenType>) -> bool {
        let tkn = match self.tokens.get(self.current) {
            None => return false,
            Some(t) => t.ttype().clone(),
        };
        for expected in tokens.iter() {
            if *expected == tkn {
                self.advance();
                return true;
            }
        }
        false
    }

    fn previous(&self) -> Option<Token> {
        if self.current == 0 {
            return None;
        }
        Some(self.tokens[self.current - 1].clone())
    }

    fn peek_at(&self, idx: usize) -> Token {
        if let Some(t) = self.tokens.get(idx) {
            t.clone()
        } else {
            Token::EOF
        }
    }

    fn peek(&self) -> Token {
        self.peek_at(self.current)
    }

    fn peek_next(&self) -> Token {
        self.peek_at(self.current + 1)
    }

    fn peek_next_next(&self) -> Token {
        self.peek_at(self.current + 2)
    }

    fn is_at_end(&self) -> bool {
        self.tokens[self.current].ttype() == TokenType::EOF
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous().unwrap()
    }

    fn pattern_action(&mut self) -> PAType {
        let b = if self.matches(vec![TokenType::LeftBrace]) {
            // { print 1; }
            let pa = PAType::Normal(PatternAction::new_action_only(self.stmts()));
            self.consume(TokenType::RightBrace, "Expected '}' after action block");
            pa
        } else if self.matches(vec![TokenType::Begin]) {
            // BEGIN { print 1; }
            self.consume(TokenType::LeftBrace, "Expected a '{' after a begin");
            let pa = PAType::Begin(self.stmts());
            self.consume(TokenType::RightBrace, "Begin action should end with '}'");
            pa
        } else if self.matches(vec![TokenType::End]) {
            // END { print 1; }
            self.consume(TokenType::LeftBrace, "Expected a {' after a end");
            let pa = PAType::End(self.stmts());
            self.consume(TokenType::RightBrace, "End action should end with '}'");
            pa
        } else {
            let test = self.expression();
            if self.matches(vec![TokenType::LeftBrace]) {
                // test { print 1; }
                let pa = PAType::Normal(PatternAction::new(Some(test), self.stmts()));
                self.consume(TokenType::RightBrace, "Patern action should end with '}'");
                pa
            } else {
                // test
                // ^ implicitly prints line if test passes
                PAType::Normal(PatternAction::new_pattern_only(test))
            }
        };
        b
    }
    fn group(&mut self) -> Stmt {
        self.consume(TokenType::LeftBrace, "Expected a '}'");
        let s = self.stmts();
        self.consume(TokenType::RightBrace, "Expected a '}'");
        s
    }

    fn stmt_and_optional_semicolon(&mut self) -> Stmt {
        let stmt = self.stmt();
        if self.peek().ttype() == TokenType::Semicolon {
            self.consume(TokenType::Semicolon, "not possible");
        }
        stmt
    }

    fn stmt(&mut self) -> Stmt {
        let stmt = if self.matches(vec![TokenType::Print]) {
            Stmt::Print(self.expression())
        } else if self.matches(vec![TokenType::For]) {
            self.consume(TokenType::LeftParen, "Expected a '(' after the for keyword");
            let init = self.stmt();
            self.consume(
                TokenType::Semicolon,
                "Expected a ';' after for loop init statement",
            );
            let test = self.expression();
            self.consume(
                TokenType::Semicolon,
                "Expected a ';' after for loop test statement",
            );
            let incr = self.stmt();
            self.consume(TokenType::RightParen, "Expected a ')' to end for loop");
            self.consume(
                TokenType::LeftBrace,
                "Expected a '{' to begin for loop body",
            );
            let body = self.stmts();
            self.consume(TokenType::RightBrace, "Expected a '}' after for loop body");
            Stmt::Group(vec![
                init,
                Stmt::While(test, Box::new(Stmt::Group(vec![body, incr]))),
            ])
        } else if self.peek_next().ttype() == TokenType::Eq {
            let str = if let Token::Ident(str) =
            self.consume(TokenType::Ident, "Expected identifier before '='")
            {
                str
            } else {
                panic!("Expected identifier before '='")
            };
            self.consume(TokenType::Eq, "Expected '=' after identifier");
            Stmt::Expr(TypedExpr::new(Expr::ScalarAssign(
                str,
                Box::new(self.expression()),
            )))
            // } else if self.matches(vec![TokenType::Ret]) {
            //     self.return_stmt()
        } else if self.matches(vec![TokenType::While]) {
            self.consume(TokenType::LeftParen, "Must have paren after while");
            let expr = self.expression();
            self.consume(
                TokenType::RightParen,
                "Must have right parent after while statement test expression",
            );
            self.consume(TokenType::LeftBrace, "Must have brace after `while (expr)`");
            let stmts = self.stmts();
            self.consume(TokenType::RightBrace, "While loop must be followed by '}'");
            Stmt::While(expr, Box::new(stmts))
        } else if self.matches(vec![TokenType::Print]) {
            let expr = self.expression();
            Stmt::Print(expr)
        } else if self.matches(vec![TokenType::If]) {
            self.if_stmt()
        } else if self.matches(vec![TokenType::LeftBrace]) {
            let s = self.stmts();
            self.consume(
                TokenType::RightBrace,
                "Expected a right brace after a group",
            );
            s
        } else {
            Stmt::Expr(self.expression())
        };
        stmt
    }

    fn stmts(&mut self) -> Stmt {
        let mut stmts = vec![];
        while self.peek().ttype() != TokenType::RightBrace {
            let stmt = self.stmt_and_optional_semicolon();
            stmts.push(stmt);
        }
        if stmts.len() == 1 {
            return stmts.pop().unwrap();
        }
        Stmt::Group(stmts)
    }

    fn if_stmt(&mut self) -> Stmt {
        self.consume(TokenType::LeftParen, "Expected '(' after if");
        let predicate = self.expression();
        self.consume(TokenType::RightParen, "Expected ')' after if predicate");
        let then_blk = self.group();
        let else_blk = if self.matches(vec![TokenType::Else]) {
            Some(Box::new(self.group()))
        } else {
            None
        };
        Stmt::If(predicate, Box::new(then_blk), else_blk)
    }

    fn expression(&mut self) -> TypedExpr {
        self.assignment()
    }

    fn assignment(&mut self) -> TypedExpr {
        let mut expr = self.ternary();
        if let Expr::Variable(var) = &expr.expr {
            let var = var.clone();
            if self.matches(vec![TokenType::Eq]) {
                // =
                return TypedExpr::new(Expr::ScalarAssign(var, Box::new(self.assignment())));
            } else if self.matches(vec![TokenType::InplaceAssign]) {
                // ?=
                let math_op = if let Token::InplaceEq(math_op) = self.previous().unwrap() { math_op } else { unreachable!() };
                let expr = Expr::MathOp(
                    Box::new(Expr::Variable(var.to_string()).into()),
                    math_op,
                    Box::new(self.assignment()),
                );
                return Expr::ScalarAssign(
                    var.to_string(),
                    Box::new(expr.into())).into();
            }
        }
        let mut is_array_index = false;
        if let Expr::ArrayIndex { .. } = &expr.expr {
            is_array_index = true;
        }
        if is_array_index && self.matches(vec![TokenType::Eq]) {
            if let Expr::ArrayIndex { name, indices } = expr.expr {
                let value = Box::new(self.assignment());
                return Expr::ArrayAssign { name, indices, value }.into();
            } else {
                unreachable!()
            }
        }
        expr
    }

    fn ternary(&mut self) -> TypedExpr {
        let mut cond = self.logical_or();
        while self.matches(vec![TokenType::Question]) {
            let expr1 = self.ternary();
            self.consume(TokenType::Colon, "Expected a colon after question mark in a ternary!");
            let expr2 = self.ternary();
            return TypedExpr::new(Expr::Ternary(
                Box::new(cond),
                Box::new(expr1),
                Box::new(expr2),
            ));
        }
        cond
    }

    fn logical_or(&mut self) -> TypedExpr {
        let mut expr = self.logical_and();
        while self.matches(vec![TokenType::Or]) {
            expr = TypedExpr::new(Expr::LogicalOp(
                Box::new(expr),
                LogicalOp::Or,
                Box::new(self.logical_and()),
            ))
        }
        expr
    }

    fn logical_and(&mut self) -> TypedExpr {
        let mut expr = self.array_membership();
        while self.matches(vec![TokenType::And]) {
            expr = TypedExpr::new(Expr::LogicalOp(
                Box::new(expr),
                LogicalOp::And,
                Box::new(self.array_membership()),
            ))
        }
        expr
    }

    fn array_membership(&mut self) -> TypedExpr {
        // <expr> in array_name
        let mut expr = self.multi_dim_array_membership();
        while self.matches(vec![TokenType::In]) {
            let name = if let Token::Ident(name) = self.consume(TokenType::Ident, "An array name must follow `<expr> in`") { name } else { unreachable!() };
            expr = Expr::InArray { name, indices: vec![expr] }.into()
        }
        expr
    }

    fn helper_multi_dim_array(&mut self) -> TypedExpr {
        self.consume(TokenType::LeftParen, "Multidimensional array must begin with left paren");
        let mut exprs = vec![self.regex()];
        while self.matches(vec![TokenType::Comma]) {
            if self.peek().ttype() == TokenType::RightParen { break; }
            exprs.push(self.regex());
        }
        self.consume(TokenType::RightParen, "Multidimensional array indices must end with right paren");
        self.consume(TokenType::In, "Multidimensional array access must be followed by an 'in'");
        let ident = self.consume(TokenType::Ident, "Multidimensional array access must be followed by an array name. Eg: (1,2,3) in ARRAY_NAME");
        let ident = if let Token::Ident(ident) = ident { ident } else { unreachable!("compiler bug consumed ident but got something else") };

        let mut expr = TypedExpr::new(Expr::InArray { name: ident, indices: exprs });
        while self.matches(vec![TokenType::In]) {
            let ident = self.consume(TokenType::Ident, "Multidimensional array access must be followed by an array name. Eg: (1,2,3) in ARRAY_NAME");
            let ident = if let Token::Ident(ident) = ident { ident } else { unreachable!("compiler bug consumed ident but got something else") };
            expr = Expr::InArray { name: ident, indices: vec![expr.into()] }.into();
        }
        expr
    }

    fn multi_dim_array_membership(&mut self) -> TypedExpr {
        let mut idx = self.current;

        // Check if we match the regex \(.+\) in if so call the helper
        if self.peek_at(idx) == Token::LeftParen {
            while self.peek_at(idx) != Token::RightParen { idx += 1; }
            if self.peek_at(idx) == Token::RightParen && self.peek_at(idx + 1) == Token::In {
                return self.helper_multi_dim_array();
            }
        }
        self.regex()
    }

    fn regex(&mut self) -> TypedExpr {
        // "a ~ /match/"
        let mut expr = self.compare();
        while self.matches(vec![TokenType::MatchedBy, TokenType::NotMatchedBy]) {
            expr = Expr::BinOp(
                Box::new(expr),
                if self.previous().unwrap().ttype() == TokenType::MatchedBy { BinOp::MatchedBy } else { BinOp::NotMatchedBy },
                Box::new(self.compare())).into();
        }
        expr
    }

    fn compare(&mut self) -> TypedExpr {
        let mut expr = self.string_concat();
        while self.matches(vec![
            TokenType::GreaterEq,
            TokenType::Greater,
            TokenType::Less,
            TokenType::LessEq,
            TokenType::EqEq,
            TokenType::BangEq,
        ]) {
            let op = match self.previous().unwrap() {
                Token::BinOp(BinOp::Less) => BinOp::Less,
                Token::BinOp(BinOp::LessEq) => BinOp::LessEq,
                Token::BinOp(BinOp::Greater) => BinOp::Greater,
                Token::BinOp(BinOp::GreaterEq) => BinOp::GreaterEq,
                Token::BinOp(BinOp::BangEq) => BinOp::BangEq,
                Token::BinOp(BinOp::EqEq) => BinOp::EqEq,
                _ => unreachable!("Parser bug in compare matches function"),
            };
            expr = Expr::BinOp(Box::new(expr), op, Box::new(self.string_concat())).into()
        }
        expr
    }

    fn string_concat(&mut self) -> TypedExpr {
        let mut expr = self.plus_minus();
        let not_these = vec![
            TokenType::InplaceAssign,
            TokenType::Less,
            TokenType::LessEq,
            TokenType::BangEq,
            TokenType::EqEq,
            TokenType::Greater,
            TokenType::GreaterEq,
            TokenType::And,
            TokenType::Or,
            TokenType::Eq,
            TokenType::Semicolon,
            TokenType::RightBrace,
            TokenType::RightParen,
            TokenType::LeftBrace,
            TokenType::Question,
            TokenType::Colon,
            TokenType::MatchedBy,
            TokenType::NotMatchedBy,
            TokenType::Comma,
            TokenType::In,
            TokenType::LeftBracket,
            TokenType::RightBracket,
        ];
        while !self.is_at_end() && !not_these.contains(&self.peek().ttype()) {
            if let Expr::Concatenation(vals) = &mut expr.expr {
                vals.push(self.plus_minus());
            } else {
                expr = TypedExpr::new(Expr::Concatenation(vec![expr, self.plus_minus()]));
            }
        }
        expr
    }

    fn plus_minus(&mut self) -> TypedExpr {
        let mut expr = self.term();
        while self.matches(vec![TokenType::Plus, TokenType::Minus]) {
            let op = match self.previous().unwrap() {
                Token::MathOp(MathOp::Minus) => MathOp::Minus,
                Token::MathOp(MathOp::Plus) => MathOp::Plus,
                _ => unreachable!("Parser bug in comparison function"),
            };
            expr = Expr::MathOp(Box::new(expr), op, Box::new(self.plus_minus())).into();
        }
        expr
    }

    fn term(&mut self) -> TypedExpr {
        let mut expr = self.unary();
        while self.matches(vec![TokenType::Star, TokenType::Slash, TokenType::Modulo]) {
            let op = match self.previous().unwrap() {
                Token::MathOp(MathOp::Star) => MathOp::Star,
                Token::MathOp(MathOp::Slash) => MathOp::Slash,
                Token::MathOp(MathOp::Modulus) => MathOp::Modulus,
                _ => unreachable!("Parser bug in comparison function"),
            };
            expr = Expr::MathOp(Box::new(expr), op, Box::new(self.unary())).into()
        }
        expr
    }

    fn unary(&mut self) -> TypedExpr {
        if !(self.peek().ttype() == TokenType::Minus
            && self.peek_next().ttype() == TokenType::Minus)
            && !(self.peek().ttype() == TokenType::Plus
            && self.peek_next().ttype() == TokenType::Plus)
            && self.matches(vec![TokenType::Minus, TokenType::Plus, TokenType::Bang])
        {
            let p = self.previous().unwrap().ttype();
            let rhs = self.unary();
            let one = TypedExpr::new(Expr::NumberF64(1.0));
            let zero = TypedExpr::new(Expr::NumberF64(0.0));
            return match p {
                TokenType::Bang => Expr::BinOp(Box::new(one), BinOp::BangEq, Box::new(rhs)),
                TokenType::Plus => Expr::MathOp(Box::new(zero), MathOp::Plus, Box::new(rhs)),
                TokenType::Minus => Expr::MathOp(Box::new(zero), MathOp::Minus, Box::new(rhs)),
                _ => unreachable!(),
            }
                .into();
        }
        self.exp()
    }

    fn exp(&mut self) -> TypedExpr {
        let mut expr = self.pre_op();
        while self.matches(vec![TokenType::Exponent]) {
            let op = MathOp::Exponent;
            expr = Expr::MathOp(Box::new(expr), op, Box::new(self.pre_op())).into()
        }
        expr
    }

    fn pre_op(&mut self) -> TypedExpr {
        if self.peek().ttype() == TokenType::Plus
            && self.peek_next().ttype() == TokenType::Plus
            && self.peek_next_next().ttype() == TokenType::Ident
        {
            self.advance();
            self.advance();
            self.advance();

            if let Token::Ident(name) = self.previous().unwrap() {
                let varExpr = Expr::Variable(name.clone()).into();
                let increment = Expr::MathOp(
                    Box::new(varExpr),
                    MathOp::Plus,
                    Box::new(Expr::NumberF64(1.0).into()),
                )
                    .into();

                return Expr::ScalarAssign(name, Box::new(increment)).into();
            }
        } else if self.peek().ttype() == TokenType::Minus
            && self.peek_next().ttype() == TokenType::Minus
            && self.peek_next_next().ttype() == TokenType::Ident
        {
            self.advance();
            self.advance();
            self.advance();

            if let Token::Ident(name) = self.previous().unwrap() {
                let var = Expr::Variable(name.clone()).into();
                let decrement = Expr::MathOp(
                    Box::new(var),
                    MathOp::Minus,
                    Box::new(Expr::NumberF64(1.0).into()),
                )
                    .into();

                return Expr::ScalarAssign(name, Box::new(decrement)).into();
            }
        }

        return self.post_op();
    }

    fn post_op(&mut self) -> TypedExpr {
        let mut expr = self.column();

        if let Expr::Variable(name) = expr.expr.clone() {
            if self.peek().ttype() == TokenType::Plus && self.peek_next().ttype() == TokenType::Plus
            {
                self.advance();
                self.advance();
                let increment = Expr::MathOp(
                    Box::new(expr),
                    MathOp::Plus,
                    Box::new(Expr::NumberF64(1.0).into()),
                )
                    .into();
                let assign = Expr::ScalarAssign(name, Box::new(increment)).into();
                expr = Expr::MathOp(
                    Box::new(assign),
                    MathOp::Minus,
                    Box::new(Expr::NumberF64(1.0).into()),
                )
                    .into();
            } else if self.peek().ttype() == TokenType::Minus
                && self.peek_next().ttype() == TokenType::Minus
            {
                self.advance();
                self.advance();
                let decrement = Expr::MathOp(
                    Box::new(expr),
                    MathOp::Minus,
                    Box::new(Expr::NumberF64(1.0).into()),
                )
                    .into();
                let assign = Expr::ScalarAssign(name, Box::new(decrement)).into();
                expr = Expr::MathOp(
                    Box::new(assign),
                    MathOp::Plus,
                    Box::new(Expr::NumberF64(1.0).into()),
                )
                    .into();
            }
        }
        expr
    }

    fn column(&mut self) -> TypedExpr {
        let mut num_cols: usize = 0;
        while self.matches(vec![TokenType::Column]) {
            num_cols += 1;
        }
        let mut expr = self.primary();
        for _ in 0..num_cols {
            // If this isn't a col we loop 0 times and just return primary
            expr = TypedExpr::new(Expr::Column(Box::new(expr)));
        }

        expr
    }

    fn primary(&mut self) -> TypedExpr {
        if self.is_at_end() {
            panic!("Unexpected end of input")
        }
        match self.tokens.get(self.current).unwrap().clone() {
            Token::NumberF64(num) => {
                self.advance();
                Expr::NumberF64(num).into()
            }
            Token::LeftParen => {
                self.consume(TokenType::LeftParen, "Expected to parse a left paren here");
                let expr = self.expression();
                self.consume(TokenType::RightParen, "Missing closing ')' after group");
                expr.into()
            }
            Token::Ident(name) => {
                self.consume(TokenType::Ident, "Expected to parse an ident here");
                if self.matches(vec![TokenType::LeftBracket]) {
                    self.array_index(name)
                } else {
                    Expr::Variable(name).into()
                }
            }
            Token::String(string) => {
                self.consume(TokenType::String, "Expected to parse a string here");
                Expr::String(string).into()
            }
            Token::Regex(string) => {
                self.consume(TokenType::Regex, "Expected to parse a string here");
                Expr::Regex(string).into()
            }
            t => panic!("Unexpected token {:?} {}", t, TokenType::name(t.ttype())),
        }
    }

    fn array_index(&mut self, name: String) -> TypedExpr {
        let mut indices = vec![self.expression()];
        while self.matches(vec![TokenType::Comma]) && self.peek().ttype() != TokenType::RightBracket {
            indices.push(self.expression());
        }
        self.consume(TokenType::RightBracket, "Array indexing must end with a right bracket.");
        Expr::ArrayIndex { name, indices }.into()
    }
}

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
        Program::new(vec![], vec![], vec![PatternAction::new_action_only($body)])
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

    assert_eq!(
        parse(lex("{1 + 2;}").unwrap()),
        Program::new(
            vec![],
            vec![],
            vec![PatternAction::new_action_only(Stmt::Expr(mathop!(
                bnum!(1.0),
                MathOp::Plus,
                bnum!(2.0)
            )))],
        )
    );
}

#[test]
fn test_ast_oop() {
    use crate::lexer::lex;
    let left = bnum!(1.0);
    let right = Box::new(mathop!(bnum!(3.0), MathOp::Star, bnum!(2.0)));
    let mult = Stmt::Expr(mathop!(left, MathOp::Plus, right));
    assert_eq!(
        parse(lex("{1 + 3 * 2;}").unwrap()),
        Program::new_action_only(mult)
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
            )))],
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
    println!(
        "Expected {}\nActual {}",
        expected.pattern_actions[0].action, actual.pattern_actions[0].action
    );
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
    assert_eq!(actual, Program::new(begins, ends, vec![generic]));
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
            )))],
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
            vec![PatternAction::new_action_only(Stmt::Print(num!(1.0)))],
        )
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
    assert_eq!(actual, Program::new(vec![], vec![], vec![pa]));
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
    assert_eq!(actual, Program::new(vec![], vec![], vec![pa]));
}

#[test]
fn test_while_l00p() {
    use crate::lexer::lex;
    let str = "{ while (123) { print 1; } }";
    let actual = parse(lex(str).unwrap());
    let body = Stmt::While(num!(123.0), Box::new(Stmt::Print(num!(1.0))));
    assert_eq!(
        actual,
        Program::new(vec![], vec![], vec![PatternAction::new_action_only(body)])
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
    println!("{}", actual.pattern_actions[0].action);
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
fn array_assign_multi_expr() {
    actual!(actual, "{ a[0+1, a[0]] }");
    let zero = bnum!(0.0);
    let one = bnum!(1.0);
    let op = Expr::MathOp(zero, MathOp::Plus, one).into();
    let a_zero = Expr::ArrayIndex {name: "a".to_string(), indices: vec![num!(0.0)]}.into();
    let expr = texpr!(Expr::ArrayIndex{name: "a".to_string(),indices: vec![op, a_zero]});
    let stmt = Stmt::Expr(expr);
    assert_eq!(actual, sprogram!(stmt));
}
