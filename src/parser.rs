use std::fmt::Display;
use std::rc::Rc;

use crate::token::TokenType;

use super::expr::{Expr, Literal};
use super::statement::Stmt;
use super::token::Token;

#[cfg(test)]
use super::scanner::Scanner;

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    pub parsed: Expr,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens: tokens.clone(),
            current: 0,
            parsed: Expr::nil(),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, String> {
        let mut statements: Vec<Stmt> = Vec::new();

        while !self.is_at_end() {
            let declaration_res = self.declaration()?;
            statements.push(declaration_res);
        }

        Ok(statements)
    }

    fn token_match(&mut self, token_types: Vec<TokenType>) -> bool {
        for token_type in token_types {
            if self.check(token_type) {
                self.advance();
                return true;
            }
        }

        false
    }

    fn check(&self, token_type: TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }

        self.peek().token_type == token_type
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::Eof
    }

    fn peek(&self) -> Token {
        self.tokens[self.current].clone()
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }

        self.previous()
    }

    fn previous(&self) -> Token {
        self.tokens[self.current - 1].clone()
    }

    pub fn expression(&mut self) -> Result<Expr, String> {
        self.assignement()
    }

    fn assignement(&mut self) -> Result<Expr, String> {
        let expr = self.or()?;

        if self.token_match([TokenType::Equal].into()) {
            let _ = self.previous();
            let value = self.assignement()?;

            let ret = match expr {
                Expr::Variable { ref name, .. } => Expr::Assign {
                    name: name.clone(),
                    value: Box::new(value),
                },
                Expr::Get { expr, name } => Expr::Set {
                    expr,
                    name,
                    value: Box::new(value),
                },
                _ => return Err("Invalid assignement target.".into()),
            };

            Ok(ret)
        } else {
            Ok(expr)
        }
    }

    fn equality(&mut self) -> Result<Expr, String> {
        let mut expr = self.comparison();

        while self.token_match([TokenType::BangEqual, TokenType::EqualEqual].into()) {
            let operator = self.previous();
            let right = self.comparison();

            expr = Ok(Expr::binary(expr.unwrap(), operator, right.unwrap()));
        }

        expr
    }

    fn primary(&mut self) -> Result<Expr, String> {
        if self.token_match([TokenType::False].into()) {
            return Ok(Expr::literal(Literal::Boolean(false)));
        }
        if self.token_match([TokenType::True].into()) {
            return Ok(Expr::literal(Literal::Boolean(true)));
        }
        if self.token_match([TokenType::Nil].into()) {
            return Ok(Expr::literal(Literal::Nil));
        }
        if self.token_match([TokenType::Number, TokenType::String].into()) {
            return Ok(Expr::literal(self.previous().literal()));
        }
        if self.token_match([TokenType::LeftParen].into()) {
            let res_expr = self.expression()?;

            let res = self.consume(TokenType::RightParen, "Expect ')' after expression.".into());
            if res.is_err() {
                return Err(res.err().unwrap());
            }

            return Ok(Expr::grouping(res_expr));
        }
        if self.token_match([TokenType::Super].into()) {
            let keyword = self.previous();
            self.consume(TokenType::Dot, "Expect '.' after 'super'".into())?;
            let method = self.consume(
                TokenType::Identifier,
                "Expect superclass method name.".into(),
            )?;

            return Ok(Expr::super_expr(keyword, method));
        }

        if self.token_match([TokenType::This].into()) {
            return Ok(Expr::this(self.previous()));
        }

        if self.token_match([TokenType::Identifier].into()) {
            return Ok(Expr::variable(self.previous()));
        }

        Err(format!(
            "unhandled current token: {:?}",
            self.tokens[self.current]
        ))
    }

    fn consume(&mut self, token_type: TokenType, err: String) -> Result<Token, String> {
        if self.check(token_type) {
            return Ok(self.advance());
        }
        Err(err)
    }

    fn unary(&mut self) -> Result<Expr, String> {
        if self.token_match([TokenType::Bang, TokenType::Minus].into()) {
            let operator = self.previous();
            let res = self.unary()?;

            Ok(Expr::unary(operator, res))
        } else {
            self.call()
        }
    }

    fn call(&mut self) -> Result<Expr, String> {
        let mut expr = self.primary()?;

        loop {
            if self.token_match([TokenType::LeftParen].into()) {
                expr = self.finish_call(expr)?;
            } else if self.token_match([TokenType::Dot].into()) {
                let token = self.consume(
                    TokenType::Identifier,
                    "Expect property name after '.'.".into(),
                )?;
                expr = Expr::Get {
                    expr: Box::new(expr),
                    name: token.lexeme(),
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, String> {
        let mut args = Vec::new();

        if !self.check(TokenType::RightParen) {
            loop {
                args.push(self.expression()?);

                if args.len() > 16 {
                    return Err("Can't have more than 16 arguments.".into());
                }

                if !self.token_match([TokenType::Comma].into()) {
                    break;
                }
            }
        }

        let paren = self.consume(TokenType::RightParen, "Expect ')' after arguments.".into())?;

        Ok(Expr::call(callee, paren, args))
    }

    fn factor(&mut self) -> Result<Expr, String> {
        let mut expr = self.unary()?;

        while self.token_match([TokenType::Slash, TokenType::Star].into()) {
            let operator = self.previous();
            let res_unary = self.unary()?;
            expr = Expr::binary(expr, operator, res_unary);
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, String> {
        let mut expr = self.factor()?;

        while self.token_match([TokenType::Minus, TokenType::Plus].into()) {
            let operator = self.previous();
            let res_right = self.factor()?;

            expr = Expr::binary(expr, operator, res_right);
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, String> {
        let mut expr = self.term()?;

        while self.token_match(
            [
                TokenType::Greater,
                TokenType::GreaterEqual,
                TokenType::Less,
                TokenType::LessEqual,
            ]
            .into(),
        ) {
            let operator = self.previous();
            let res_right = self.term()?;

            expr = Expr::binary(expr, operator, res_right);
        }

        Ok(expr)
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().token_type == TokenType::Semicolon {
                return;
            }

            match self.peek().token_type {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return,
                _ => {}
            }

            self.advance();
        }
    }

    fn statement(&mut self) -> Result<Stmt, String> {
        if self.token_match([TokenType::Print].into()) {
            self.print_statement()
        } else if self.token_match([TokenType::Return].into()) {
            self.return_statement()
        } else if self.token_match([TokenType::LeftBrace].into()) {
            self.block()
        } else if self.token_match([TokenType::If].into()) {
            self.if_statement()
        } else if self.token_match([TokenType::While].into()) {
            self.while_statement()
        } else if self.token_match([TokenType::For].into()) {
            self.for_statement()
        } else {
            self.expr_statement()
        }
    }

    fn print_statement(&mut self) -> Result<Stmt, String> {
        let value = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after value".into())?;
        Ok(Stmt::Print(value))
    }

    fn expr_statement(&mut self) -> Result<Stmt, String> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after expression.".into())?;
        Ok(Stmt::Expr(expr))
    }

    fn declaration(&mut self) -> Result<Stmt, String> {
        let res: Result<Stmt, String> = if self.token_match([TokenType::Class].into()) {
            self.class_declaration("class".into())
        } else if self.token_match([TokenType::Fun].into()) {
            self.function("function".into())
        } else if self.token_match([TokenType::Var].into()) {
            self.var_declaration()
        } else {
            self.statement()
        };

        if res.is_err() {
            self.synchronize();
            // return Ok(Stmt::Nil);
        }

        res
    }

    fn function(&mut self, kind: String) -> Result<Stmt, String> {
        let name = self.consume(TokenType::Identifier, format!("Expect {} name.", kind))?;

        self.consume(
            TokenType::LeftParen,
            format!("Expect '(' after {} name.", kind),
        )?;

        let mut parameters = Vec::new();

        if !self.check(TokenType::RightParen) {
            loop {
                if parameters.len() >= 255 {
                    return Err(String::from("Can't have more than 255 parameters."));
                }

                parameters.push(self.consume(
                    TokenType::Identifier,
                    String::from("Expect parameter name."),
                )?);

                if !self.token_match([TokenType::Comma].into()) {
                    break;
                }
            }
        }

        self.consume(
            TokenType::RightParen,
            String::from("Expect ')' after parameters."),
        )?;
        self.consume(
            TokenType::LeftBrace,
            format!("Expect '{{' before {} body.", kind),
        )?;

        let body = self.block()?;

        Ok(Stmt::function(name.lexeme(), parameters, body))
    }

    fn return_statement(&mut self) -> Result<Stmt, String> {
        let keyword = self.previous();

        let value = if !self.check(TokenType::Semicolon) {
            self.expression()?
        } else {
            Expr::Nil
        };

        self.consume(
            TokenType::Semicolon,
            "Expect ';' after return value.".into(),
        )?;

        Ok(Stmt::Return(keyword, value))
    }

    fn class_declaration(&mut self, kind: String) -> Result<Stmt, String> {
        let name = self.consume(TokenType::Identifier, format!("Expect {} name.", kind))?;

        let mut superclass = Expr::nil();

        if self.token_match([TokenType::Less].into()) {
            self.consume(TokenType::Identifier, "Expect superclass name.".into())?;
            superclass = Expr::Variable {
                name: self.previous(),
            };
        }

        self.consume(
            TokenType::LeftBrace,
            String::from("Expect '{' before class body."),
        )?;

        let mut methods: Vec<Stmt> = Vec::new();

        loop {
            if self.check(TokenType::RightBrace) || self.is_at_end() {
                break;
            }
            let method: Stmt = self.function("method".into())?;
            methods.push(method);
        }

        self.consume(
            TokenType::RightBrace,
            String::from("Expect '}' after class body."),
        )?;

        Ok(Stmt::Class(name, superclass, methods))
    }

    // TODO: To refactor
    fn var_declaration(&mut self) -> Result<Stmt, String> {
        let name = self.consume(TokenType::Identifier, "Expect variable name.".into());
        let mut initializer = Expr::Nil;

        if self.token_match([TokenType::Equal].into()) {
            let res = self.expression();
            if res.is_ok() {
                initializer = res.unwrap();
            } else {
                return Err(res.err().unwrap());
            }
        }

        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration.".into(),
        )?;

        Ok(Stmt::var(name.unwrap().lexeme(), initializer))
    }

    fn block(&mut self) -> Result<Stmt, String> {
        let mut statements = Vec::new();

        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            let statement = self.declaration()?;
            statements.push(statement);
        }

        self.consume(TokenType::RightBrace, "Expect '}' after block.".into())?;

        Ok(Stmt::Block(statements))
    }

    fn if_statement(&mut self) -> Result<Stmt, String> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.".into())?;

        let condition = self.expression()?;
        self.consume(
            TokenType::RightParen,
            "Expect ')' after if condition.".into(),
        )?;

        let then_branch = self.statement()?;
        let mut else_branch = Stmt::Nil;

        if self.token_match([TokenType::Else].into()) {
            else_branch = self.statement()?;
        }

        Ok(Stmt::If(
            condition,
            Rc::new(then_branch),
            Rc::new(else_branch),
        ))
    }

    fn or(&mut self) -> Result<Expr, String> {
        let mut expr = self.and()?;

        while self.token_match([TokenType::Or].into()) {
            let operator = self.previous();
            let right = self.and()?;

            expr = Expr::logical(expr, operator, right);
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<Expr, String> {
        let mut expr = self.equality()?;

        while self.token_match([TokenType::And].into()) {
            let operator = self.previous();
            let right = self.equality()?;

            expr = Expr::logical(expr, operator, right);
        }

        Ok(expr)
    }

    fn while_statement(&mut self) -> Result<Stmt, String> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.".into())?;
        let expr = self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after condition.".into())?;
        let body = self.statement()?;

        Ok(Stmt::While(expr, Rc::new(body)))
    }

    fn for_statement(&mut self) -> Result<Stmt, String> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.".into())?;

        let initializer = if self.token_match([TokenType::Semicolon].into()) {
            Stmt::Nil
        } else if self.token_match([TokenType::Var].into()) {
            self.var_declaration()?
        } else {
            self.expr_statement()?
        };

        let condition = if !self.token_match([TokenType::Semicolon].into()) {
            self.expression()?
        } else {
            // No condition means always true
            Expr::Literal {
                value: Literal::Boolean(true),
            }
        };

        self.consume(
            TokenType::Semicolon,
            "Expect ';' after loop condition.".into(),
        )?;

        let increment = if !self.token_match([TokenType::RightParen].into()) {
            self.expression()?
        } else {
            Expr::Nil
        };

        self.consume(
            TokenType::RightParen,
            "Expect ')' after for clauses.".into(),
        )?;

        let mut body = self.statement()?;

        if increment != Expr::Nil {
            // if there is an increment to do, we're doing it after running the body block
            body = Stmt::Block([body, Stmt::Expr(increment)].into());
        }

        body = Stmt::While(condition, Rc::new(body));

        if initializer != Stmt::Nil {
            body = Stmt::Block([initializer, body].into())
        }

        Ok(body)
    }
}

impl Display for Parser {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Parsed: {:?}", self.parsed)
    }
}

#[test]
fn test_expression() {
    struct Test<'a> {
        input: &'a str,
        expected: Expr,
    }

    let tests: Vec<Test> = [
        Test {
            input: "1+1",
            expected: Expr::binary(
                Expr::literal(Literal::Number(1.)),
                Token::new(TokenType::Plus, "+".into(), Literal::Nil, 1, 0),
                Expr::literal(Literal::Number(1.)),
            ),
        },
        Test {
            input: "(true!=false)",
            expected: Expr::grouping(Expr::binary(
                Expr::literal(Literal::Boolean(true)),
                Token::new(TokenType::BangEqual, "!=".into(), Literal::Nil, 1, 0),
                Expr::literal(Literal::Boolean(false)),
            )),
        },
    ]
    .into();

    for test in tests {
        let mut scanner = Scanner::new(&test.input);
        let res = scanner.scan_tokens();
        assert!(res.is_ok());

        let mut parser = Parser::new(scanner.tokens);
        let res = parser.expression();
        assert!(res.is_ok());

        assert_eq!(test.expected, res.unwrap());
    }
}
