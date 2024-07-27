use phf::phf_map;

use crate::expr::Literal;

use super::token::{Token, TokenType};

#[derive(Debug)]
pub struct Scanner {
    pub tokens: Vec<Token>,
    source: String,
    start: usize,
    current: usize,
    line: usize,
}

impl Scanner {
    const KEYWORDS: phf::Map<&'static str, TokenType> = phf_map! {
        "and" => TokenType::And,
        "class" => TokenType::Class,
        "else" => TokenType::Else,
        "false" => TokenType::False,
        "for" => TokenType::For,
        "fun" => TokenType::Fun,
        "if" => TokenType::If,
        "nil" => TokenType::Nil,
        "or" => TokenType::Or,
        "print" => TokenType::Print,
        "return" => TokenType::Return,
        "super" => TokenType::Super,
        "this" => TokenType::This,
        "true" => TokenType::True,
        "var" => TokenType::Var,
        "while" => TokenType::While,
    };

    pub fn new(input: &str) -> Self {
        Scanner {
            tokens: Vec::new(),
            source: String::from(input),

            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self) -> Result<(), String> {
        while !self.is_at_end() {
            self.start = self.current;
            let res = self.scan_token();
            match res {
                Ok(_) => {}
                Err(x) => {
                    return Err(x);
                }
            }
        }

        self.tokens.push(Token::new(
            TokenType::Eof,
            String::from(""),
            Literal::Nil,
            self.line,
            self.current,
        ));

        Ok(())
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn scan_token(&mut self) -> Result<(), String> {
        let c = self.advance();

        match c {
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            '-' => self.add_token(TokenType::Minus),
            '+' => self.add_token(TokenType::Plus),
            ';' => self.add_token(TokenType::Semicolon),
            '*' => self.add_token(TokenType::Star),
            '!' => {
                let token_type = if self.next_match('=') {
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                };
                self.add_token(token_type);
            }
            '=' => {
                let token_type = if self.next_match('=') {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                };
                self.add_token(token_type);
            }
            '<' => {
                let token_type = if self.next_match('=') {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                };
                self.add_token(token_type);
            }
            '>' => {
                let token_type = if self.next_match('=') {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                };
                self.add_token(token_type);
            }
            '/' => {
                if self.next_match('/') {
                    while !self.is_at_end() && self.peek().is_some_and(|c| c == '\n') {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenType::Slash);
                }
            }
            ' ' | '\r' | '\t' => {}
            '\n' => self.line += 1,
            '"' => return self.parse_string(),
            '0'..='9' => return self.parse_number(),
            'a'..='z' | 'A'..='Z' => self.parse_identifier(),
            _ => {
                return Err(format!(
                    "scanner::scan_token: could not find valid token type of '{}'",
                    c
                ));
            }
        }

        Ok(())
    }

    fn advance(&mut self) -> char {
        let c = self.source.chars().nth(self.current).unwrap();
        self.current += 1;

        c
    }

    fn add_token(&mut self, token_type: TokenType) {
        let s: String = String::from(&self.source[self.start..self.current]);
        self.tokens.push(Token::new(
            token_type,
            s,
            Literal::Nil,
            self.line,
            self.current,
        ));
    }

    fn add_token_literal(&mut self, token_type: TokenType, value: Literal) {
        let s: String = String::from(&self.source[self.start..self.current]);
        self.tokens
            .push(Token::new(token_type, s, value, self.line, self.current));
    }

    fn next_match(&mut self, c: char) -> bool {
        if self.is_at_end() || self.source.chars().nth(self.current).unwrap() != c {
            false
        } else {
            self.advance();
            true
        }
    }

    fn peek(&mut self) -> Option<char> {
        if self.is_at_end() {
            None
        } else {
            Some(self.source.chars().nth(self.current).unwrap())
        }
    }

    fn peek_next(&mut self) -> Option<char> {
        let idx = self.current + 1;

        if idx >= self.source.len() {
            None
        } else {
            Some(self.source.chars().nth(idx).unwrap())
        }
    }

    fn parse_string(&mut self) -> Result<(), String> {
        while self.peek().is_some_and(|c| c != '"') && !self.is_at_end() {
            if self.peek().is_some_and(|c| c == '\n') {
                self.line += 1;
            }

            self.advance();
        }

        if self.is_at_end() {
            return Err("unterminated string.".into());
        }

        self.advance();

        let res = String::from(&self.source[self.start + 1..self.current - 1]);
        self.add_token_literal(TokenType::String, Literal::String(res));

        Ok(())
    }

    fn parse_number(&mut self) -> Result<(), String> {
        while self.peek().is_some_and(|c| c.is_ascii_digit()) {
            self.advance();
        }

        // Look for a fractional part.
        if self.peek().is_some_and(|c| c == '.')
            && self.peek_next().is_some_and(|c| c.is_ascii_digit())
        {
            self.advance();

            while self.peek().is_some_and(|c| c.is_ascii_digit()) {
                self.advance();
            }
        }

        match self.source[self.start..self.current].parse::<f64>() {
            Ok(x) => self.add_token_literal(TokenType::Number, Literal::Number(x)),
            Err(x) => return Err(x.to_string()),
        };

        Ok(())
    }

    fn parse_identifier(&mut self) {
        while self.peek().is_some_and(Self::is_alphanumeric) {
            self.advance();
        }

        let text = String::from(&self.source[self.start..self.current]);

        let token_type = Scanner::KEYWORDS.get(&text);
        self.add_token(match token_type {
            Some(token_type) => token_type.clone(),
            None => TokenType::Identifier,
        });
    }

    fn is_digit(c: char) -> bool {
        c.is_ascii_digit()
    }

    fn is_alpha(c: char) -> bool {
        c.is_ascii_alphabetic() || c == '_'
    }

    fn is_alphanumeric(c: char) -> bool {
        Self::is_alpha(c) || Self::is_digit(c)
    }
}

#[test]
fn test_scanner() {
    let input = "(){}==!== = = <= >= ==
    \"first string\" \"second\"
    \"third
string\"
    4
    12345.67890
    no_scrub
    super
    bla
    !true
    ";

    let mut scanner = Scanner::new(input);
    let res = scanner.scan_tokens();
    assert!(res.is_ok());

    let mut expected_tokens: Vec<TokenType> = Vec::new();
    expected_tokens.push(TokenType::LeftParen);
    expected_tokens.push(TokenType::RightParen);
    expected_tokens.push(TokenType::LeftBrace);
    expected_tokens.push(TokenType::RightBrace);
    expected_tokens.push(TokenType::EqualEqual);
    expected_tokens.push(TokenType::BangEqual);
    expected_tokens.push(TokenType::Equal);
    expected_tokens.push(TokenType::Equal);
    expected_tokens.push(TokenType::Equal);
    expected_tokens.push(TokenType::LessEqual);
    expected_tokens.push(TokenType::GreaterEqual);
    expected_tokens.push(TokenType::EqualEqual);
    expected_tokens.push(TokenType::String);
    expected_tokens.push(TokenType::String);
    expected_tokens.push(TokenType::String);
    expected_tokens.push(TokenType::Number);
    expected_tokens.push(TokenType::Number);
    expected_tokens.push(TokenType::Identifier);
    expected_tokens.push(TokenType::Super);
    expected_tokens.push(TokenType::Identifier);
    expected_tokens.push(TokenType::Bang);
    expected_tokens.push(TokenType::True);
    expected_tokens.push(TokenType::Eof);

    println!("{:?}", scanner.tokens);

    assert_eq!(expected_tokens.len(), scanner.tokens.len());

    for (idx, token_type) in expected_tokens.iter().cloned().enumerate() {
        assert_eq!(token_type, scanner.tokens[idx].token_type);
    }
}
