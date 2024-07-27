use std::fmt::{self, Display};

use crate::expr::Literal;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals,
    Identifier,
    String,
    Number,

    // Keywords,
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
}

impl TokenType {
    // XXX TODO
    fn to_string(&self) -> &'static str {
        "undefined"
    }

    fn name(&self) -> &'static str {
        self.to_string()
    }
}

impl Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    lexeme: String,
    literal: Literal,
    line: usize,
    index: usize,
}

impl Token {
    pub fn new(
        token_type: TokenType,
        lexeme: String,
        literal: Literal,
        line: usize,
        index: usize,
    ) -> Self {
        Token {
            token_type,
            lexeme,
            literal,
            line,
            index,
        }
    }

    pub fn literal(&self) -> Literal {
        self.literal.clone()
    }

    pub fn lexeme(&self) -> String {
        self.lexeme.clone()
    }

    pub fn index(&self) -> usize {
        self.index
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Token{{token_type:{}, lexeme:{} literal:{} line:{}}}",
            self.token_type, self.lexeme, self.literal, self.lexeme
        )
    }
}
