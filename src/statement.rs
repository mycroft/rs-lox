use std::rc::Rc;

use crate::expr::Expr;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    If(Expr, Rc<Stmt>, Rc<Stmt>),
    Expr(Expr),
    Print(Expr),
    Var(String, Expr),
    Block(Vec<Stmt>),
    While(Expr, Rc<Stmt>),
    Function(String, Vec<Token>, Rc<Stmt>),
    Return(Token, Expr),
    Class(Token, Expr, Vec<Stmt>),
    Nil,
}

impl Stmt {
    pub fn var(name: String, expr: Expr) -> Stmt {
        Stmt::Var(name, expr)
    }

    pub fn function(name: String, args: Vec<Token>, body: Stmt) -> Stmt {
        Stmt::Function(name, args, Rc::new(body))
    }
}
