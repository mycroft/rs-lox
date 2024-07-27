use std::fmt::{self, Display};

use crate::callable::Callable;
use crate::class::LoxInstance;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Boolean(bool),
    Number(f64),
    String(String),
    Callable(Callable),
    Instance(LoxInstance),
    Nil,
}

impl Literal {
    pub fn as_num(&self) -> Option<f64> {
        match self {
            Literal::Number(n) => Some(*n),
            _ => None,
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Literal::Boolean(v) => Some(*v),
            Literal::Nil => Some(false),
            Literal::String(x) => Some(!x.is_empty()),
            Literal::Number(x) => Some(*x != 0.),
            _ => unreachable!(),
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Nil => write!(f, "nil"),
            Literal::Boolean(b) => {
                if *b {
                    write!(f, "true")
                } else {
                    write!(f, "false")
                }
            }
            Literal::Number(x) => write!(f, "{}", x),
            Literal::String(s) => write!(f, "{}", s),
            Literal::Callable(callable) => match callable {
                Callable::Klass(klass) => write!(f, "klass {}", klass.name()),
                _ => todo!(),
            },
            Literal::Instance(instance) => write!(f, "{} instance", instance.klass_name()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Literal {
        value: Literal,
    },
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
    Grouping {
        expr: Box<Expr>,
    },
    Variable {
        name: Token,
    },
    Assign {
        name: Token,
        value: Box<Expr>,
    },
    Logical {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        paren: Token,
        args: Vec<Expr>,
    },
    Get {
        expr: Box<Expr>,
        name: String,
    },
    Set {
        expr: Box<Expr>,
        name: String,
        value: Box<Expr>,
    },
    This {
        keyword: Token,
    },
    Super {
        keyword: Token,
        method: Token,
    },
    Nil,
}

impl Expr {
    pub fn binary(left: Expr, operator: Token, right: Expr) -> Self {
        Expr::Binary {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }

    pub fn literal(value: Literal) -> Self {
        Expr::Literal { value }
    }

    pub fn unary(operator: Token, right: Expr) -> Self {
        Expr::Unary {
            operator,
            right: Box::new(right),
        }
    }

    pub fn nil() -> Expr {
        Expr::Nil
    }

    pub fn grouping(expr: Expr) -> Self {
        Expr::Grouping {
            expr: Box::new(expr),
        }
    }

    pub fn variable(name: Token) -> Self {
        Expr::Variable { name }
    }

    pub fn logical(left: Expr, operator: Token, right: Expr) -> Self {
        Expr::Logical {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }

    pub fn call(callee: Expr, paren: Token, args: Vec<Expr>) -> Self {
        Expr::Call {
            callee: Box::new(callee),
            paren,
            args,
        }
    }

    pub fn this(token: Token) -> Self {
        Expr::This { keyword: token }
    }

    pub fn super_expr(keyword: Token, method: Token) -> Self {
        Expr::Super { keyword, method }
    }
}
