use std::cell::RefCell;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::class::{LoxClass, LoxInstance};
use crate::environment::Environment;
use crate::error::{Result, RuntimeError};
use crate::expr::Literal;
use crate::interpreter::Interpreter;
use crate::statement::Stmt;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Callable {
    Function(LoxFunction),
    Klass(LoxClass),
    Clock,
    Random,
}

impl Callable {
    pub fn call(
        &self,
        interpreter: &mut Interpreter,
        args_values: Vec<Literal>,
    ) -> Result<Literal> {
        match self {
            Callable::Function(fun) => fun.call(interpreter, args_values),
            Callable::Klass(class) => class.call(interpreter, args_values),
            Callable::Clock => {
                // TODO: to fix unwrap and return an error message
                let s = SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .unwrap()
                    .as_secs() as f64;
                Ok(Literal::Number(s))
            }
            Callable::Random => Ok(Literal::Number(4.)),
        }
    }

    pub fn arity(&self) -> usize {
        match self {
            Callable::Clock => 0,
            Callable::Function(fun) => fun.arity(),
            Callable::Random => 0,
            Callable::Klass(klass) => klass.arity(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LoxFunction {
    pub name: String,
    pub args: Vec<Token>,
    pub body: Rc<Stmt>,

    pub closure: Rc<RefCell<Environment>>,

    pub is_initializer: bool,
}

impl LoxFunction {
    pub fn new(stmt: &Stmt, env: Rc<RefCell<Environment>>, is_initializer: bool) -> Self {
        if let Stmt::Function(name, args, body) = stmt {
            Self {
                name: name.clone(),
                args: args.to_vec(),
                body: body.clone(),
                closure: env,
                is_initializer,
            }
        } else {
            panic!("Invalid function creation argument.")
        }
    }

    pub fn arity(&self) -> usize {
        self.args.len()
    }

    pub fn bind(&self, instance: &LoxInstance) -> LoxFunction {
        let mut env = Environment::with(self.closure.clone());
        env.define("this", Literal::Instance(instance.clone()));

        Self {
            name: self.name.clone(),
            args: self.args.clone(),
            body: self.body.clone(),
            closure: Rc::new(RefCell::new(env)),
            is_initializer: self.is_initializer,
        }
    }

    pub fn call(
        &self,
        interpreter: &mut Interpreter,
        args_values: Vec<Literal>,
    ) -> Result<Literal> {
        let body = self.body.as_ref();

        if self.args.len() != args_values.len() {
            return Err(RuntimeError::InvalidParameters);
        }

        let mut environment = Environment::with(self.closure.clone());
        for (i, token) in self.args.iter().enumerate() {
            environment.define(&token.lexeme(), args_values.get(i).unwrap().clone());
        }

        let res = match interpreter.interpret_within(environment, &[body].iter().map(|&s| s.clone()).collect::<Vec<Stmt>>()) {
            Err(RuntimeError::Return) => interpreter.pop_retval(),
            res => res,
        };

        if self.is_initializer {
            // returning instance instead of nil
            let closure = self.closure.borrow();
            return Ok(closure.get_at(0, "this"));
        }

        res
    }
}
