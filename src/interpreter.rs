use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::callable::{Callable, LoxFunction};
use crate::class::LoxClass;
use crate::environment::Environment;
use crate::error::{Result, RuntimeError};
use crate::expr::{Expr, Literal};
use crate::statement::Stmt;
use crate::token::{Token, TokenType};

#[cfg(test)]
use crate::parser::Parser;
#[cfg(test)]
use crate::scanner::Scanner;

#[derive(Debug)]
pub struct Interpreter {
    env: Rc<RefCell<Environment>>,
    globals: Rc<RefCell<Environment>>,
    retvals: Vec<Literal>,
    locals: HashMap<usize, usize>,
}

impl Interpreter {
    pub fn new() -> Self {
        // Prepare globals
        let mut env = Environment::new();

        env.define("clock", Literal::Callable(Callable::Clock));
        env.define("random", Literal::Callable(Callable::Random));

        let env_ref = Rc::new(RefCell::new(env));

        Self {
            env: env_ref.clone(),
            globals: env_ref,
            retvals: Vec::new(),
            locals: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, stmts: &Vec<Stmt>) -> Result<Literal> {
        let mut last_result = Literal::Nil;
        for stmt in stmts {
            last_result = self.evaluate_statement(stmt)?;
        }

        Ok(last_result)
    }

    pub fn resolve(&mut self, token: &Token, depth: usize) {
        self.locals.insert(token.index(), depth);
    }

    pub fn pop_retval(&mut self) -> Result<Literal> {
        if self.retvals.is_empty() {
            return Err(RuntimeError::MissingReturnValue);
        }

        Ok(self.retvals.pop().unwrap())
    }

    pub fn interpret_within(&mut self, env: Environment, stmts: &Vec<Stmt>) -> Result<Literal> {
        let env = env.clone();
        let previous_env = self.env.clone();

        self.env = Rc::new(RefCell::new(env));

        let res = self.interpret(stmts);

        self.env = previous_env;

        res
    }

    pub fn evaluate_statement(&mut self, stmt: &Stmt) -> Result<Literal> {
        let res = match stmt {
            Stmt::Expr(expr) => self.evaluate(expr)?,
            Stmt::Print(expr) => {
                let res = self.evaluate(expr)?;
                println!("{}", res);
                Literal::Nil
            }
            Stmt::Var(name, expr) => self.evaluate_set_var(name, expr)?,
            Stmt::Block(stmts) => self.evaluate_block(stmts)?,
            Stmt::If(expr, stmt1, stmt2) => {
                self.evaluate_if(expr, stmt1.as_ref(), stmt2.as_ref())?
            }
            Stmt::While(expr, stmt) => self.evaluate_while(expr, stmt.as_ref())?,
            Stmt::Nil => Literal::Nil,
            Stmt::Function(name, args, body) => {
                // XXX TODO: Move in its own function...
                let function = LoxFunction {
                    name: name.clone(),
                    args: args.to_vec(),
                    body: body.clone(),
                    closure: self.env.clone(),
                    is_initializer: false,
                };

                let callable = Callable::Function(function);
                let literal = Literal::Callable(callable);

                self.evaluate_set_var(name, &Expr::Literal { value: literal })?
            }
            Stmt::Return(_, expr) => {
                let val = self.evaluate(expr)?;
                self.retvals.push(val);

                return Err(RuntimeError::Return);
            }
            Stmt::Class(_, _, _) => self.evaluate_class(stmt)?,
        };

        Ok(res)
    }

    pub fn evaluate(&mut self, expr: &Expr) -> Result<Literal> {
        match expr {
            Expr::Nil => Ok(Literal::Nil),
            Expr::Literal { value } => Ok(value.clone()),
            Expr::Binary {
                left,
                operator,
                right,
            } => self.evaluate_binary(left, operator, right),
            Expr::Unary { operator, right } => self.evaluate_unary(operator, right),
            Expr::Variable { name, .. } => self.evaluate_var(name),
            Expr::Logical {
                left,
                operator,
                right,
            } => self.evaluate_logical(left, operator, right),
            Expr::Assign { name, value } => self.evaluate_assign(name, value),
            Expr::Call {
                callee,
                paren,
                args,
            } => self.evaluate_call(callee, paren, args),
            Expr::Get { expr, name } => self.evaluate_get(expr, name),
            Expr::Set { expr, name, value } => self.evaluate_set(expr, name, value),
            Expr::This { keyword } => self.evaluate_this(keyword),
            Expr::Super { keyword, method } => self.evaluate_super(keyword, method),
            _ => Err(RuntimeError::MissingLiteral { expr: expr.clone() }),
        }
    }

    pub fn evaluate_call(
        &mut self,
        callee: &Expr,
        _paren: &Token,
        args: &Vec<Expr>,
    ) -> Result<Literal> {
        let callee = self.evaluate(callee)?;

        let mut callable_args = Vec::new();
        for arg in args {
            callable_args.push(self.evaluate(arg)?);
        }

        let callee = match callee {
            Literal::Callable(f) => f,
            _ => return Err(RuntimeError::InvalidType),
        };

        if args.len() != callee.arity() {
            return Err(RuntimeError::ArgumentsMismatch {
                va0: args.len(),
                va1: callee.arity(),
            });
        }

        callee.call(self, callable_args)
    }

    pub fn evaluate_assign(&mut self, token: &Token, value: &Expr) -> Result<Literal> {
        let literal = self.evaluate(value)?;
        if let Some(distance) = self.locals.get(&token.index()) {
            self.env
                .borrow_mut()
                .assign_at(*distance, &token.lexeme(), literal.clone())?;
        } else {
            self.env
                .borrow_mut()
                .assign(&token.lexeme(), literal.clone())?;
        }
        Ok(literal)
    }

    pub fn evaluate_unary(&mut self, op: &Token, right: &Expr) -> Result<Literal> {
        let right_literal = self.evaluate(right)?;

        match op.token_type {
            TokenType::Minus => {
                let n = right_literal
                    .as_num()
                    .ok_or(RuntimeError::ConversionError)?;
                Ok(Literal::Number(-1. * n))
            }
            TokenType::Bang => {
                let v = right_literal.as_bool().ok_or(RuntimeError::InvalidType)?;
                Ok(Literal::Boolean(!v))
            }
            _ => Err(RuntimeError::InvalidOperator {
                operator: op.lexeme(),
            }),
        }
    }

    pub fn evaluate_binary(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<Literal> {
        let left_literal = self.evaluate(left)?;
        let right_literal = self.evaluate(right)?;

        match op.token_type {
            TokenType::Minus => match (left_literal, right_literal) {
                (Literal::Number(left_n), Literal::Number(right_n)) => {
                    Ok(Literal::Number(left_n - right_n))
                }
                _ => Err(RuntimeError::InvalidOperator {
                    operator: op.lexeme(),
                }),
            },
            TokenType::Plus => match (left_literal, right_literal) {
                (Literal::Number(left_n), Literal::Number(right_n)) => {
                    Ok(Literal::Number(left_n + right_n))
                }
                (Literal::String(left_s), Literal::String(right_s)) => {
                    Ok(Literal::String(left_s + &right_s))
                }
                _ => Err(RuntimeError::InvalidOperator {
                    operator: op.lexeme(),
                }),
            },
            TokenType::Star => match (left_literal, right_literal) {
                (Literal::Number(left_n), Literal::Number(right_n)) => {
                    Ok(Literal::Number(left_n * right_n))
                }
                _ => Err(RuntimeError::InvalidOperator {
                    operator: op.lexeme(),
                }),
            },
            TokenType::Slash => match (left_literal, right_literal) {
                (Literal::Number(left_n), Literal::Number(right_n)) => {
                    Ok(Literal::Number(left_n / right_n))
                }
                _ => Err(RuntimeError::InvalidOperator {
                    operator: op.lexeme(),
                }),
            },
            TokenType::EqualEqual => match (left_literal, right_literal) {
                (Literal::Number(left_n), Literal::Number(right_n)) => {
                    Ok(Literal::Boolean(left_n == right_n))
                }
                (Literal::Boolean(left_n), Literal::Boolean(right_n)) => {
                    Ok(Literal::Boolean(left_n == right_n))
                }
                (Literal::String(left_s), Literal::String(right_s)) => {
                    Ok(Literal::Boolean(left_s == right_s))
                }
                (Literal::Nil, Literal::Nil) => Ok(Literal::Boolean(true)),
                _ => Err(RuntimeError::InvalidOperator {
                    operator: op.lexeme(),
                }),
            },
            TokenType::BangEqual => match (left_literal, right_literal) {
                (Literal::Number(left_n), Literal::Number(right_n)) => {
                    Ok(Literal::Boolean(left_n != right_n))
                }
                (Literal::Boolean(left_n), Literal::Boolean(right_n)) => {
                    Ok(Literal::Boolean(left_n != right_n))
                }
                (Literal::Nil, Literal::Nil) => Ok(Literal::Boolean(false)),
                _ => Err(RuntimeError::InvalidOperator {
                    operator: op.lexeme(),
                }),
            },
            TokenType::Greater => match (left_literal, right_literal) {
                (Literal::Number(left_n), Literal::Number(right_n)) => {
                    Ok(Literal::Boolean(left_n > right_n))
                }
                _ => Err(RuntimeError::InvalidOperator {
                    operator: op.lexeme(),
                }),
            },
            TokenType::GreaterEqual => match (left_literal, right_literal) {
                (Literal::Number(left_n), Literal::Number(right_n)) => {
                    Ok(Literal::Boolean(left_n >= right_n))
                }
                _ => Err(RuntimeError::InvalidOperator {
                    operator: op.lexeme(),
                }),
            },
            TokenType::Less => match (left_literal, right_literal) {
                (Literal::Number(left_n), Literal::Number(right_n)) => {
                    Ok(Literal::Boolean(left_n < right_n))
                }
                _ => Err(RuntimeError::InvalidOperator {
                    operator: op.lexeme(),
                }),
            },
            TokenType::LessEqual => match (left_literal, right_literal) {
                (Literal::Number(left_n), Literal::Number(right_n)) => {
                    Ok(Literal::Boolean(left_n <= right_n))
                }
                _ => Err(RuntimeError::InvalidOperator {
                    operator: op.lexeme(),
                }),
            },
            _ => Err(RuntimeError::InvalidOperator {
                operator: op.lexeme(),
            }),
        }
    }

    fn evaluate_set_var(&mut self, name: &str, expr: &Expr) -> Result<Literal> {
        match self.evaluate(expr) {
            Ok(val) => {
                self.env.borrow_mut().define(name, val);
                Ok(Literal::Nil)
            }
            Err(s) => Err(s),
        }
    }

    fn evaluate_var(&mut self, token: &Token) -> Result<Literal> {
        self.lookup_variable(token)
    }

    fn lookup_variable(&mut self, token: &Token) -> Result<Literal> {
        if let Some(distance) = self.locals.get(&token.index()) {
            Ok(self.env.borrow().get_at(*distance, &token.lexeme()))
        } else if !self.env.borrow().has(&token.lexeme()) {
            Err(RuntimeError::UnknownVariable {
                variable: token.lexeme(),
            })
        } else {
            Ok(self.globals.borrow().get(&token.lexeme()))
        }
    }

    fn evaluate_block(&mut self, stmts: &Vec<Stmt>) -> Result<Literal> {
        let previous_env = self.env.clone();
        let mut last_literal = Literal::Nil;

        self.env = Rc::new(RefCell::new(Environment::with(previous_env.clone())));

        for stmt in stmts {
            last_literal = self.evaluate_statement(stmt)?;
        }

        self.env = previous_env;

        Ok(last_literal)
    }

    fn evaluate_if(&mut self, expr: &Expr, stmt1: &Stmt, stmt2: &Stmt) -> Result<Literal> {
        let res = self.evaluate(expr)?;

        if res.as_bool().is_some_and(|b| b) {
            self.evaluate_statement(stmt1)
        } else {
            self.evaluate_statement(stmt2)
        }
    }

    fn evaluate_logical(&mut self, left: &Expr, operator: &Token, right: &Expr) -> Result<Literal> {
        let left = self.evaluate(left)?;

        if operator.token_type == TokenType::Or {
            if left.as_bool().is_some_and(|b| b) {
                return Ok(left);
            }
        } else if !left.as_bool().is_some_and(|b| b) {
            return Ok(left);
        }

        self.evaluate(right)
    }

    fn evaluate_while(&mut self, expr: &Expr, body: &Stmt) -> Result<Literal> {
        while self.evaluate(expr)?.as_bool().is_some_and(|b| b) {
            self.evaluate_statement(body)?;
        }

        Ok(Literal::Nil)
    }

    fn evaluate_class(&mut self, stmt: &Stmt) -> Result<Literal> {
        if let Stmt::Class(token, superclass, stmts) = stmt {
            let mut superklass = None;

            if superclass != &Expr::nil() {
                let superclass = self.evaluate(superclass)?;
                let mut is_klass = false;

                if let Literal::Callable(Callable::Klass(klass)) = superclass {
                    is_klass = true;
                    superklass = Some(klass);
                }

                if !is_klass {
                    return Err(RuntimeError::SuperclassMustBeClass);
                }
            }

            self.env.borrow_mut().define(&token.lexeme(), Literal::Nil);

            if let Some(ref klass) = superklass {
                let mut env = Environment::with(self.env.clone());
                env.define(
                    "super",
                    Literal::Callable(Callable::Klass(klass.clone())),
                );

                self.env = Rc::new(RefCell::new(env));
            }

            let mut methods = HashMap::new();

            for method in stmts {
                if let Stmt::Function(name, _, _) = method {
                    let fun = LoxFunction::new(method, self.env.clone(), *name == "init");
                    methods.insert(name.clone(), fun);
                } else {
                    return Err(RuntimeError::InvalidType);
                }
            }

            if superklass.is_some() {
                let env = self.env.as_ref().borrow().enclosing.clone().unwrap();
                self.env = env.clone();
            }

            let klass = LoxClass::new(token.lexeme(), superklass, methods);
            self.env
                .borrow_mut()
                .assign(&token.lexeme(), Literal::Callable(Callable::Klass(klass)))?;
        } else {
            return Err(RuntimeError::InvalidType);
        }

        Ok(Literal::Nil)
    }

    fn evaluate_get(&mut self, expr: &Expr, name: &str) -> Result<Literal> {
        let obj = self.evaluate(expr)?;

        if let Literal::Instance(inst) = obj {
            return inst.get(name);
        }

        Err(RuntimeError::OnlyInstancesHaveProperties)
    }

    fn evaluate_set(&mut self, expr: &Expr, name: &str, value: &Expr) -> Result<Literal> {
        let object = self.evaluate(expr)?;

        if let Literal::Instance(mut instance) = object {
            let val = self.evaluate(value)?;
            instance.set(name, val.clone());
            Ok(val)
        } else {
            Err(RuntimeError::OnlyInstancesHaveFields)
        }
    }

    fn evaluate_this(&mut self, keyword: &Token) -> Result<Literal> {
        self.lookup_variable(keyword)
    }

    fn evaluate_super(&mut self, keyword: &Token, method: &Token) -> Result<Literal> {
        let distance = self.locals.get(&keyword.index()).unwrap();

        let superclass = self.env.borrow().get_at(*distance, "super");
        let object = self.env.borrow().get_at(*distance - 1, "this");

        if let Literal::Callable(Callable::Klass(superclass)) = superclass {
            if let Literal::Instance(instance) = object {
                if let Some(method) = superclass.get_method(&method.lexeme()) {
                    return Ok(Literal::Callable(Callable::Function(
                        method.bind(&instance),
                    )));
                } else {
                    return Err(RuntimeError::UndefinedProperty {
                        property: method.lexeme(),
                    });
                }
            }
        }

        Ok(Literal::Nil)
    }
}

#[test]
fn test_evaluate() {
    struct Test<'a> {
        input: &'a str,
        expected: Literal,
    }

    let tests: Vec<Test> = [
        Test {
            input: "nil",
            expected: Literal::Nil,
        },
        Test {
            input: "1",
            expected: Literal::Number(1.),
        },
        Test {
            input: "-2",
            expected: Literal::Number(-2.),
        },
        Test {
            input: "true",
            expected: Literal::Boolean(true),
        },
        Test {
            input: "false",
            expected: Literal::Boolean(false),
        },
        Test {
            input: "1+1",
            expected: Literal::Number(2.),
        },
        Test {
            input: "42",
            expected: Literal::Number(42.),
        },
        Test {
            input: "2-3",
            expected: Literal::Number(-1.),
        },
        Test {
            input: "true",
            expected: Literal::Boolean(true),
        },
        Test {
            input: "false",
            expected: Literal::Boolean(false),
        },
        Test {
            input: "!true",
            expected: Literal::Boolean(false),
        },
        Test {
            input: "!false",
            expected: Literal::Boolean(true),
        },
    ]
    .into();

    let mut interpreter = Interpreter::new();

    for test in tests {
        let mut scanner = Scanner::new(&test.input);
        let res = scanner.scan_tokens();
        assert!(res.is_ok());

        let mut parser = Parser::new(scanner.tokens);
        let res = parser.expression();
        assert!(res.is_ok());

        let res = interpreter.evaluate(&res.unwrap());
        assert!(res.is_ok());

        assert_eq!(test.expected, res.unwrap());
    }
}

#[test]
fn test_interpreter_env() {
    struct Test<'a> {
        input: &'a str,
        expected: Literal,
    }

    let tests: Vec<Test> = [
        Test {
            input: "var a = 1; var b = a + 2; b;",
            expected: Literal::Number(3.),
        },
        Test {
            input: r#"var a = "con"; var b = "cat"; a+b;"#,
            expected: Literal::String("concat".into()),
        },
    ]
    .into();

    let mut interpreter = Interpreter::new();

    for test in tests {
        let mut scanner = Scanner::new(&test.input);
        let res = scanner.scan_tokens();
        assert!(res.is_ok());

        let mut parser = Parser::new(scanner.tokens);
        let res = parser.parse();
        assert!(res.is_ok());

        let res = interpreter.interpret(&res.unwrap());
        assert!(res.is_ok());

        assert_eq!(test.expected, res.unwrap());
    }
}
