use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::error::{Result, RuntimeError};
use super::expr::Literal;

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    pub env: HashMap<String, Literal>,
    pub enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            env: HashMap::new(),
            enclosing: None,
        }
    }

    pub fn with(enclosing: Rc<RefCell<Environment>>) -> Self {
        Self {
            env: HashMap::new(),
            enclosing: Some(enclosing),
        }
    }

    pub fn define(&mut self, name: &str, value: Literal) {
        self.env.insert(name.into(), value);
    }

    pub fn get_at(&self, distance: usize, name: &str) -> Literal {
        if distance == 0 {
            self.get(name)
        } else if let Some(enclosing_env) = &self.enclosing {
            return enclosing_env.as_ref().borrow().get_at(distance - 1, name);
        } else {
            Literal::Nil
        }
    }

    pub fn assign(&mut self, name: &str, value: Literal) -> Result<Literal> {
        if self.env.contains_key(name) {
            self.env.insert(name.into(), value.clone());
            Ok(value)
        } else if let Some(enclosing_env) = &self.enclosing {
            enclosing_env.as_ref().borrow_mut().assign(name, value)
        } else {
            Err(RuntimeError::UndefinedVariable { variable: name.into() })
        }
    }

    pub fn assign_at(&mut self, distance: usize, name: &str, value: Literal) -> Result<Literal> {
        if distance == 0 {
            self.assign(name, value)
        } else if let Some(enclosing_env) = &self.enclosing {
            enclosing_env
                .as_ref()
                .borrow_mut()
                .assign_at(distance - 1, name, value)
        } else {
            Ok(Literal::Nil)
        }
    }

    pub fn get(&self, name: &str) -> Literal {
        if let Some(val) = self.env.get(name) {
            return val.clone();
        }

        if let Some(enclosing_env) = &self.enclosing {
            return enclosing_env.as_ref().borrow().get(name);
        }

        Literal::Nil
    }

    pub fn has(&self, name: &str) -> bool {
        if self.env.contains_key(name) {
            true
        } else if let Some(enclosing_env) = &self.enclosing {
            enclosing_env.as_ref().borrow().has(name)
        } else {
            false
        }
    }
}

#[test]
fn test_environment() {
    let mut env = Environment::new();
    assert!(!env.has("a"));

    env.define("a", Literal::Boolean(true));
    assert!(env.has("a"));
    assert!(!env.has("b"));
    assert_eq!(Literal::Boolean(true), env.get("a"));

    env.define("a", Literal::Boolean(false));
    assert_eq!(Literal::Boolean(false), env.get("a"));
    assert_eq!(Literal::Nil, env.get("ab"));
}

#[test]
fn test_enclosing_env() {
    let env = Rc::new(RefCell::new(Environment::new()));
    env.borrow_mut().define("a", Literal::Number(42.));

    {
        let mut child_env = Environment::with(env.clone());
        assert_eq!(Literal::Number(42.), child_env.get("a"));

        assert!(child_env.assign("a".into(), Literal::Number(4242.)).is_ok());
        assert_eq!(Literal::Number(4242.), child_env.get("a"));
    }

    assert_eq!(Literal::Number(4242.), env.borrow().get("a"));
}
