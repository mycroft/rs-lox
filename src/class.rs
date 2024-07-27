use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::callable::{Callable, LoxFunction};
use crate::error::Result;
use crate::{expr::Literal, interpreter::Interpreter};

#[derive(Debug, Clone, PartialEq)]
pub struct LoxClass {
    name: String,
    superclass: Box<Option<LoxClass>>,
    methods: Rc<RefCell<HashMap<String, LoxFunction>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LoxInstance {
    klass: LoxClass,
    fields: Rc<RefCell<HashMap<String, Literal>>>,
}

impl LoxClass {
    pub fn new(
        name: String,
        superclass: Option<LoxClass>,
        methods: HashMap<String, LoxFunction>,
    ) -> Self {
        Self {
            name,
            superclass: Box::new(superclass),
            methods: Rc::new(RefCell::new(methods)),
        }
    }

    pub fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Literal>) -> Result<Literal> {
        let instance = LoxInstance::new(self.clone());
        if let Some(initializer) = self.get_method("init") {
            initializer.bind(&instance).call(interpreter, arguments)?;
        }
        Ok(Literal::Instance(instance))
    }

    pub fn name(&self) -> String {
        self.name.clone()
    }

    pub fn get_method(&self, name: &str) -> Option<LoxFunction> {
        let methods = self.methods.borrow();

        if methods.contains_key(name) {
            return methods.get(name).cloned();
        }

        if let Some(klass) = self.superclass.as_ref() {
            return klass.get_method(name);
        }

        None
    }

    pub fn arity(&self) -> usize {
        if let Some(initializer) = self.get_method("init") {
            initializer.arity()
        } else {
            0
        }
    }
}

impl LoxInstance {
    pub fn new(klass: LoxClass) -> Self {
        Self {
            klass,
            fields: Rc::new(RefCell::new(HashMap::new())),
        }
    }

    pub fn klass_name(&self) -> String {
        self.klass.name()
    }

    pub fn get(&self, name: &str) -> Result<Literal> {
        let fields = self.fields.borrow();
        if fields.contains_key(name) {
            Ok(fields.get(name).unwrap().clone())
        } else if let Some(method) = self.klass.get_method(name) {
            let fun = method.bind(self);
            Ok(Literal::Callable(Callable::Function(fun)))
        } else {
            Err(crate::error::RuntimeError::UndefinedProperty { property: name.into() })
        }
    }

    pub fn set(&mut self, name: &str, value: Literal) {
        let mut fields = self.fields.borrow_mut();
        fields.insert(name.into(), value);
    }
}
