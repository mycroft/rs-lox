use std::collections::HashMap;

use crate::error::{Result, RuntimeError};

use crate::{expr::Expr, interpreter::Interpreter, statement::Stmt, token::Token};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FunctionType {
    None,
    Function,
    Initializer,
    Method,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ClassType {
    None,
    Class,
    SubClass,
}

#[derive(Debug)]
pub struct Resolver<'a> {
    scopes: Vec<HashMap<String, bool>>,
    interpreter: &'a mut Interpreter,
    current_function: FunctionType,
    current_class: ClassType,
}

impl<'a> Resolver<'a> {
    pub fn new(interpreter: &'a mut Interpreter) -> Self {
        Self {
            scopes: Vec::new(),
            interpreter,
            current_function: FunctionType::None,
            current_class: ClassType::None,
        }
    }

    pub fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn end_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn declare(&mut self, token_name: String) -> Result<()> {
        if self.scopes.is_empty() {
            return Ok(());
        }

        let scope = self.scopes.last_mut().unwrap();

        if scope.contains_key(&token_name) {
            return Err(RuntimeError::AlreadyKnownVariableInScope);
        }
        scope.insert(token_name, false);

        Ok(())
    }

    pub fn define(&mut self, token_name: String) {
        if self.scopes.is_empty() {
            return;
        }

        let scope = self.scopes.last_mut().unwrap();
        scope.insert(token_name, true);
    }

    pub fn resolve(&mut self, stmts: &Vec<Stmt>) -> Result<()> {
        for stmt in stmts {
            self.resolve_stmt(stmt)?;
        }

        Ok(())
    }

    fn resolve_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Block(_) => self.visit_block_stmt(stmt),
            Stmt::Expr(_) => self.visit_expr_stmt(stmt),
            Stmt::Function(_, _, _) => self.visit_function_stmt(stmt),
            Stmt::If(_, _, _) => self.visit_if_stmt(stmt),
            Stmt::Nil => Ok(()),
            Stmt::Print(_) => self.visit_print_stmt(stmt),
            Stmt::Return(_, _) => self.visit_return_stmt(stmt),
            Stmt::Var(_, _) => self.visit_var_stmt(stmt),
            Stmt::While(_, _) => self.visit_while_stmt(stmt),
            Stmt::Class(_, _, _) => self.visit_class_stmt(stmt),
        }
    }

    fn resolve_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Assign { .. } => self.visit_assign_expr(expr),
            Expr::Binary { .. } => self.visit_binary_expr(expr),
            Expr::Call { .. } => self.visit_call_expr(expr),
            Expr::Grouping { .. } => self.visit_grouping_expr(expr),
            Expr::Literal { .. } => self.visit_literal_expr(expr),
            Expr::Logical { .. } => self.visit_logical_expr(expr),
            Expr::Nil => {}
            Expr::Unary { .. } => self.visit_unary_expr(expr),
            Expr::Variable { .. } => self.visit_variable_expr(expr),
            Expr::Get { .. } => self.visit_get_expr(expr),
            Expr::Set { .. } => self.visit_set_expr(expr),
            Expr::This { .. } => self.visit_this_expr(expr),
            Expr::Super { .. } => self.visit_super_expr(expr),
        }
    }

    fn resolve_local(&mut self, token: &Token) {
        if self.scopes.is_empty() {
            return;
        }

        let mut i = self.scopes.len() - 1;

        loop {
            let scope = &self.scopes[i];

            if scope.contains_key(&token.lexeme()) {
                self.interpreter.resolve(token, self.scopes.len() - 1 - i);
            }

            if i == 0 {
                break;
            }

            i -= 1;
        }
    }

    fn resolve_function(&mut self, stmt: &Stmt, function_type: FunctionType) -> Result<()> {
        let enclosing_function = self.current_function;
        self.current_function = function_type;

        match stmt {
            Stmt::Function(_, args, stmt) => {
                self.begin_scope();

                for param in args {
                    self.declare(param.lexeme().clone())?;
                    self.define(param.lexeme());
                }

                self.resolve_stmt(stmt)?;

                self.end_scope();
            }
            _ => panic!("resolve_function should only handle Stmt::Function"),
        }

        self.current_function = enclosing_function;

        Ok(())
    }

    /*
     * Statements
     */
    fn visit_block_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        self.begin_scope();
        match stmt {
            Stmt::Block(stmts) => self.resolve(stmts)?,
            _ => panic!("visit_block_stmt should only handle Stmt::Block"),
        }
        self.end_scope();

        Ok(())
    }

    fn visit_var_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Var(name, expr) => {
                self.declare(name.clone())?;
                self.resolve_expr(expr);
                if *expr != Expr::Nil {
                    self.define(name.clone());
                }
            }
            _ => panic!("visit_var_stmt should only handle Stmt::Var"),
        }

        Ok(())
    }

    fn visit_function_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Function(name, _params, _stmt) => {
                self.declare(name.clone())?;
                self.define(name.clone());

                self.resolve_function(stmt, FunctionType::Function)?;
            }
            _ => panic!("visit_function_stmt should only handle Stmt::Function"),
        }

        Ok(())
    }

    fn visit_expr_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Expr(expr) => {
                self.resolve_expr(expr);
            }
            _ => panic!("visit_expr_stmt should only handle Stmt::Expr"),
        }

        Ok(())
    }

    fn visit_if_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::If(condition, then_branch, else_branch) => {
                self.resolve_expr(condition);
                self.resolve_stmt(then_branch)?;

                if **else_branch != Stmt::Nil {
                    self.resolve_stmt(else_branch)?;
                }
            }
            _ => panic!("visit_if_stmt should only handle Stmt::If"),
        }

        Ok(())
    }

    fn visit_print_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Print(expr) => {
                self.resolve_expr(expr);
            }
            _ => panic!("visit_print_stmt should only handle Stmt::Print"),
        }

        Ok(())
    }

    fn visit_return_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Return(_, value) => {
                if self.current_function == FunctionType::None {
                    return Err(RuntimeError::ReturningFropTopLevel);
                }

                if *value != Expr::Nil {
                    if self.current_function == FunctionType::Initializer {
                        return Err(RuntimeError::ReturningValueFromInitializer);
                    }
                    self.resolve_expr(value);
                }
            }
            _ => panic!("visit_return_stmt should only handle Stmt::Result"),
        }

        Ok(())
    }

    fn visit_while_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::While(expr, stmt) => {
                self.resolve_expr(expr);
                self.resolve_stmt(stmt)?;
            }
            _ => panic!("visit_while_stmt should only handle Stmt::While"),
        }

        Ok(())
    }

    fn visit_class_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        if let Stmt::Class(name, superclass, methods) = stmt {
            let enclosing_class = self.current_class;
            self.current_class = ClassType::Class;

            self.declare(name.lexeme())?;
            self.define(name.lexeme());

            if let Expr::Variable {
                name: superclass_name,
            } = superclass
            {
                if superclass_name.lexeme() == name.lexeme() {
                    return Err(RuntimeError::CantInheritFromItself);
                }

                self.current_class = ClassType::SubClass;

                self.resolve_expr(superclass);

                self.begin_scope();
                self.scopes.last_mut().unwrap().insert("super".into(), true);
            }

            self.begin_scope();
            self.scopes
                .last_mut()
                .unwrap()
                .insert("this".to_string(), true);

            for method in methods {
                let declaration = if let Stmt::Function(name, _, _) = method {
                    if *name == "init" {
                        FunctionType::Initializer
                    } else {
                        FunctionType::Method
                    }
                } else {
                    FunctionType::Method
                };

                self.resolve_function(method, declaration)?;
            }

            self.end_scope();

            if let Expr::Variable { name: _ } = superclass {
                self.end_scope();
            }

            self.current_class = enclosing_class;
        } else {
            panic!("visit_class_stmt should only handle Stmt::Class");
        }

        Ok(())
    }

    /*
     * Expressions
     */
    fn visit_variable_expr(&mut self, expr: &Expr) {
        let token = match expr {
            Expr::Variable { ref name } => name.clone(),
            _ => panic!("visit_variable_expr should only handle Expr::Variable"),
        };

        if !self.scopes.is_empty()
            && self
                .scopes
                .last()
                .unwrap()
                .get(&token.lexeme())
                .unwrap_or(&true)
                == &false
        {
            panic!("Can't read local variable in its own initializer.")
        }

        self.resolve_local(&token);
    }

    fn visit_assign_expr(&mut self, expr: &Expr) {
        let (token, value) = match expr {
            Expr::Assign { name, value } => (name, value),
            _ => panic!("visit_assign_expr should only handle Expr::Assign"),
        };

        self.resolve_expr(value);
        self.resolve_local(token);
    }

    fn visit_binary_expr(&mut self, expr: &Expr) {
        if let Expr::Binary {
            left,
            operator: _,
            right,
        } = expr
        {
            self.resolve_expr(left);
            self.resolve_expr(right);
        } else {
            panic!("visit_binary_expr should only handle Expr::Binary");
        }
    }

    fn visit_call_expr(&mut self, expr: &Expr) {
        if let Expr::Call {
            callee,
            paren: _,
            args,
        } = expr
        {
            self.resolve_expr(callee);

            for arg in args {
                self.resolve_expr(arg);
            }
        } else {
            panic!("visit_call_expr should only handle Expr::Call");
        }
    }

    fn visit_grouping_expr(&mut self, expr: &Expr) {
        if let Expr::Grouping { expr } = expr {
            self.resolve_expr(expr);
        } else {
            panic!("visit_grouping_expr should only handle Expr::Grouping");
        }
    }

    fn visit_literal_expr(&mut self, expr: &Expr) {
        if let Expr::Literal { value: _ } = expr {
            // nothing is happening here
        } else {
            panic!("visit_literal_expr should only handle Expr::Literal");
        }
    }

    fn visit_logical_expr(&mut self, expr: &Expr) {
        if let Expr::Logical {
            left,
            operator: _,
            right,
        } = expr
        {
            self.resolve_expr(left);
            self.resolve_expr(right);
        } else {
            panic!("visit_logical_expr should only handle Expr::Logical");
        }
    }

    fn visit_unary_expr(&mut self, expr: &Expr) {
        if let Expr::Unary { operator: _, right } = expr {
            self.resolve_expr(right);
        } else {
            panic!("visit_unary_expr should only handle Expr::Unary");
        }
    }

    fn visit_get_expr(&mut self, expr: &Expr) {
        if let Expr::Get { expr, name: _ } = expr {
            self.resolve_expr(expr);
        } else {
            panic!("visit_get_expr should only handle Expr::Get");
        }
    }

    fn visit_set_expr(&mut self, expr: &Expr) {
        if let Expr::Set {
            expr,
            name: _,
            value,
        } = expr
        {
            self.resolve_expr(expr);
            self.resolve_expr(value);
        } else {
            panic!("visit_set_expr should only handle Expr::Set");
        }
    }

    fn visit_this_expr(&mut self, expr: &Expr) {
        if let Expr::This { keyword } = expr {
            if self.current_class == ClassType::None {
                panic!("Can't use 'this' outside of a class.");
            }
            self.resolve_local(keyword);
        } else {
            panic!("visit_this_expr should only handle Expr::This");
        }
    }

    fn visit_super_expr(&mut self, expr: &Expr) {
        if self.current_class == ClassType::None {
            panic!("Can't use 'super' outside of a class.");
        } else if self.current_class != ClassType::SubClass {
            panic!("Can't user 'super' in a class with no superclass.");
        }

        if let Expr::Super { keyword, method: _ } = expr {
            self.resolve_local(keyword);
        } else {
            panic!("visit_super_expr should only handle Expr::Super");
        }
    }
}
