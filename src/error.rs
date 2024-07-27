use thiserror::Error;

use super::expr::Expr;

#[derive(Debug, Error)]
pub enum RuntimeError {
    #[error("Unknown variable: {variable}")]
    UnknownVariable { variable: String },
    #[error("Undefined variable: {variable}")]
    UndefinedVariable { variable: String },
    #[error("Undefined property: {property}")]
    UndefinedProperty { property: String },
    #[error("Conversion error")]
    ConversionError,
    #[error("Invalid number of function parameters")]
    InvalidParameters,
    #[error("invalid operator: {operator}")]
    InvalidOperator { operator: String },
    #[error("invalid type")]
    InvalidType,
    #[error("missing literal in evaluate: {expr:?}")]
    MissingLiteral { expr: Expr },
    #[error("Missing return value in retval stack")]
    MissingReturnValue,
    // #[error("Can not overwrite enclosing environment.")]
    // EnvironmentDeniedOverwrite,
    #[error("Variable with this name already exists in scope.")]
    AlreadyKnownVariableInScope,
    #[error("Can't return from top-level code.")]
    ReturningFropTopLevel,
    #[error("Only instances have properties")]
    OnlyInstancesHaveProperties,
    #[error("Only instances have fields")]
    OnlyInstancesHaveFields,
    #[error("Can't return a value from an initializer")]
    ReturningValueFromInitializer,
    #[error("Arguments mismatch: {va0} != {va1}")]
    ArgumentsMismatch { va0: usize, va1: usize },
    #[error("A class can't inherit from itself.")]
    CantInheritFromItself,
    #[error("Superclass must be a class.")]
    SuperclassMustBeClass,
    #[error("Returned value")]
    Return,
    // #[error("Unknown error")]
    // Unknown,
}

pub type Result<T> = ::std::result::Result<T, RuntimeError>;
