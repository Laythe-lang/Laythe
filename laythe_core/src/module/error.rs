use std::{error::Error, fmt::Display};

#[derive(Debug, PartialEq)]
pub enum ModuleError {
  PackageDoesNotMatch,
  ModuleDoesNotExist,
  ModulePathMalformed,
  ModuleNotDecedent,
  ModuleNotDirectDecedent,
  SymbolDoesNotExist,
  SymbolAlreadyExported,
  SymbolAlreadyExists,
  SymbolNotExported,
  InvalidImport,
}

impl Display for ModuleError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match *self {
      ModuleError::PackageDoesNotMatch => write!(f, "Package does not exist."),
      ModuleError::ModuleDoesNotExist => write!(f, "Module does not exist."),
      ModuleError::ModulePathMalformed => write!(f, "Module path is malformed."),
      ModuleError::ModuleNotDecedent => write!(f, "Module is not a decedent of it's parent module"),
      ModuleError::ModuleNotDirectDecedent => {
        write!(f, "Module is not a direct decedent of it's parent module")
      }
      ModuleError::SymbolDoesNotExist => write!(f, "Symbol does not exist."),
      ModuleError::SymbolAlreadyExported => write!(f, "Symbol already exported."),
      ModuleError::SymbolAlreadyExists => write!(f, "Symbol already exists."),
      ModuleError::SymbolNotExported => write!(f, "Symbol not exported."),
      ModuleError::InvalidImport => write!(f, "Invalid import."),
    }
  }
}

impl Error for ModuleError {
  fn source(&self) -> Option<&(dyn Error + 'static)> {
    None
  }

  fn description(&self) -> &str {
    "description() is deprecated; use Display"
  }

  fn cause(&self) -> Option<&dyn Error> {
    None
  }
}

pub type ModuleResult<T> = Result<T, ModuleError>;
