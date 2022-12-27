use std::{error::Error, fmt::Display};

#[derive(Debug, PartialEq, Eq)]
pub enum ImportError {
  PackageDoesNotMatch,
  ModuleDoesNotExist,
  SymbolDoesNotExist,
  SymbolNotExported,
  MalformedPath,
  InvalidImport,
}

impl Display for ImportError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match *self {
      Self::PackageDoesNotMatch => write!(f, "Package does not exist."),
      Self::ModuleDoesNotExist => write!(f, "Module does not exist."),
      Self::SymbolDoesNotExist => write!(f, "Symbol does not exist."),
      Self::SymbolNotExported => write!(f, "Symbol not exported."),
      Self::MalformedPath => write!(f, "Malformed path."),
      Self::InvalidImport => write!(f, "Invalid import."),
    }
  }
}

impl Error for ImportError {
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

#[derive(Debug, PartialEq, Eq)]
pub enum ModuleInsertError {
  ModuleAlreadyExists,
}

impl Display for ModuleInsertError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match *self {
      Self::ModuleAlreadyExists => write!(f, "Symbol already exists."),
    }
  }
}

impl Error for ModuleInsertError {
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

#[derive(Debug, PartialEq, Eq)]
pub enum SymbolInsertError {
  SymbolAlreadyExists,
}

impl Display for SymbolInsertError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match *self {
      Self::SymbolAlreadyExists => write!(f, "Symbol already exists."),
    }
  }
}

impl Error for SymbolInsertError {
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

#[derive(Debug, PartialEq, Eq)]
pub enum SymbolExportError {
  SymbolDoesNotExist,
  SymbolAlreadyExported,
}

impl Display for SymbolExportError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match *self {
      Self::SymbolDoesNotExist => write!(f, "Symbol does not exist."),
      Self::SymbolAlreadyExported => write!(f, "Symbol already exported."),
    }
  }
}

impl Error for SymbolExportError {
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

pub type ImportResult<T> = Result<T, ImportError>;
pub type ModuleInsertResult = Result<(), ModuleInsertError>;
pub type SymbolInsertResult = Result<(), SymbolInsertError>;
pub type SymbolExportResult = Result<(), SymbolExportError>;
