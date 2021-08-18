#![deny(clippy::all)]

mod byte_code;
mod cache;
pub mod compiler;
mod constants;
pub mod source;
pub mod vm;
use codespan_reporting::diagnostic::Diagnostic;

#[cfg(any(test, feature = "debug"))]
mod debug;

/// The result of a compilation
pub type FeResult<T, F> = Result<T, Vec<Diagnostic<F>>>;
