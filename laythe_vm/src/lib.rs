#![deny(clippy::all)]

pub mod ast;
mod cache;
mod call_frame;
pub mod compiler;
mod constants;
pub mod files;
pub mod parser;
pub mod scanner;
pub mod token;
pub mod vm;
use codespan_reporting::diagnostic::Diagnostic;

#[cfg(test)]
pub mod ast_printer;

#[cfg(any(test, feature = "debug"))]
mod debug;

/// The result of a compilation
pub type FeResult<T, F> = Result<T, Vec<Diagnostic<F>>>;
