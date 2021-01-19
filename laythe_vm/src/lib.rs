#![deny(clippy::all)]

pub mod ast;
pub mod call_frame;
pub mod compiler;
pub mod constants;
pub mod debug;
pub mod files;
pub mod parser;
pub mod scanner;
pub mod token;
pub mod vm;
use codespan_reporting::diagnostic::Diagnostic;

#[cfg(test)]
pub mod ast_printer;

/// The result of a compilation
pub type FeResult<T, F> = Result<T, Vec<Diagnostic<F>>>;
