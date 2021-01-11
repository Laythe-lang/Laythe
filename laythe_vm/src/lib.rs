#![deny(clippy::all)]
pub mod ast;
pub mod call_frame;
pub mod compiler;
pub mod constants;
pub mod debug;
pub mod parser;
pub mod scanner;
pub mod token;
pub mod vm;

#[cfg(test)]
pub mod ast_printer;
