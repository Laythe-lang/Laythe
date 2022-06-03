#![deny(clippy::all)]

mod byte_code;
mod cache;
mod chunk_builder;
pub mod compiler;
mod constants;
pub mod source;
pub mod vm;
use codespan_reporting::diagnostic::Diagnostic;
use source::VmFileId;

#[cfg(any(test, feature = "debug"))]
mod debug;

/// The result of a compilation
pub type FeResult<T> = Result<T, Vec<Diagnostic<VmFileId>>>;
