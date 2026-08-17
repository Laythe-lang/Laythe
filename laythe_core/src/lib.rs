#![deny(clippy::all)]
mod align_utils;
mod allocator;
mod captures;
mod chunk;
mod collections;
pub mod constants;
pub mod hooks;
pub mod impls;
mod macros;
pub mod managed;
pub mod module;
pub mod object;
mod reference;
pub mod signature;
pub mod support;
pub mod utils;
pub mod value;

use fnv::FnvBuildHasher;
use hashbrown::HashSet;
use object::Instance;

pub use allocator::{Allocator, NO_GC};
pub use captures::Captures;
pub use chunk::Chunk;
pub use collections::{IndexedResult, UniqueVector, VecBuilder};
pub use hooks::*;
pub use reference::{ObjRef, ObjectRef, Ref};

pub type Call = LyResult<value::Value>;
pub type LyResult<T> = Result<T, LyError>;

#[derive(Debug, PartialEq, Eq)]
pub enum LyError {
  Err(Instance),
  Exit(u16),
}

pub type LyHashSet<K> = HashSet<K, FnvBuildHasher>;
