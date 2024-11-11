#![deny(clippy::all)]
mod captures;
mod chunk;
mod align_utils;
mod collections;
mod reference;
mod macros;
pub mod constants;
pub mod hooks;
pub mod impls;
mod allocator;
pub mod managed;
pub mod module;
pub mod object;
pub mod signature;
pub mod support;
pub mod utils;
pub mod value;

use fnv::FnvBuildHasher;
use hashbrown::HashSet;
use object::Instance;

pub use collections::{VecBuilder, IndexedResult};
pub use reference::{ObjectRef, ObjRef, Ref};
pub use captures::Captures;
pub use allocator::{Allocator, NO_GC};
pub use chunk::Chunk;
pub use hooks::*;

pub type Call = LyResult<value::Value>;
pub type LyResult<T> = Result<T, LyError>;

#[derive(Debug, PartialEq, Eq)]
pub enum LyError {
  Err(Instance),
  Exit(u16),
}

pub type LyHashSet<K> = HashSet<K, FnvBuildHasher>;

