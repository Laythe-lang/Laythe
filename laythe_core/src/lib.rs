#![deny(clippy::all)]
pub mod call_frame;
pub mod chunk;
pub mod constants;
pub mod hooks;
pub mod impls;
pub mod managed;
pub mod memory;
pub mod module;
pub mod object;
pub mod signature;
pub mod support;
pub mod utils;
pub mod value;

pub type Call = LyResult<value::Value>;
pub type LyResult<T> = Result<T, LyError>;

#[derive(Debug)]
pub enum LyError {
  Err(GcObj<Instance>),
  Exit(u16),
}

pub type LyHashSet<K> = HashSet<K, FnvBuildHasher>;

use fnv::FnvBuildHasher;
use hashbrown::HashSet;
use managed::GcObj;
use object::Instance;
