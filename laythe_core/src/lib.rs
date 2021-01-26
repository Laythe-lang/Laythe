#![deny(clippy::all)]
pub mod chunk;
pub mod constants;
pub mod hooks;
pub mod iterator;
pub mod module;
pub mod native;
pub mod object;
pub mod package;
pub mod signature;
pub mod utils;
pub mod value;

pub type Call = LyResult<value::Value>;
pub type LyHashSet<K> = HashSet<K, FnvBuildHasher>;

use std::fmt;

use fnv::FnvBuildHasher;
use hashbrown::HashSet;
use laythe_env::managed::Gc;
use object::Instance;

#[derive(Clone, PartialEq, Debug)]
pub enum LyResult<T> {
  Ok(T),
  Err(Gc<Instance>),
  Exit(u16),
}

#[macro_export]
macro_rules! get {
  ( $x:expr ) => {
    match $x {
      Call::Ok(val) => val,
      Call::Err(e) => return Call::Err(e),
      Call::Exit(e) => return Call::Exit(e),
    }
  };
}

// This is a separate function to reduce the code size of the methods
#[inline(never)]
#[cold]
#[track_caller]
fn unwrap_failed(msg: &str, error: &dyn fmt::Debug) -> ! {
  panic!("{}: {:?}", msg, error)
}

impl<T> LyResult<T> {
  #[inline]
  pub fn is_ok(&self) -> bool {
    matches!(self, Self::Ok(_))
  }

  #[inline]
  pub fn is_err(&self) -> bool {
    matches!(self, Self::Err(_))
  }

  #[inline]
  pub fn is_exit(&self) -> bool {
    matches!(self, Self::Exit(_))
  }

  #[inline]
  pub fn map<U, F: FnOnce(T) -> U>(self, op: F) -> LyResult<U> {
    match self {
      Self::Ok(t) => LyResult::Ok(op(t)),
      Self::Err(e) => LyResult::Err(e),
      Self::Exit(e) => LyResult::Exit(e),
    }
  }

  #[inline]
  pub fn and_then<U, F: FnOnce(T) -> LyResult<U>>(self, op: F) -> LyResult<U> {
    match self {
      Self::Ok(t) => op(t),
      Self::Err(e) => LyResult::Err(e),
      Self::Exit(e) => LyResult::Exit(e),
    }
  }

  #[inline]
  #[track_caller]
  pub fn expect(self, msg: &str) -> T {
    match self {
      Self::Ok(t) => t,
      Self::Err(e) => unwrap_failed(msg, &e),
      Self::Exit(e) => unwrap_failed(msg, &e),
    }
  }

  pub fn unwrap(self) -> T {
    match self {
      Self::Ok(t) => t,
      Self::Err(e) => unwrap_failed("called `LyResult::unwrap()` on an `Err` value", &e),
      Self::Exit(e) => unwrap_failed("called `LyResult::unwrap()` on an `Exit` value", &e),
    }
  }
}

impl<T: fmt::Debug> LyResult<T> {
  #[inline]
  #[track_caller]
  pub fn expect_err(self, msg: &str) -> Gc<Instance> {
    match self {
      Self::Ok(t) => unwrap_failed(msg, &t),
      Self::Err(e) => e,
      Self::Exit(e) => unwrap_failed(msg, &e),
    }
  }
}
