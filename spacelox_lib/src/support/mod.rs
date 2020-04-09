#[cfg(test)]
use spacelox_core::io::NativeStdIo;
#[cfg(test)]
use spacelox_core::managed::Trace;
#[cfg(test)]
use spacelox_core::memory::{Gc, NoGc};

#[cfg(test)]
pub fn test_native_dependencies() -> (Gc, Box<dyn Trace>) {
  let gc = Gc::new(Box::new(NativeStdIo()));
  (gc, Box::new(NoGc()))
}
