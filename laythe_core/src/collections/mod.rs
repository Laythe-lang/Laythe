mod array;
mod shared_vector;

pub use array::{Array, ArrayHandle};
pub use shared_vector::{
  IndexedResult, RawSharedVector, RawSharedVectorHandle, RawVecLocation, VecBuilder,
};
