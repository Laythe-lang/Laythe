//! Interned string and more for rust.
//!
//! # What is interning?
//!
//! Interning is a method to store exactly one copy of immutable data.
//!
//! Imagine your program holds lots of string values, mostly same value in it,
//! and does not mutate them at all. If you use `String` to store them,
//! lots of memories are wasted just for storing identical texts.
//!
//! Interning efficiently eliminate this problem by managing global pool of cache,
//! in the case above the type of the pool can be `HashSet<Rc<str>>`.
//! When you need a new owned string, first you should lookup global pool for it.
//! If desired string is found then use it.
//! If not, just create a new one and put them also to the pool.
//!
//! Or, you can just use `internship` and ignore all the hassle. Why not?
//!
//! # What does this library provide?
//!
//! This crate exposes a set of interned types which correspond to `Rc`
//! but guaranteed to be unique over its value within thread.
//! Instances of them are per-thread cached to archive this goal.
//!
//! Additionally, these types does not heap-allocate small data that can be fit on stack.
//! Size limit of inline-able data is 15 bytes on 64-byte machines.
//!
//! `IStr`, `IBytes`, and `ICStr` correspond to `str`, `[u8]`, and `CStr` respectively.

mod handle;
mod istr;

pub use istr::IStr;
