use crate::io::IoImpl;
use std::time::Duration;

/// A wrapper around time facilities provided to Laythe
pub struct Time {
  time: Box<dyn TimeImpl>,
}

impl Default for Time {
  fn default() -> Self {
    Self {
      time: Box::new(TimeMock()),
    }
  }
}

impl Time {
  /// Create a new wrapper around time
  pub fn new(time: Box<dyn TimeImpl>) -> Self {
    Self { time }
  }

  /// Get a duration from the start of the vm startup
  pub fn elapsed(&self) -> Result<Duration, String> {
    self.time.elapsed()
  }
}

pub trait TimeImpl {
  fn elapsed(&self) -> Result<Duration, String>;
}

#[derive(Debug)]
pub struct IoTimeMock();

impl IoImpl<Time> for IoTimeMock {
  fn make(&self) -> Time {
    Time::new(Box::new(TimeMock()))
  }
}

pub struct TimeMock();

impl TimeImpl for TimeMock {
  fn elapsed(&self) -> Result<Duration, String> {
    Ok(Duration::new(3, 14236))
  }
}
