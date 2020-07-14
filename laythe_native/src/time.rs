use laythe_env::{
  io::IoImpl,
  time::{Time, TimeImpl},
};
use std::time::{Duration, SystemTime};

#[derive(Debug, Clone)]
pub struct IoTimeNative(SystemTime);

impl Default for IoTimeNative {
  fn default() -> Self {
    Self(SystemTime::now())
  }
}

impl IoImpl<Time> for IoTimeNative {
  fn make(&self) -> Time {
    Time::new(Box::new(TimeNative::new(self.0)))
  }
}

#[derive(Debug, Clone)]
pub struct TimeNative {
  start: SystemTime,
}

impl TimeNative {
  pub fn new(start: SystemTime) -> Self {
    Self { start }
  }
}

impl TimeImpl for TimeNative {
  fn elapsed(&self) -> Result<Duration, String> {
    self.start.elapsed().map_err(|err| err.to_string())
  }
}
