use js_sys::Date;
use laythe_env::{
  io::IoImpl,
  time::{Time, TimeImpl},
};
use std::time::Duration;

#[derive(Debug)]
pub struct IoTimeWasm {
  start: f64,
}

impl Default for IoTimeWasm {
  fn default() -> Self {
    Self { start: Date::now() }
  }
}

impl IoImpl<Time> for IoTimeWasm {
  fn make(&self) -> Time {
    Time::new(Box::new(TimeWasm { start: self.start }))
  }
}

#[derive(Debug, Clone)]
struct TimeWasm {
  start: f64,
}

impl TimeImpl for TimeWasm {
  fn elapsed(&self) -> Result<Duration, String> {
    let delta = (Date::now() - self.start) as u64;
    let secs = delta / 1000;
    let nanos = ((delta % 1000) * 1000) as u32;
    Ok(Duration::new(secs, nanos))
  }
}
