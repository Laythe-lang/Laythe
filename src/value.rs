#[derive(Debug)]
pub struct ValueVec {
  pub values: Vec<f64>
}

impl ValueVec {
  pub fn new() -> ValueVec {
    ValueVec { values: Vec::new() }
  }
}