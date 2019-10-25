type Value = f64;

// 
#[derive(Debug)]
pub struct ValueVec {
  pub values: Vec<Value>
}

impl ValueVec {
  pub fn new() -> ValueVec {
    ValueVec { values: Vec::new() }
  }
}