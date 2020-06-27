fn outer() {
  fn inner() {
    return nil + false;
  }

  return inner;
}

outer().call();