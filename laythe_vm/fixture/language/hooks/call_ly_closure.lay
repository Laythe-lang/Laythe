fn outer(parameter) {
  fn inner() {
    return parameter;
  }

  return inner;
}

assertEq(10, outer(10).call());
assertEq("john", outer("john").call());
assertEq(outer, outer(outer).call());