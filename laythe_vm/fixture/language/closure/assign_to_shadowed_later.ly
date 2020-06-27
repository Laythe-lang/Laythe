let a = "global";

{
  fn assign() {
    a = "assigned";
  }

  let a = "inner";
  assign();
  assertEq(a, "inner"); // expect: inner
}

assertEq(a, "assigned"); // expect: assigned
