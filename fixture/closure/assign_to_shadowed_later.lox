var a = "global";

{
  fun assign() {
    a = "assigned";
  }

  var a = "inner";
  assign();
  assertEq(a, "inner"); // expect: inner
}

assertEq(a, "assigned"); // expect: assigned
