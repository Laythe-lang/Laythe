var a = "outer";
{
  fun foo() {
    return a;
  }

  assertEq(foo(), "outer"); // expect: outer
  var a = "inner";
  assertEq(foo(), "outer"); // expect: outer
}
