{
  var foo = "closure";
  fun f() {
    {
      assertEq(foo, "closure"); // expect: closure
      var foo = "shadow";
      assertEq(foo, "shadow"); // expect: shadow
    }
    assertEq(foo, "closure"); // expect: closure
  }
  f();
}
