{
  let foo = "closure";
  fn f() {
    {
      assertEq(foo, "closure"); // expect: closure
      let foo = "shadow";
      assertEq(foo, "shadow"); // expect: shadow
    }
    assertEq(foo, "closure"); // expect: closure
  }
  f();
}
