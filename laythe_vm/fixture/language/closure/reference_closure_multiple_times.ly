let f;

{
  let a = "a";
  fn f_() {
    assertEq(a, "a");
    assertEq(a, "a");
  }
  f = f_;
}

f();
// expect: a
// expect: a
