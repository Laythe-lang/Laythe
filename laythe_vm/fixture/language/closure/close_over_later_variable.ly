// This is a regression test. There was a bug where if an upvalue for an
// earlier local (here "a") was captured *after* a later one ("b"), then it
// would crash because it walked to the end of the upvalue list (correct), but
// then didn't handle not finding the variable.

fn f() {
  let a = "a";
  let b = "b";
  fn g() {
    assertEq(b, "b"); // expect: b
    assertEq(a, "a"); // expect: a
  }
  g();
}
f();
