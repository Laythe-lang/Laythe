let f;

fn foo(param) {
  fn f_() {
    assertEq(param, "param");
  }
  f = f_;
}
foo("param");

f(); // expect: param
