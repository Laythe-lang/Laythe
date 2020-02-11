var f;

fun foo(param) {
  fun f_() {
    assertEq(param, "param");
  }
  f = f_;
}
foo("param");

f(); // expect: param
