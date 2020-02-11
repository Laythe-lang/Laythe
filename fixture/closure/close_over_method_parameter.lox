var f;

class Foo {
  method(param) {
    fun f_() {
      assertEq(param, "param");
    }
    f = f_;
  }
}

Foo().method("param");
f(); // expect: param
