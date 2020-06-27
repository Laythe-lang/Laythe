let f;

class Foo {
  method(param) {
    fn f_() {
      assertEq(param, "param");
    }
    f = f_;
  }
}

Foo().method("param");
f(); // expect: param
