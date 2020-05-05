class A {
  init(param) {
    this.field = param;
  }

  test() {
    return this.field;
  }
}

class B < A {}

var b = B("value");
assertEq(b.test(), "value"); // expect: value
