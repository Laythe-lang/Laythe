class A {
  init(param) {
    this.field = param;
  }

  test() {
    return this.field;
  }
}

class B < A {}

let b = B("value");
assertEq(b.test(), "value"); // expect: value
