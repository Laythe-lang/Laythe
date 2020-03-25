class Base {
  init(a) {
    this.a = a;
  }
}

class Derived < Base {
  init(a, b) {
    super.init(a);
    this.b = b;
  }
}

var derived = Derived("a", "b");
assertEq(derived.a, "a"); // expect: a
assertEq(derived.b, "b"); // expect: b
