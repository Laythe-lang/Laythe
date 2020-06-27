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

let derived = Derived("a", "b");
assertEq(derived.a, "a"); // expect: a
assertEq(derived.b, "b"); // expect: b
