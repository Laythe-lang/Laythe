class Foo {
  static method0() { return "no args"; }
  static method1(a) { return a; }
  static method2(a, b) { return a + b; }
  static method3(a, b, c) { return a + b + c; }
  static method4(a, b, c, d) { return a + b + c + d; }
  static method5(a, b, c, d, e) { return a + b + c + d + e; }
  static method6(a, b, c, d, e, f) { return a + b + c + d + e + f; }
  static method7(a, b, c, d, e, f, g) { return a + b + c + d + e + f + g; }
  static method8(a, b, c, d, e, f, g, h) { return a + b + c + d + e + f + g + h; }
}

assertEq(Foo.method0(), "no args"); // expect: no args
assertEq(Foo.method1(1), 1); // expect: 1
assertEq(Foo.method2(1, 2), 3); // expect: 3
assertEq(Foo.method3(1, 2, 3), 6); // expect: 6
assertEq(Foo.method4(1, 2, 3, 4), 10); // expect: 10
assertEq(Foo.method5(1, 2, 3, 4, 5), 15); // expect: 15
assertEq(Foo.method6(1, 2, 3, 4, 5, 6), 21); // expect: 21
assertEq(Foo.method7(1, 2, 3, 4, 5, 6, 7), 28); // expect: 28
assertEq(Foo.method8(1, 2, 3, 4, 5, 6, 7, 8), 36); // expect: 36
