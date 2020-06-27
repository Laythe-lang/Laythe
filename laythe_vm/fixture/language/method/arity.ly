class Foo {
  method0() { return "no args"; }
  method1(a) { return a; }
  method2(a, b) { return a + b; }
  method3(a, b, c) { return a + b + c; }
  method4(a, b, c, d) { return a + b + c + d; }
  method5(a, b, c, d, e) { return a + b + c + d + e; }
  method6(a, b, c, d, e, f) { return a + b + c + d + e + f; }
  method7(a, b, c, d, e, f, g) { return a + b + c + d + e + f + g; }
  method8(a, b, c, d, e, f, g, h) { return a + b + c + d + e + f + g + h; }
}

let foo = Foo();
assertEq(foo.method0(), "no args"); // expect: no args
assertEq(foo.method1(1), 1); // expect: 1
assertEq(foo.method2(1, 2), 3); // expect: 3
assertEq(foo.method3(1, 2, 3), 6); // expect: 6
assertEq(foo.method4(1, 2, 3, 4), 10); // expect: 10
assertEq(foo.method5(1, 2, 3, 4, 5), 15); // expect: 15
assertEq(foo.method6(1, 2, 3, 4, 5, 6), 21); // expect: 21
assertEq(foo.method7(1, 2, 3, 4, 5, 6, 7), 28); // expect: 28
assertEq(foo.method8(1, 2, 3, 4, 5, 6, 7, 8), 36); // expect: 36
