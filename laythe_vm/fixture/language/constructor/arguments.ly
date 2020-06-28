class Foo {
  init(a, b) {
    print "init"; // expect: init
    self.a = a;
    self.b = b;
  }
}

let foo = Foo(1, 2);
assertEq(foo.a, 1); // expect: 1
assertEq(foo.b, 2); // expect: 2
