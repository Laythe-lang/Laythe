class Foo {
  init(a, b) {
    print "init"; // expect: init
    this.a = a;
    this.b = b;
  }
}

let foo = Foo(1, 2);
assertEq(foo.a, 1); // expect: 1
assertEq(foo.b, 2); // expect: 2
