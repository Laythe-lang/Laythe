class Foo {
  init(a, b) {
    self.a = a;
    self.b = b;
  }
}

let foo = Foo(1, 2, 3, 4); // expect runtime error: Expected 2 arguments but got 4.