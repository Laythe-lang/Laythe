class Foo {
  init(arg) {
    print("Foo.init(" + arg + ")");
    self.field = "init";
  }
}

let foo = Foo("one"); // expect: Foo.init(one)
foo.field = "field";

let foo2 = foo.init("two"); // expect: Foo.init(two)
print(foo2); // expect: Foo instance

// Make sure init() doesn't create a fresh instance.
assertEq(foo.field, "init"); // expect: init
