class Foo {
  init() {
    fn init() {
      return "bar";
    }
    assertEq(init(), "bar"); // expect: bar
  }
}

print(Foo()); // expect: Foo instance
