class Foo {
  bar() {}
}

assertEq(Foo().bar(), nil); // expect: nil
