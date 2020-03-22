class Foo {
  init(arg) {
    print "Foo.init(" + arg + ")";
    this.field = "init";
  }
}

fun init() {
  return "not initializer";
}

assertEq(init(), "not initializer"); // expect: not initializer
