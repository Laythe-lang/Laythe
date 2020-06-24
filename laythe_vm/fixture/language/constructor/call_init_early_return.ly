class Foo {
  init() {
    print "init";
    return;
    assert(false);
  }
}

var foo = Foo(); // expect: init
print foo.init(); // expect: init
// expect: Foo instance
