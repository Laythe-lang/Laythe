class Foo {
  init() {
    print "init";
    return;
    assert(false);
  }
}

let foo = Foo(); // expect: init
print foo; // expect: Foo instance
