// Bound methods have identity equality.
class Foo {
  method() {}
}

var foo = Foo();
var fooMethod = foo.method;

// Same bound method.
assert(fooMethod == fooMethod); // expect: true

// Different closurizations.
assertEq(foo.method == foo.method, false); // expect: false
