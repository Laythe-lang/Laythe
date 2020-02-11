// Bound methods have identity equality.
class Foo {}
class Bar {}

assert(Foo == Foo); // expect: true
assertEq(Foo == Bar, false); // expect: false
assertEq(Bar == Foo, false); // expect: false
assert(Bar == Bar); // expect: true

assertEq(Foo == "Foo", false); // expect: false
assertEq(Foo == nil, false);   // expect: false
assertEq(Foo == 123, false);   // expect: false
assertEq(Foo == true, false);  // expect: false
