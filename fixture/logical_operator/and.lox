// Note: These tests implicitly depend on ints being truthy.

// Return the first non-true argument.
assertEq(false and 1, false); // expect: false
assertEq(true and 1, 1); // expect: 1
assertEq(1 and 2 and false, false); // expect: false

// Return the last argument if all are true.
assertEq(1 and true, true); // expect: true
assertEq(1 and 2 and 3, 3); // expect: 3

// Short-circuit at the first false argument.
var a = "before";
var b = "before";
(a = true) and
    (b = false) and
    (a = "bad");
assertEq(a, true); // expect: true
assertEq(b, false); // expect: false
