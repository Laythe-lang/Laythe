// Note: These tests implicitly depend on ints being truthy.

// Return the first true argument.
assertEq(1 or true, 1); // expect: 1
assertEq(false or 1, 1); // expect: 1
assertEq(false or false or true, true); // expect: true

// Return the last argument if all are false.
assertEq(false or false, false); // expect: false
assertEq(false or false or false, false); // expect: false

// Short-circuit at the first true argument.
let a = "before";
let b = "before";
(a = false) or
    (b = true) or
    (a = "bad");
assertEq(a, false); // expect: false
assertEq(b, true); // expect: true
