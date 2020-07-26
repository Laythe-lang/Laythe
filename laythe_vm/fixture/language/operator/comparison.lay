assert(1 < 2);    // expect: true
assertEq(2 < 2, false);    // expect: false
assertEq(2 < 1, false);    // expect: false

assert(1 <= 2);    // expect: true
assert(2 <= 2);    // expect: true
assertEq(2 <= 1, false);    // expect: false

assertEq(1 > 2, false);    // expect: false
assertEq(2 > 2, false);    // expect: false
assert(2 > 1);    // expect: true

assertEq(1 >= 2, false);    // expect: false
assert(2 >= 2);    // expect: true
assert(2 >= 1);    // expect: true

// Zero and negative zero compare the same.
assertEq(0 < -0, false); // expect: false
assertEq(-0 < 0, false); // expect: false
assertEq(0 > -0, false); // expect: false
assertEq(-0 > 0, false); // expect: false
assert(0 <= -0); // expect: true
assert(-0 <= 0); // expect: true
assert(0 >= -0); // expect: true
assert(-0 >= 0); // expect: true
