let a = "a";
let b = "b";
let c = "c";

// Assignment is right-associative.
a = b = c;
assertEq(a, c); // expect: c
assertEq(b, c); // expect: c
assertEq(c, c); // expect: c
