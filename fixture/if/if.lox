// Evaluate the 'then' expression if the condition is true.
if (true) assert(true); // expect: good
if (false) assert(false);

// Allow block body.
if (true) { assert(true); } // expect: block

// Assignment in if condition.
var a = false;
if (a = true) assertEq(a, true); // expect: true
