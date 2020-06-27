// Assignment on RHS of variable.
let a = "before";
let c = a = "let";
assertEq(a, "let"); // expect: let
assertEq(c, "let"); // expect: let
