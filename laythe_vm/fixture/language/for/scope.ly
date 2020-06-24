{
  var i = "before";

  // New variable is in inner scope.
  for (var i = 0; i < 1; i = i + 1) {
    assertEq(i, 0); // expect: 0

    // Loop body is in second inner scope.
    var i = -1;
    assertEq(i, -1); // expect: -1
  }
}

{
  // New variable shadows outer variable.
  for (var i = 0; i > 0; i = i + 1) {}

  // Goes out of scope after loop.
  var i = "after";
  assertEq(i, "after"); // expect: after

  // Can reuse an existing variable.
  for (i = 0; i < 1; i = i + 1) {
    assertEq(i, 0); // expect: 0
  }
}