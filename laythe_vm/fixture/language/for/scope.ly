{
  let i = "before";

  // New variable is in inner scope.
  for (let i = 0; i < 1; i = i + 1) {
    assertEq(i, 0); // expect: 0

    // Loop body is in second inner scope.
    let i = -1;
    assertEq(i, -1); // expect: -1
  }
}

{
  // New variable shadows outer variable.
  for (let i = 0; i > 0; i = i + 1) {}

  // Goes out of scope after loop.
  let i = "after";
  assertEq(i, "after"); // expect: after

  // Can reuse an existing variable.
  for (i = 0; i < 1; i = i + 1) {
    assertEq(i, 0); // expect: 0
  }
}
