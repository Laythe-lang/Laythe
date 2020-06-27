{
  let i = "before";

  for (let i in [0]) {

  }

  // New variable is in inner scope.
  for (let i in [0]) {
    assertEq(i, 0); // expect: 0

    // Loop body is in second inner scope.
    let i = -1;
    assertEq(i, -1); // expect: -1
  }
}

{
  // New variable shadows outer variable.
  for (let i in [0]) {}

  // Goes out of scope after loop.
  let i = "after";
  assertEq(i, "after"); // expect: after

  // Can reuse an existing variable.
  for (let i in [0]) {
    assertEq(i, 0); // expect: 0
  }
}
