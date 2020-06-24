// False and nil are false.
assertEq(false and "bad", false); // expect: false
assertEq(nil and "bad", nil); // expect: nil

// Everything else is true.
assertEq(true and "ok", "ok"); // expect: ok
assertEq(0 and "ok", "ok"); // expect: ok
assertEq("" and "ok", "ok"); // expect: ok
