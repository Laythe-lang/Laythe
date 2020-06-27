fn caller(g) {
  g();
  // g should be a function, not nil.
  assertEq(g == nil, false); // expect: false
}

fn callCaller() {
  let capturedlet = "before";
  let a = "a";

  fn f() {
    // Commenting the next line out prevents the bug!
    capturedlet = "after";

    // Returning anything also fixes it, even nil:
    //return nil;
  }

  caller(f);
}

callCaller();
