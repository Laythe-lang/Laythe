assertNe(true, false);
assertNe(10, 3);
assertNe("cat", "cats");

fn a() {}
fn b() {}
assertNe(a, b);

assertNe(true, 10);
assertNe(b, "cat");
assertNe(3, a);
assertNe("cats", 20);