fn outer() {
  thrower();
}

fn thrower() {
  [][1];
}

try {
  outer();
  assert(false);
} catch {
  assert(true);
}