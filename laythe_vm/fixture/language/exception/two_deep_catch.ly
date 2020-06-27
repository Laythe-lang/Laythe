fn outer(something) {
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