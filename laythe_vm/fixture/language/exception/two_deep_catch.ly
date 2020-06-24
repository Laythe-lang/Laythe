fun outer(something) {
  thrower();
}

fun thrower() {
  [][1];
}

try {
  outer();
  assert(false);
} catch {
  assert(true);
}