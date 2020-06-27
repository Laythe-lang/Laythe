let list = [];
try {
  list.insert();
  assert(false);
} catch {
  assert(true);
}

try {
  list.pop(10);
  assert(false);
} catch {
  assert(true);
}