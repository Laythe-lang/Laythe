let list = [];

try {
  list.insert(true, 10);
  assert(false);
} catch {
  assert(true);
}

