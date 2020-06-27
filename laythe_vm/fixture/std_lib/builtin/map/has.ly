let map = {
  1: 2,
  2: 3,
  4: 5,
  false: nil
};

assert(map.has(1));
assert(map.has(2));
assert(map.has(4));
assert(map.has(false));

assert(!map.has(true));
assert(!map.has(15));
assert(!map.has("sup"));