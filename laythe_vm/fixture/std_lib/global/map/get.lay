fn example() {}

let map = {
  "cat": true,
  "dog": true,
  "parrot": true,
  "rabbit": true,
  example: example,
};

assert(map.get("cat"));
assert(map.get("dog"));
assert(map.get("parrot"));
assert(map.get("rabbit"));
assertEq(map.get(example), example);