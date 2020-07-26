fn example() {}

let map = {
  1: "dude",
  "cat": true,
  "dog": true,
  nil: true,
  "rabbit": true,
  example: example,
};

assertEq(map.set(1, "bro"), "dude");
assertEq(map.set(false, 25), nil);
assertEq(map.set(example, || true), example);