class Example {
  test1(a, b) {
    return [a, b];
  }

  test2(test) {
    return test;
  }
}

let map = {};
let example = Example();

assertEq(example.test1.call(10, 5)[1], 5);
assertEq(example.test2.call("test"), "test");

let x1 = example.test1;
let x2 = example.test2;

assertEq(x1.call(10, 5)[1], 5);
assertEq(x2.call("test"), "test");

assertEq(map.insert.call(1, 1), nil);
assertEq(map.insert.call(1, false), 1);

let x3 = map.insert;

assertEq(x3.call(1, "test"), false);
assertEq(x3.call(1, "dude"), "test");