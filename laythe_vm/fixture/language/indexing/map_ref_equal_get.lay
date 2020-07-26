class Example {}
let e1 = Example();
let e2 = Example();

let map = {
  "key1": "value1",
  "key2": "value2",
  "key3": "value3",
  e1: "value4",
  e2: "value5",
};

assertEq(map["key1"], "value1");
assertEq(map["key2"], "value2");
assertEq(map["key3"], "value3");
assertEq(map[e1], "value4");
assertEq(map[e2], "value5");