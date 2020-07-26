let map = {
  "a": 1,
  "b": 2,
  "c": 3,
  "d": 4,
  "f": 5,
};

let newMap = {};
for (let kvp in map) {
  newMap[kvp[0]] = kvp[1];
}

assertEq(newMap.size(), map.size());
for (let kvp in newMap) {
  assertEq(kvp[1], map[kvp[0]]);
}