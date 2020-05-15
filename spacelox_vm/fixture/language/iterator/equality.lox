var list = [1, 2, 3];
var map = {
  "key1": 10,
  "key2": false,
};

var listIter1 = list.iter();
var listIter2 = list.iter();

var mapIter1 = map.iter();
var mapIter2 = map.iter();

assertEq(listIter1, listIter1);
assertEq(listIter2, listIter2);
assertNe(listIter1, listIter2);

assertEq(mapIter1, mapIter1);
assertEq(mapIter2, mapIter2);
assertNe(mapIter1, mapIter2);