let list = [1, 2, 3, 4, "5"];
assertEq(list[0], 1);

list[0] = false;
assertEq(list[0], false);

list[3] = "dog";
assertEq(list[3], "dog");