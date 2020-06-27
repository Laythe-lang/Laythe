let x = [1, 2, 3];

assertEq(x.pop(), 3);
assertEq(x.pop(), 2);
assertEq(x.pop(), 1);
assertEq(x.pop(), nil);