var a = [1, 2];
a.push(nil);
assertEq(a.size(), 3);
assertEq(a.str(), "[1, 2, nil]");

a.push("cat", -5);
assertEq(a.size(), 5);
assertEq(a.str(), "[1, 2, nil, 'cat', -5]");