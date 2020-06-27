assertEq([].size(), 0);
assertEq([1, 2, 3].size(), 3);

let x1 = [];
let x2 = [1, 2, 3];
assertEq(x1.size(), 0);
assertEq(x2.size(), 3);

x1.push(1, 4);
x2.remove(2);
assertEq(x1.size(), 2);
assertEq(x2.size(), 2);