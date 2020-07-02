let l1 = List.collect(10.times());
let l2 = List.collect([1, 2, 3].iter());
let l3 = List.collect({ "dude": 1, "bro": 2, "cat": 3 }.iter());

assertEq(l1.size(), 10);
assertEq(l2.size(), 3);
assertEq(l3.size(), 3);