class Example {
  test1() {}
  test2() {}
}

var example = Example();
assertEq(example.test1.name(), "test1");
assertEq(example.test2.name(), "test2");

var x1 = example.test1;
var x2 = example.test2;
assertEq(x1.name(), "test1");
assertEq(x2.name(), "test2");

assertEq([].push.name(), "push");
assertEq([].pop.name(), "pop");

var x3 = [].push;
var x4 = [].pop;
assertEq(x3.name(), "push");
assertEq(x4.name(), "pop");