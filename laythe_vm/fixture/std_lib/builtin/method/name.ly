class Example {
  test1() {}
  test2() {}
}

let example = Example();
assertEq(example.test1.name(), "test1");
assertEq(example.test2.name(), "test2");

let x1 = example.test1;
let x2 = example.test2;
assertEq(x1.name(), "test1");
assertEq(x2.name(), "test2");

assertEq([].push.name(), "push");
assertEq([].pop.name(), "pop");

let x3 = [].push;
let x4 = [].pop;
assertEq(x3.name(), "push");
assertEq(x4.name(), "pop");