fun greeter(name) {
  print "hi! " + name.str();
}

var x = ["cat", "dog", greeter, false, [10]];
var iter = x.iter();

assertEq(iter.current, nil);

assertEq(iter.next(), true);
assertEq(iter.current, "cat");

assertEq(iter.next(), true);
assertEq(iter.current, "dog");

assertEq(iter.next(), true);
assertEq(iter.current, greeter);

assertEq(iter.next(), true);
assertEq(iter.current, false);

assertEq(iter.next(), true);
assertEq(iter.current[0], 10);

assertEq(iter.next(), false);