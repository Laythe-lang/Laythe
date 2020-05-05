var f1;
var f2;
var f3;

var i = 1;
while (i < 4) {
  var j = i;
  fun f() { return j; }

  if (j == 1) f1 = f;
  else if (j == 2) f2 = f;
  else f3 = f;

  i = i + 1;
}

assertEq(f1(), 1); // expect: 1
assertEq(f2(), 2); // expect: 2
assertEq(f3(), 3); // expect: 3
