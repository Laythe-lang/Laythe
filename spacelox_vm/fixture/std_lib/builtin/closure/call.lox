fun inner(value) {
  return value * 2;
}

fun outer(name) {
  print name;
  return inner;
}

assertEq(outer.call("hi"), inner);
assertEq(inner.call(10), 20);
assertEq(outer.call("hi").call(5), 10);
