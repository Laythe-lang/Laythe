fn f1(x1, x2, x3) {
  print(x1);
  print(x2);
  print(x3);
}

fn f2(x) {
  print(x);
}

assertEq(f1.size(), 3);
assertEq(f2.size(), 1);