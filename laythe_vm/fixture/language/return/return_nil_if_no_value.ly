fn f() {
  return;
  print("bad");
}

assertEq(f(), nil); // expect: nil
