fun f() {
  while (true) {
    var i = "i";
    fun g() { return i; }
    return g;
  }
}

var h = f();
assertEq(h(), "i"); // expect: i
