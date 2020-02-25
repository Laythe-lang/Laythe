var f;

fun f1() {
  var a = "a";
  fun f2() {
    var b = "b";
    fun f3() {
      var c = "c";
      fun f4() {
        assertEq(a, "a");
        assertEq(b, "b");
        assertEq(c, "c");
      }
      f = f4;
    }
    f3();
  }
  f2();
}
f1();

f();
// expect: a
// expect: b
// expect: c
