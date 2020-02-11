var f;
var g;

{
  var local = "local";
  fun f_() {
    assertEq(local, "local");
    local = "after f";
    assertEq(local, "after f");
  }
  f = f_;

  fun g_() {
    assertEq(local, "after f");
    local = "after g";
    assertEq(local, "after g");
  }
  g = g_;
}

f();
// expect: local
// expect: after f

g();
// expect: after f
// expect: after g
