fn f() {
  for (;;) {
    let i = "i";
    fn g() { print i; }
    return g;
  }
}

let h = f();
h(); // expect: i
