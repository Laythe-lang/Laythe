fn f() {
  for (let x in [1]) {
    let i = "i";
    fn g() { print(i); }
    return g;
  }
}

let h = f();
h(); // expect: i
