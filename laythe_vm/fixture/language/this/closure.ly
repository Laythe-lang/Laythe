class Foo {
  getClosure() {
    fn closure() {
      return self.toString();
    }
    return closure;
  }

  toString() { return "Foo"; }
}

let closure = Foo().getClosure();
print closure(); // expect: Foo
