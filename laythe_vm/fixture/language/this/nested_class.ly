class Outer {
  method() {
    print(self); // expect: Outer instance

    fn f() {
      print(self); // expect: Outer instance

      class Inner {
        method() {
          print(self); // expect: Inner instance
        }
      }

      Inner().method();
    }
    f();
  }
}

Outer().method();
