class Foo {
  static method() {
    print(method); // expect runtime error: Undefined variable 'method'.
  }
}

Foo.method();
