class A {
  example(parameter) {
    return parameter;
  }
}

let a = A();
let example = a.example;
assertEq(10, a.example.call(10));
assertEq("john", a.example.call("john"));
assertEq(example, a.example.call(example));