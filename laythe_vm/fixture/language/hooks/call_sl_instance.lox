class A {
  example(parameter) {
    return parameter;
  }
}

var a = A();
var example = a.example;
assertEq(10, a.example.call(10));
assertEq("john", a.example.call("john"));
assertEq(example, a.example.call(example));