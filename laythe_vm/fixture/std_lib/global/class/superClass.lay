class A {}
class B < A {}

assertEq(B.superClass(), A);
assertEq(B.superClass().superClass(), Object);

assertEq(A.superClass(), Object);

assertEq(List.superClass(), Object);
assertEq(Object.superClass(), nil);