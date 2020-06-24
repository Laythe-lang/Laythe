fun first() {}
fun second() {}

var assigned = first;

assertEq(first.name(), "first");
assertEq(second.name(), "second");
assertEq(assigned.name(), "first");