assertEq(nil != nil, false); // expect: false

assertEq(true != true, false); // expect: false
assert(true != false); // expect: true

assertEq(1 != 1, false); // expect: false
assert(1 != 2); // expect: true

assertEq("str" != "str", false); // expect: false
assert("str" != "ing"); // expect: true

assert(nil != false); // expect: true
assert(false != 0); // expect: true
assert(0 != "0"); // expect: true
