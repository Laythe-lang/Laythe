assertEq(!true, false);     // expect: false
assert(!false);    // expect: true
assert(!!true);    // expect: true

assertEq(!123, false);      // expect: false
assertEq(!0, false);        // expect: false

assert(!nil);     // expect: true

assertEq(!"", false);       // expect: false

fn foo() {}
assertEq(!foo, false);      // expect: false
