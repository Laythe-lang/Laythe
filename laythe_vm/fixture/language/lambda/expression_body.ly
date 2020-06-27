assertEq((|x| x)(10), 10);

let result = "result";
assertEq((|| result)(), "result");
assertEq((|| || || result)()()(), "result");