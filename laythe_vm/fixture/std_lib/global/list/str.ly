assertEq([1, 2, 3].str(), "[1, 2, 3]");
assertEq([false, nil, "hi"].str(), "[false, nil, 'hi']");
assertEq([[1, 2], { "cat": "dog" }].str(), "[[1, 2], { 'cat': 'dog' }]");