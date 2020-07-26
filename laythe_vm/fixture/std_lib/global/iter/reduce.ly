assertEq(15, [1, 2, 3, 4, 5]
  .iter()
  .reduce(0, |acc, curr| acc + curr)
);
assertEq("hello world!", ["hello", " ", "world", "!"]
  .iter()
  .reduce("", |acc, curr| acc + curr)
);