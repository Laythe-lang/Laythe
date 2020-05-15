var isEven = |n| {
  if (n == 0) return true;
  return isOdd(n - 1);
};

var isOdd = |n| {
  return isEven(n - 1);
};

print isEven(4); // expect: true
print isOdd(3); // expect: true
