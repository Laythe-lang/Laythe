{
  var isEven = |n| {
    if (n == 0) return true;
    return isOdd(n - 1); // expect runtime error: Undefined variable 'isOdd'.
  }

  var isOdd = |n| {
    return isEven(n - 1);
  }

  isEven(4);
}