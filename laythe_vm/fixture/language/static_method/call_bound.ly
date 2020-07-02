class A {
  static ten() { 
    return 10;
  }
}

let ten = A.ten;
assertEq(ten(), 10);