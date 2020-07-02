// self benchmark stresses both field and method lookup.

class Foo {
  init() {
    self.field0 = 1;
    self.field1 = 1;
    self.field2 = 1;
    self.field3 = 1;
    self.field4 = 1;
    self.field5 = 1;
    self.field6 = 1;
    self.field7 = 1;
    self.field8 = 1;
    self.field9 = 1;
    self.field10 = 1;
    self.field11 = 1;
    self.field12 = 1;
    self.field13 = 1;
    self.field14 = 1;
    self.field15 = 1;
    self.field16 = 1;
    self.field17 = 1;
    self.field18 = 1;
    self.field19 = 1;
    self.field20 = 1;
    self.field21 = 1;
    self.field22 = 1;
    self.field23 = 1;
    self.field24 = 1;
    self.field25 = 1;
    self.field26 = 1;
    self.field27 = 1;
    self.field28 = 1;
    self.field29 = 1;
  }

  method0() { return self.field0; }
  method1() { return self.field1; }
  method2() { return self.field2; }
  method3() { return self.field3; }
  method4() { return self.field4; }
  method5() { return self.field5; }
  method6() { return self.field6; }
  method7() { return self.field7; }
  method8() { return self.field8; }
  method9() { return self.field9; }
  method10() { return self.field10; }
  method11() { return self.field11; }
  method12() { return self.field12; }
  method13() { return self.field13; }
  method14() { return self.field14; }
  method15() { return self.field15; }
  method16() { return self.field16; }
  method17() { return self.field17; }
  method18() { return self.field18; }
  method19() { return self.field19; }
  method20() { return self.field20; }
  method21() { return self.field21; }
  method22() { return self.field22; }
  method23() { return self.field23; }
  method24() { return self.field24; }
  method25() { return self.field25; }
  method26() { return self.field26; }
  method27() { return self.field27; }
  method28() { return self.field28; }
  method29() { return self.field29; }
}

let foo = Foo();
let start = clock();
let i = 0;
while (i < 500000) {
  foo.method0();
  foo.method1();
  foo.method2();
  foo.method3();
  foo.method4();
  foo.method5();
  foo.method6();
  foo.method7();
  foo.method8();
  foo.method9();
  foo.method10();
  foo.method11();
  foo.method12();
  foo.method13();
  foo.method14();
  foo.method15();
  foo.method16();
  foo.method17();
  foo.method18();
  foo.method19();
  foo.method20();
  foo.method21();
  foo.method22();
  foo.method23();
  foo.method24();
  foo.method25();
  foo.method26();
  foo.method27();
  foo.method28();
  foo.method29();
  i = i + 1;
}

print clock() - start;
