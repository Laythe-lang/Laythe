class Zoo {
  init() {
    self.aarletk  = 1;
    self.baboon   = 1;
    self.cat      = 1;
    self.donkey   = 1;
    self.elephant = 1;
    self.fox      = 1;
  }
  ant()    { return self.aarletk; }
  banana() { return self.baboon; }
  tuna()   { return self.cat; }
  hay()    { return self.donkey; }
  grass()  { return self.elephant; }
  mouse()  { return self.fox; }
}

let zoo = Zoo();
let sum = 0;
let start = clock();
while (sum < 10000000) {
  sum = sum + zoo.ant()
            + zoo.banana()
            + zoo.tuna()
            + zoo.hay()
            + zoo.grass()
            + zoo.mouse();
}

print(sum);
print(clock() - start);
