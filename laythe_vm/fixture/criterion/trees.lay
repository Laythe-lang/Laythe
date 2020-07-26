class Tree {
  init(depth) {
    self.depth = depth;
    if (depth > 0) {
      self.a = Tree(depth - 1);
      self.b = Tree(depth - 1);
      self.c = Tree(depth - 1);
      self.d = Tree(depth - 1);
      self.e = Tree(depth - 1);
    }
  }

  walk() {
    if (self.depth == 0) return 0;
    return self.depth 
        + self.a.walk()
        + self.b.walk()
        + self.c.walk()
        + self.d.walk()
        + self.e.walk();
  }
}

let tree = Tree(5);

for (let i = 0; i < 3; i = i + 1) {
  tree.walk();
}
