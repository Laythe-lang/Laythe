class Tree {
  init(item, depth) {
    this.item = item;
    this.depth = depth;
    if (depth > 0) {
      var item2 = item + item;
      depth = depth - 1;
      this.left = Tree(item2 - 1, depth);
      this.right = Tree(item2, depth);
    } else {
      this.left = nil;
      this.right = nil;
    }
  }

  check() {
    if (this.left == nil) {
      return this.item;
    }

    return this.item + this.left.check() - this.right.check();
  }
}

var minDepth = 3;
var maxDepth = 6;
var stretchDepth = maxDepth + 1;

Tree(0, stretchDepth).check();

var longLivedTree = Tree(0, maxDepth);

// iterations = 2 ** maxDepth
var iterations = 1;
var d = 0;
while (d < maxDepth) {
  iterations = iterations * 2;
  d = d + 1;
}

var depth = minDepth;
while (depth < stretchDepth) {
  var check = 0;
  var i = 1;
  while (i <= iterations) {
    check = check + Tree(i, depth).check() + Tree(-i, depth).check();
    i = i + 1;
  }

  iterations = iterations / 4;
  depth = depth + 2;
}