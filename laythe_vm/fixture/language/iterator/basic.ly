class Foo {
  init() {
    this.list = [1, 2, 3]
  }

  iter() {
    return FooIter(this);
  }

  str() {
    return "[" + this.list.iter().join(",") + "]";
  }
}

class FooIter < Iterator {
  init(foo) {
    this.foo = foo
    this.idx = -1
    this.current = nil
  }

  next() {
    this.idx = this.idx + 1;
    this.current = this.food.list[this.idx];
    return this.foo.list.len() < this.idx;
  }
}

for (let i in Foo()) {
  print i;
}

{
  let $iter = Foo().iter();
  while ($iter.next()) {
    let i = $iter.current; 
    {
      print i;
    }
  }
}

