# Laythe

A programming language originally based on the 2nd book of [Crafting Interpreters](https://craftinginterpreters.com/). See git tag [v0.1.0](https://github.com/Laythe-lang/Laythe/releases/tag/v0.1.0) for a fully compliant lox implementations. Since v0.1.0 I've continued adding features and exploring what's possible to learn and do in the PL world. 

# Getting Started

Laythe is built in rust and as such uses the typical set of cargo commands for building, testing, running and benchmarks. If you don't have cargo on your system it us recommended you us [rustup](https://rustup.rs/) to get setup.

### Build debug
```
cargo build
```

### Build Release
```
cargo build --release
```

### Run Test Suite
```
cargo test
```

### Run Benchmark Suite
```
cargo bench
```

### Run Repl
```
cargo run [--release] 
```

### Run a File
```
cargo run [--release] [filepath]
```

# Notable differences from Lox

At this point laythe should probably be considered a cousin to Lox. Primarily there are extensions but a few features have been removed as well.


## Additions

### Built in Classes
Laythe now has machinery to give all types methods. Some simple examples include `.str()` methods to get a string representation of each type.

```laythe
laythe:> let x = true;
laythe:> x.str()
'true'
```

### Lambdas
There are now function expressions. This was actually a very minimal change to enable this as it reuses almost all the the function machinery.

```laythe
laythe:> let func = |x| x * 2;
laythe:> let withBody |name| { print("hi! " + name); };
laythe:> func(5)
10
laythe:> withBody("John")
hi! john
```

### Static Methods
Classes now support static methods using the `static` keyword.

```laythe
class WithStatic {
  static example() { 
    return 'example';
  }
}

laythe:> WithStatic.example()
'example'
```

### New Collection Types
Laythe now has lists and maps as part of the language both supporting literals.

```laythe
laythe:> let list = [1, false, nil, 3, clock];
laythe:> let map = { 'key1', 10, 'key2': false, 15: nil };
laythe:> list[2];
nil
laythe:> map[false];
15
```
Map support all types with objects supported by reference equality. Since strings are interned this gives the desire value comparison results most would expect.

### Type Annotations
Laythe now supports a basic set of type annotations. Long term this will eventually turn into optional typing, but the parser will now ingest some Typescript like annotations.

```laythe
// let type def
let b: string = "example"

// essentially equivalent
let f: (a: string) -> number = |a| Number.parse(a);
let f = |a: string| -> number Number.parse(a);

// function signature
fn adder(x: number, y: number) -> number { 
  return x + y
}

// class type param
class Foo<T> {
  // field declaration
  bar: T;

  init(bar: T) { 
    self.bar = bar;
  }
}

// type def
type Holder<T> = Foo<T>[];

// also called interface in TS
trait NumHolder {
  holder: Holder<number>;
}
```

### Implicit Return
Like ruby Laythe now supports implicit returns in a few cases. These included lambda, functions, methods and static methods

```laythe
laythe:> let f1 = || { 10 };
laythe:> fn f2() { 20 }
laythe:> print(f1());
// 10
laythe:> print(f2());
// 10
```

### String Interpolation
Laythe largely borrowed Javascript's string interpolation syntax. The primarily difference is we don't introduce a new quoting character 

```laythe
class Person {
  init(first, last, age) {
    self.first = first;
    self.last = last;
  }

  str() {
    "${self.first} ${self.last} is ${} years old"
    // this is largely equivalent to
    // self.first.str() + " " + self.last.str() + " is " + self.age.str() + " years old"
  }
}

print(Person("Jim", "Smith", 29).str())
// "Jim Smith is 29 years old"
```

## Modified

### Gc
The gc is setup quite differently from clox mostly because dealing with lifetimes in the base implementation was nearly impossible. Instead objects as of 5/8/2020 Act as a sort of smart pointer into a `Vec<Box<Value>>`. The gc takes a context to specify the active object roots to sweep. Gc is also explicitly turned off during compilation as values are only freed during the main loop.

### Grammar
The grammar is now different in a few ways from lox. These are changes in additions to type annotations.

```
// variable declaration
var lox = 10;
let laythe = 10;

// function declaration
fun loxFun (a, b, c) { // lox
  return a + b + c
}

fn laytheFnExpressionBody (a, b, c) { a + b + c }
fn laytheFnBlockBody (a, b, c) {
  return a + b + c
}

// classes
class LoxClass {
  init(field1) { 
    this.field1 = field1
  }
}

class LaytheClass {
  init(field1) { 
    self.field1 = field1
  }
}

// lox control flow

// has parens around condition, any statement for body
if (10 < 3) {
  var y = 10;
} else print(x);

// c style look
for (var x = 0; x < 5; x = x + 1) { }

while (true) print("hi")

// laythe control flow

// no parens, body must be block
if 10 < 3 {
  let y = 10;
} else { 
  print(x)
}

// range style look
for i in 100.times() {

}

while true { }
```

### Performance

Run the lox benchmark suite for laythe and clox laythe averages 73% of the speed of clox. In general this seems to be focused around hashing speed and the overhead of the main interpreter loop. This is probably best seen in `equality.lox` and `fib.lox` where equality is likely the simplest in terms of execution is only 80% as clox while fib which exercises both global lookups for fib and function calls only achieves 63% of clox. The benchmark Laythe edges out clox is in binary trees. This is likely do to the class caching the init function and inline caching property and method calls. 

Running the original benchmark suite on a 2015 dell xps we have.

|benchmark|clox|Laythe|relative speed|notes|
|--|--|--|--|--|
|binary_tress.lox|total: 2.58032|total: 2.35423|1.1|This likely due to the small number of properties and the init optimization. We essentially avoid hashing altogether in this bench|
|equality.lox|loop: 2.54958 elapsed: 2.08519|loop: 2.647906 elapsed: 3.131277|0.80|Similar to above we have essentially zero hashing and again perform pretty equal to clox|
|fib.lox|total: 1.33588|total: 2.08769|0.63|Here we have some hashing from the `fib` lookup but even making this local shows a difference. It appears there is still some performance difference in function calling here|
|instantiation.lox|total: 0.794824|total: 1.50812|0.53|Again localizing this gives a decent speedup but hashing and function calls still seem to slow Laythe down|
|invocation.lox|total: 0.419431|total: 0.814637|0.52|Now the hashing speed difference is quite apparent|
|method_call.lox|total: 0.26644|total: 0.341227|0.65|Same as above|
|properties.lox|total: 0.645307|total: 0.937192|0.69|Same as above|
|trees.lox|total: 3.09063|total: 3.387378|0.91|Same as above|
|zoo.lox|total: 0.495144|total: 0.709942|0.70|Same as above|

## Future Ideas

These are some features / changes I'm considering adding to the language.

### Gradual typing
I'm a huge fan of Typescript and believe it's gradual typing approach is quite fantastic. I'd like to incorporate a similar system here. Beyond just the normal type errors I'd like to have more of a runtype check at typed and untyped boundaries, at least enable that with a flag.

```laythe
// Possible type syntax
fn unTyped(x) {
  return typed(x)
}

fn typed(x: number) -> string {
  return typedInner(x);
}

fn typedInner(x: number) -> string {
  return x.str();
}

> unTyped(10);
'10'
> unTyped(20);
'20'
≥ unTyped('15')
typeError 'typed' expected number received string'
```

Here I would think it could be useful if laythe automatically injected some runtime type checking before it's called in `unTyped`. The runtime check could then be emitted inside of `typed` as we know the value comes from a trust checked `typedInner`. I think this approach would really help extend the usefulness of the typescript model. I know outside of the strict flags in ts that you know the wrong type might still slip through. Here I think we can inject runtime checks to assert call boundaries are truly what the claim to be.

## Placeholder partial application
I think placeholder partial application could really help prompt more first class function patterns. I think this is a small piece of syntactic sugar that could really cut down on a lot of boiler plate

```
fn sum(a, b) { a + b }
fn eq(a, b) { a == b }

// possible today
let increment1 = |x| sum(1, x);

// idea
let increment2 = sum(1, _);

print(increment1(10));
// 11
print(increment2(10));
// 12

// I think the nicer usecase is inline

// take a list of number increment by 2 and collect into
// a new list
[1, 2, 3].iter().map(sum(_, 2)).into(List.collect);
```

## Fibers
They are all the rage, and I pattern I haven't used before but they feel like they several solve different problems for single threaded concurrency. Generally it seems fibers can be both used as generators and to handle async programming. I'm not exactly sure what I want to go with here but generally I think this is the approach I want to take.