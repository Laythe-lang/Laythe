# Laythe

A gradual typed, object oriented, fiber based concurrent scripting language. Laythe's goal is to
seemlessly transistion from a single file untyped script to fully typed and fast production project.
This is planned to be achieved by including a Typescript like gradual compiler that can eventually
be used to JIT faster code.

The language was originally based on the 2nd book of [Crafting Interpreters](https://craftinginterpreters.com/). See git tag [v0.1.0](https://github.com/Laythe-lang/Laythe/releases/tag/v0.1.0) for a fully compliant lox implementations.

## Getting Started

Laythe is built in rust and as such uses the typical set of cargo commands for building, testing, running and benchmarks. If you don't have cargo on your system it us recommended you us [rustup](https://rustup.rs/) to get setup.

### Build debug

```bash
cargo build
```

### Build Release

```bash
cargo build --release
```

### Run Test Suite

```bash
cargo test
```

### Run Benchmark Suite

```basy
cargo bench
```

### Run Repl

```bash
cargo run [--release]
```

### Run a File

```bash
cargo run [--release] [filepath]
```

## Language Overview

As stated at the top Laythe adjectives are

* Gradually Typed
* Object Oriented
* Fiber (Coroutine) based concurrency

### Basic Types

Today laythe supports a small number of basic types.

```laythe
// ---- boolean ----
true;
false;

// ---- nil ----
nil;

// ---- numbers ----
// single number type IEEE 64 bit float point
// this may change in the future but for now al
10.5;
3;

// ---- strings ----
"this is a string";

// with interpolation
"this is a string with interpolation ${10}";
```

### Collection

Laythe support two collection types lists and maps

```laythe
// ---- lists ----

// list can be homogeneous
let l1 = [1, 2, 3];
print(l1[2]);
laythe:> 3

print(l1[-1]);
laythe:> 3

// or heterogeous
let l2 = ['foo', nil, []];
print(l2[0]);
laythe:> 'foo'

// ---- maps ----
// maps which can use any value as key and value
let list = [];
let m = {
  1: 2,
  nil: 'some string',
  'some string': false,
  list: nil,
};

print(m[list]);
laythe:> nil
```

### Function

Functions in Laythe are first class values and support closure capture.

```laythe
// ---- function statement ----
fn doubler(x) {
  x * 2
}

// ---- function expressions / lambdas ----

// with expression body
|x| x * 2;

// with block body
|x| { x * 2 };

// ---- recursion ----
fn fib(n) {
  if n < 2 { return n; }
  fib(n - 2) + fib(n - 1)
}

// ---- closure capture ----

let name = "John";
fn greeter() {
  print("Hello ${name}");
}

// ---- first class values ----

fn halver(x) { x / 2 }

fn invoker(fn, val) { fn(val) }

print(invoker(halfer, 10));
laythe:> 5
```

### Classes

```laythe
// ---- classes ----

// class with no explicit super class no initializer
class A {}

// create an instance by call the class
let a = A();

class B {
  // constructor
  init(x) {
    self.x = x
    self.z = 'some string';
  }

  // instance method
  bar() {
    self.x
  }

  // static method
  static baz() {
    print("hello from B")
  }
}

// calling the class class init if present
let b = B('cat');

// call an instance method
print(b.bar());
laythe:> 'cat'

// call a class method
B.baz();
laythe:> "hello from B"

// access a property
print(b.x);
laythe:> "cat"

// subclass B as C
class C : B {
  init(x, y) {
    super.init(x)
    self.y = y;
  }

  bar() {
    // call super method bar
    super.bar() + 3
  }

  foo() {
    'y is ${self.y}'
  }
}

let c = C(10, nil);

print(c.bar());
laythe:> 13

print(c.foo());
laythe:> 'y is nil';
```

### Control Flow

Laythe has the faily standard set of control flow you'd find in an scripting language

```laythe

// ---- conditional flow ----

if 10 > 3 {
  // predicate met branch
}

if 10 == nil {
  // predicate met branch
} else {
  // alternative branch optional
}

// condtional expressions (ternary)
let y = 10 > 3
  ? 'cat'
  : 'dog';


// ---- while loop ----
let x = 0;
while x < 10 {
  x += 1;
}

print(x)
laythe:> 10

// ---- for loop ----
let sum = 0;

// anything the implmenents the iterator interface
for y in [1, 2, 3] {
  sum += y;
}

print(sum);
laythe:> 6
```

### Type Annotations

Laythe now supports a basic set of type annotations. Long term this will eventually turn into gradual typing,
but the parser will now ingest some Typescript like annotations.

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

### Fibers and Channels

Laythe concurrent model builds around fibers and channels. Fibers are known by a number of terms such as green threads, and stackfull coroutines. These are separate and lightweight units of execution that in many ways act like an os thread. Laythe currently only runs a single fiber at a time switch between fibers on channel send's and receives.

```laythe
fn printer(ch) {
  print(<- ch)
}

fn writer(ch) {
  let strings = ["foo", "bar", "baz"];

  for i in 100.times() {
    for string in strings {
      ch <- string;
    }
  }
}

// create a buffered channel that can hold 10 elements
let ch = chan(10);

// "launch" the printer function as a fiber passing in the channel
launch printer(ch)

// send 3 strings to the channel to eventually be written
ch <- "foo";
ch <- "bar";
ch <- "baz";

// send many string to the channel to eventually be written
writer(ch);
```

## Performance

Run the lox benchmark suite for laythe and clox laythe averages 73% of the speed of clox. In general this seems to be focused around hashing speed and the overhead of the main interpreter loop. This is probably best seen in `equality.lox` and `fib.lox` where equality is likely the simplest in terms of execution is only 80% as clox while fib which exercises both global lookups for fib and function calls only achieves 63% of clox. The benchmark Laythe edges out clox is in binary trees. This is likely do to the class caching the init function and inline caching property and method calls.

Running the original benchmark suite on a 2015 dell xps we have.

|benchmark|clox|Laythe|relative speed|notes|
|--|--|--|--|--|
|binary_tress.lox|total: 2.58032|total: 2.07827|1.1|We've optimized instance to allocate properties inline and have an init optimization that benefits speed|
|equality.lox|loop: 2.54958 elapsed: 2.08519|loop: 2.53177 elapsed: 2.52173|0.80|Similar to above we have essentially zero hashing and again perform pretty equal to clox|
|fib.lox|total: 1.33588|total: 2.09226|0.63|Here we have some hashing from the `fib` lookup but even making this local shows a difference. It appears there is still some performance difference in function calling here|
|instantiation.lox|total: 0.794824|total: 1.58841|0.53|Again localizing this gives a decent speedup but hashing and function calls still seem to slow Laythe down|
|invocation.lox|total: 0.419431|total: 0.782444|0.52|Now the hashing speed difference is quite apparent|
|method_call.lox|total: 0.26644|total: 0.326959|0.65|Same as above|
|properties.lox|total: 0.645307|total: 0.889522|0.69|Same as above|
|trees.lox|total: 3.09063|total: 3.387378|0.91|Same as above|
|zoo.lox|total: 0.495144|total: 0.680576|0.70|Same as above|

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

```laythe
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

## JIT

Eventually I'd like to use the available type information to eventually allow the runtime
to be JIT'd. This is a very far off goal but that would be the ideal endstate.
