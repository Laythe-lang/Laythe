# Laythe

A programming language originally based on the 2nd book of [Crafting Interpreters](https://craftinginterpreters.com/). See git tag [v0.1.0](https://github.com/Laythe-lang/Laythe/releases/tag/v0.1.0) for a fully compliant lox implementations. Since v0.1.0 I've continued adding features and exploring what's possible to learn and do in the PL world. 

# Getting Started

This project can be built using the typical set of cargo commands for building, testing, running and benching.

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

# Notable differences

I've continued to work on the language with a few extensions. Below are some of the differences


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

**Type Annotations**: Laythe now supports a basic set of type annotations. Long term this will eventually turn into optional typing, but the parser will now ingest some Typescript like annotations.

```
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

## Modified

### Gc
The gc is setup quite differently from clox mostly because dealing with lifetimes in the base implementation was nearly impossible. Instead objects as of 5/8/2020 Act as a sort of smart pointer into a `Vec<Box<Value>>`. The gc takes a context for to specify the object roots. Gc is also explicitly turned off during compilation as values are only freed during the main loop.

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

fn laytheFnExpressionBody (a, b, c) a + b + c
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

In the whole performance is unfortunately only about 50~60% that of clox. For the internal hashmap I use the `hashbrown` crate with the `fnv` hasher which should be about the best setup for small hashes as all hashes are 16bytes. This still falls quite short of the brutally simply method used in clox. Beyond that I have not implemented the NAN tagging optimization. I'm hoping to eventually close the gap here.

Running the original benchmark suite on a 2015 dell xps we have.

|benchmark|clox|Laythe|relative speed|notes|
|--|--|--|--|--|
|binary_tress.lox|total: 0.653353|total: 0.615767|1.06|This likely due to the small number of properties and the init optimization. We essentially avoid hashing altogether in this bench|
|equality.lox|loop: 2.23344 elapsed: 2.09234|loop: 2.650926 elapsed: 3.375722|0.61|Similar to above we have essentially zero hashing and again perform pretty equal to clox|
|fib.lox|total: 1.34376|total: 1.958585|0.69|Here we have some hashing from the `fib` lookup but even making this local shows a difference. It appears there is still some performance difference in function calling here|
|instantiation.lox|total: 0.800149|total: 1.505313|0.53|Again localizing this gives a decent speedup but hashing and function calls still seem to slow Laythe down|
|invocation.lox|total: 0.425061|total: 1.046553|0.41|Now the hashing speed difference is quite apparent|
|method_call.lox|total: 0.272629|total: 0.434426|0.63|Same as above|
|properties.lox|total: 0.653874|total: 1.501415|0.43|Same as above|
|trees.lox|total: 3.25011|total: 5.259737|0.62|Same as above|
|zoo.lox|total: 0.476715|total: 0.937690|0.51|Same as above|

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

### Implicit Return
I think this is just a nice to have and can really cleanup some code, but I think the rust / ruby etc. implicit return really is visually nice

```laythe
fn example() {
  10 // returned
}
```

### Move Instance hash to Class
The number of instance to classes in almost every case should be relatively large. In this case it makes a lot of sense in terms of memory to instead store the field mapping on the class. Where it `HashMap<Managed<String>, usize>`. The instance could then just hold a vector of `Value`s which should pretty significantly reduce memory pressure and usage. 

To accompany this we'll need to close all instances so you can no longer add field after the initializer. I think this behavior is not very desirable to begin with, plus it leads to a lot of potential in terms of gradual typing and optimization. 

As a side effect of this change we should be able to emit new instruction of getting field `0-N` directly and completely avoiding the hash.

### Debug an Release
I think I'd like to strive to keep as much of the plug and play philosophy of many dynamics languages around while optionally providing some of the static. Ideally I'd like to be able to run a script without any type checking and no optimization passes. I think having a separate mode that does both type check and some basic optimization could address some short coming some dynamic project face when they become large. 