# Laythe

A rust implementation of lox from the 2nd book of [Crafting Interpreters](https://craftinginterpreters.com/). The implementation is complete in the sense that it currently passes all tests in the original clox implementation.

I've continued to work on this language attempting to add features to make the project less of a toy language and more of usable.

## Getting Started

This project can be built uses the typical set of cargo commands for building, testing, running and benching.

#### In debug
```
cargo build
```

#### In Release
```
cargo build release
```

To run the test suite run the following.
```
cargo test
```

A set of benchmarks have been setup using criterion. To run these again use cargo.

```
cargo bench
```

If you have the the flamegraph cargo subcommand you can profile a script by the following.

```
cargo flamegraph --bin=laythe [filepath]
```

To run the repl use.
```
cargo run [--release] 
```

To run a script use.
```
cargo run [--release] [filepath]
```

## Notable differences

I've continued to work on the language with a few extensions. Below are some of the differences


### Added

**Built in Classes**: Laythe now has machinery to give all types methods. Some simple examples include `.str()` methods to get a string representation of each type.

```lox
> var x = true;
> x.str()
'true'
```

**Lambdas**: There are now function expressions. This was actually a very minimal change to enable this as it reuses almost all the the function machinery.

```
> var func = |x| x * 2;
> var withBody |name| { print "hi! " + name; };
> func(5)
10
> withBody("John")
hi! john
```

**New Collection Types**: Laythe now has lists and maps as part of the language both supporting literals.

```lox
> var list = [1, false, nil, 3, clock];
> var map = { 'key1', 10, 'key2': false, 15: nil };
> list[2];
nil
> map[false];
15
```
Map support all types with objects supported by reference equality. Since strings are interned this gives the desire value comparison results most would expect.

### Modified

**Gc** The gc is setup quite differently from clox mostly because dealing with lifetimes in the base implementation was nearly impossible. Instead objects as of 5/8/2020 Act as a sort of smart pointer into a `Vec<Box<Value>>`. The gc takes a context for determining when should be considered roots. Gc is also explicitly turned off during compilation as values are only freed during the main loop.

### Performance

In the whole performance is unfortunately only about 50~60% that of clox. For the internal hashmap I use the `hashbrown` crate with the `fnv` hasher which should be about the best setup for small hashes as all hashes are 16bytes. This still falls quite short of the brutally simply method used in clox. Beyond that I have not implemented the NAN tagging optimization. I'm hoping to eventually close the gap here.

Running the original benchmark suite on a 2015 dell xps we have

|benchmark|clox|Laythe|relative speed|notes|
|--|--|--|--|--|
|binary_tress.lox|total: 0.781831|total: 0.7417039|1.05|This likely due to the small number of properties and the init optimization. We essentially avoid hashing altogether in this bench|
|equality.lox|loop: 2.42498 elapsed: 2.70574|loop: 2.632479 elapsed: 2.936534|0.92|Similar to above we have essentially zero hashing and again perform pretty equal to clox|
|fib.lox|total: 1.43954|total: 2.613307|0.55|Here we have some hashing from the `fib` lookup but even making this local shows a difference. It appears there is still some performance difference in function calling here|
|instantiation.lox|total: 0.904372|total: 1.75085|0.51|Again localizing this gives a decent speedup but hashing and function calls still seem to slow Laythe down|
|invocation.lox|total: 0.433293|total: 1.181278|0.366|Now the hashing speed difference is quite apparent|
|method_call.lox|total: 0.286027|total: 0.531728|0.53|Same as above|
|properties.lox|total: 0.732466|total: 1.725661|0.42|Same as above|
|trees.lox|total: 3.48647|total: 6.325482|0.55|Same as above|
|zoo.lox|total: 0.474|total: 1.167421|0.406|Same as above|

