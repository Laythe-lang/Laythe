# SpaceLox

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
cargo flamegraph --bin=spacelox [filepath]
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

**Built in Classes**: Spacelox now has machinery to give all types methods. Some simple examples include `.str()` methods to get a string representation of each type.

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

**New Collection Types**: Spacelox now has lists and maps as part of the language both supporting literals.

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

**Value** The value type is currently a single rust enum where object is flatten into booleans, numbers and nil. This may change in the future as it currently prevents the NAN tagging optimization.

### Performance

In the whole performance is unfortunately only about 50~60% that of clox. For the internal hashmap I use the `hashbrown` crate with the `fnv` hasher which should be about the best setup for small hashes as all hashes are 16bytes. This still falls quite short of the brutally simply method used in clox. Beyond that I have not implemented the NAN tagging optimization. I'm hoping to eventually close the gap here.