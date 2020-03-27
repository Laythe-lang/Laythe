# SpaceLox

A rust implementation of lox as a virtual machine. This project is currently in progress as I progress through the book [Crafting Interpreters](https://craftinginterpreters.com/).

# Getting Started

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
To run the repl use.
```
cargo run [--release] [filepath]
```