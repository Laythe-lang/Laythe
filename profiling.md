## Cargo flamegraph

```bash
echo -1 | sudo tee /proc/sys/kernel/perf_event_paranoid
cargo flamegraph --bin=laythe <path>
```

## Callgrind

```bash
valgrind --tool=callgrind --dump-instr=yes --collect-jumps=yes --simulate-cache=yes <path-to-your-executable                [your-executable-program-options]
```
