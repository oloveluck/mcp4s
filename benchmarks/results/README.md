# Benchmark baselines

`baseline.json` is a committed JMH result used as the reference point for spotting performance
regressions between mcp4s versions.

## What's authoritative

- **`gc.alloc.rate.norm` (bytes allocated per op)** — *deterministic and machine-independent*.
  This is the number to compare across versions and across machines. A regression is bytes/op
  going **up**.
- **`primaryMetric.score` (ops/sec, throughput)** — machine- and JVM-dependent. Only meaningful
  when compared on the **same hardware**; treat it as advisory.

## How this baseline was produced

```
sbt "benchmarks/Jmh/run -bm thrpt -prof gc -rf json -rff benchmarks/results/baseline.json mcp4s.benchmarks.*"
```

- Config: `@Fork(2)`, 5 warmup + 5 measurement iterations (the benchmark annotations).
- Recorded on: **Apple M3 / macOS / Temurin 21** (bytecode targets JDK 17). Throughput numbers
  in this file reflect that machine; allocation numbers are portable.

## Refreshing

Regenerate when cutting a release or after a change that legitimately shifts the numbers:

1. Close other apps (a quiet machine matters for throughput; allocations are stable regardless).
2. Run the command above to overwrite `baseline.json`.
3. Sanity-check the diff (see `BENCHMARKS.md` → "Comparing versions"), then commit, noting the
   machine if it changed.

Do not hand-edit the JSON.
