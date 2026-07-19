# mcp4s Benchmarks

Performance tooling for mcp4s, supporting the project goal of **high-performance, resilient,
and correct** MCP servers and clients. Benchmarks live in the JVM-only, non-published
`benchmarks` module and reuse the cross-platform `mcp4s-testkit` fixtures.

There are three complementary layers:

| Layer | Tool | What it measures |
|-------|------|------------------|
| **Micro** | JMH (`sbt-jmh`) | The in-memory request hot path with no network: codec decode, `Dispatcher` routing, tool lookup + execute + encode. |
| **End-to-end** | `ThroughputDriver` (HdrHistogram) | The full stack over a real transport: codecs + fs2 + ember + sessions + concurrency. Also a resilience probe (counts failed calls under load). |
| **SLO regression** | `McpBenchmark.run` / `McpPerformanceSuite` (in `mcp4s-testkit`, weaver) | Reusable load run against any `Server[IO]` — a one-liner returning a `PerfReport`, or a weaver suite asserting a `PerfProfile`'s SLOs (failure rate / throughput / p99). See [Testing](docs/content/testing/README.md). |

JMH tells you how fast the *core* is and how much it allocates; the driver and the perf suite
tell you what a *client actually experiences* and whether the system stays correct under
sustained, concurrent load. `McpPerformanceSuite` is the one downstream users can run against
their own servers as a CI gate.

## Running

### JMH microbenchmarks

```bash
# Full run (2 forks, statistically meaningful):
sbt "benchmarks/Jmh/run mcp4s.benchmarks.*"

# Allocation profile — bytes allocated per op (the authoritative cross-version metric):
sbt "benchmarks/Jmh/run -prof gc mcp4s.benchmarks.*"

# Quick smoke run (1 fork, 1 iteration):
sbt "benchmarks/Jmh/run -f 1 -wi 1 -i 1 mcp4s.benchmarks.*"
```

The microbenchmarks (all in `Throughput` and `SampleTime` modes) cover the request hot path and
the known optimization targets:

| Benchmark | Measures |
|-----------|----------|
| `DispatcherBench.dispatchToolsCall` | full in-memory `tools/call`: param extraction → lookup → execute → encode |
| `DispatcherBench.dispatchToolsList` | `tools/list`: capability check → list → encode |
| `DispatcherBench.decodeToolsCallRequest` | raw circe decode of a `tools/call` wire message |
| `DispatcherBench.encodeToolResult` | encode one `ToolResult` (runs the codec's per-message `dropNullValues`) |
| `DispatcherBench.encodeToolListResponse` | encode a `tools/list` result — optional-field dropping across many objects |
| `ToolLookupBench.dispatchToolsCallWorstCase` | `tools/call` vs **N tools** (`@Param` 1/10/100) — the left-biased `OptionT` lookup scan |
| `ResourceTemplateBench.dispatchResourcesRead` | `resources/read` against a URI template (guards the precompiled-regex optimization) |

### End-to-end throughput driver

```bash
# Defaults: http, 20000 calls, concurrency 8
sbt "benchmarks/runMain mcp4s.benchmarks.ThroughputDriver"

# WebSocket, custom load:
sbt -Dbench.transport=ws -Dbench.calls=50000 -Dbench.concurrency=16 \
    "benchmarks/runMain mcp4s.benchmarks.ThroughputDriver"
```

System properties: `bench.transport` (`http` | `ws`), `bench.calls`, `bench.concurrency`.
The driver starts a `TestServers.simple` server on an ephemeral port, opens N connections,
warms up, then measures — reporting throughput, latency percentiles, and **failed-call count**
(a non-zero failure count signals a resilience problem, not just a slow path).

## Comparing versions (regression tracking)

To track performance reliably across versions, lean on **allocations per op**
(`gc.alloc.rate.norm`, reported by `-prof gc`). It is *deterministic and machine-independent*, so
it's comparable across machines and over time. Throughput (ops/sec) depends on the machine and
JVM — only compare it on the **same hardware**, as a sanity check.

A committed reference lives at [`benchmarks/results/baseline.json`](benchmarks/results/) (see its
README). Regenerate / compare with:

```bash
# (Re)generate the baseline — note: -rff is relative to the benchmarks/ module dir:
sbt "benchmarks/Jmh/run -bm thrpt -prof gc -rf json -rff results/baseline.json mcp4s.benchmarks.*"

# Run the current build into a separate file, then eyeball both with jq:
sbt "benchmarks/Jmh/run -bm thrpt -prof gc -rf json -rff results/current.json mcp4s.benchmarks.*"

jq -r '.[] | "\(.benchmark)\t\(.params.toolCount // "-")\t\(.primaryMetric.score) \(.primaryMetric.scoreUnit)\t\(.secondaryMetrics["gc.alloc.rate.norm"].score) B/op"' \
  benchmarks/results/baseline.json | column -t -s$'\t'
```

(The `gc.alloc.rate.norm` key in the JSON has no leading `·` — that prefix is console-only.)

A **regression** is bytes/op going up beyond a few percent on a benchmark. The workflow for an
optimization: capture the baseline, make the change, re-run, confirm bytes/op dropped (and
same-machine throughput rose). This is exactly how the list-encoding cache (−80% on
`dispatchToolsList`) and the precompiled template regex (−28% on `dispatchResourcesRead`) below
were validated.

## Indicative results

> ⚠️ Throughput numbers below are from one machine (Apple M3) and are illustrative. The reliable,
> portable signal is **bytes/op** from `-prof gc` — compare that against `baseline.json`.

**JMH (in-memory core), full config, Apple M3 / Temurin 21 — from `baseline.json`:**

| Benchmark | bytes/op | note |
|-----------|---------:|------|
| `decodeToolsCallRequest` | 2,552 | |
| `encodeToolResult` | ~2,300 | |
| `dispatchToolsCall` | ~8,870 | |
| `dispatchToolsList` | **5,748** | was 28,896 before the list-encoding cache (**−80%**) |
| `encodeToolListResponse` | 23,176 | raw encode (no dispatch cache) — shows the cost the cache avoids |
| `ResourceTemplateBench.dispatchResourcesRead` | **7,791** | was 10,819 before precompiling the template regex (**−28%**) |
| `ToolLookupBench` (N=1 / 10 / 100) | 8,623 / 10,080 / 24,112 | bytes/op still grows with tool count (left-biased lookup scan) |

`encodeToolListResponse` (a raw encode, not routed through the dispatcher) stays high — it's the
~23 KB the `dispatchToolsList` cache now avoids re-paying on every list call. The remaining
growth in `ToolLookupBench` is the next candidate (Map-based lookup) if servers get large.

**End-to-end (`ThroughputDriver`):**

| Transport | load | success / fail | p50 | p99 | notes |
|-----------|------|---------------:|----:|----:|-------|
| HTTP | 2,000 calls, conc 8 | 2000 / 0 | ~5–8 ms | ~20–40 ms | stable; new Ember client per connection (throughput varies ~850–1,400 calls/s with machine load) |
| WebSocket | 4,000 calls, conc 8 | 4000 / 0 | ~1.5 ms | ~7.6 ms | http4s `JdkWSClient`; ~4,100 calls/s — fastest transport (was hanging under load before the migration) |

The gap between in-memory dispatch (~75k ops/s) and end-to-end HTTP shows the **transport —
not the server core — dominates real latency**, which is exactly the kind of insight that
justifies the two-layer approach.

## Findings & follow-ups

- **✅ RESOLVED — WebSocket calls stalling under sustained load.** The original sttp-based WS
  client (`HttpClientFs2Backend`) hung under longer runs: a fraction of calls never returned
  (no response, no error), so a 4,000-call run hung indefinitely. **Migrating the client WS
  transport to http4s `JdkWSClient`** (the high-level `WSConnectionHighLevel`, which handles
  ping/pong/close itself) fixed it: the same 4,000-call / concurrency-8 run now completes in
  ~1.0 s with **0 failures** at **~4,100 calls/sec** (p50 1.5 ms, p99 7.6 ms) — faster than
  HTTP. This was the first concrete issue the benchmark suite surfaced, and the fix directly
  serves the "resilient" goal.
- **✅ RESOLVED — list-response encoding allocated heavily.** `tools/list` re-encoded the tool
  list (circe derivation + `dropNullValues`) on every call (~29 KB/op). The dispatcher now caches
  the encoded JSON per list, keyed by value, so a static server encodes once and a `list_changed`
  server re-encodes the new list. **−80%** on `dispatchToolsList` (28,896 → 5,748 B/op).
- **✅ RESOLVED — resource-template regex recompiled per read.** `McpResource` template matching
  rebuilt and compiled the match regex on every read; it's now compiled once per template.
  **−28%** on `dispatchResourcesRead` (10,819 → 7,791 B/op).
- **Open — per-request dispatch overhead (~4 KB on `tools/call`).** Beyond codecs, each request
  allocates an otel `Span` (even with the noop tracer), a cancellation `Deferred`, and two
  in-flight `Map` updates. Short-circuiting the noop-tracer span is the likely next win.
- **Open — tool lookup is linear.** Allocations grow ~155 B per tool scanned; a `Map`-based
  lookup would flatten it for servers with many tools.

## Roadmap

1. ✅ Module scaffold, JMH matrix (dispatch/decode/encode, lookup-vs-N, template), end-to-end driver.
2. ✅ Committed baseline + allocations-per-op comparison workflow.
3. ✅ First optimizations: list-encoding cache (−80% `tools/list`), precompiled template regex
   (−28% template read).
4. Next: short-circuit the noop-tracer span; `Map`-based tool lookup; resilience/soak scenarios in
   the driver (bounded-queue saturation, `maxSessions` limits, chaos/jittered servers).
5. Optional: CI regression gating against the checked-in baseline.
