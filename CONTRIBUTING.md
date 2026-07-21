# Contributing to mcp4s

## Setup

Requires **Java 17+** and **sbt**.

```bash
git clone --recurse-submodules https://github.com/oloveluck/mcp4s.git
cd mcp4s
sbt compile       # compile all modules (JVM/JS/Native cross-build)
sbt test          # run all tests (also compiles every docs code snippet)
sbt docs/run      # generate docs site to docs/target/site/
sbt conformance   # run the MCP conformance suite (needs Node 22+)
```

## Project Structure

```
core/          # Protocol types, codecs, schema DSL
client/        # Client connection and transports
server/        # Server DSL, dispatching, transports
testkit/       # Published test fixtures, compliance + performance suites
examples/      # Example servers and integration tests
benchmarks/    # JMH microbenchmarks and end-to-end throughput driver
conformance/   # MCP protocol conformance suite (git submodule)
docs/          # Documentation site (Laika)
```

The full contributor guide — code style, doc-snippet compilation, benchmarks, release
process — lives in the docs site under
[`docs/content/project/contributing.md`](docs/content/project/contributing.md).
