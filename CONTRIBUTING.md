# Contributing to mcp4s

## Setup

Requires **Java 21+** and **Mill** (bootstrap script included).

```bash
git clone https://github.com/oloveluck/mcp4s.git
cd mcp4s
mill __.compile   # compile all modules
mill __.test      # run all tests
mill docs.run     # generate docs site to out/docs/site/
```

## Project Structure

```
core/          # Protocol types, codecs
client/        # Client connection and transports
server/        # Server DSL, dispatching, transports
examples/      # Example servers and integration tests
conformance/   # MCP protocol conformance tests
docs/          # Documentation site (Laika)
```

## PR Checklist

- [ ] Compiles without warnings on Scala 3.3.4 and 3.6.4
- [ ] All tests pass
- [ ] New functionality has test coverage
