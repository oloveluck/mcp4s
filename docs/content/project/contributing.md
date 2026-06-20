# Contributing

How to set up your development environment and contribute to mcp4s.

## Development Setup

### Prerequisites

- **Java 17+** (Temurin recommended)
- **sbt** build tool
- **Node.js 18+** and a C toolchain (`clang`) for the JS and Native targets
- **Scala Native** system libraries when building Native locally
  (e.g. `clang`, `libgc`)

### Clone and Build

```bash
git clone https://github.com/oloveluck/mcp4s.git
cd mcp4s
git submodule update --init   # fetch the conformance submodule
sbt compile                   # compiles JVM + JS + Native
```

### Run Tests

```bash
sbt test                      # all platforms
sbt rootJVM/test              # a single platform (rootJVM / rootJS / rootNative)
```

### Build the Docs Site

```bash
sbt docs/run
```

The output will be in `docs/target/site/`. Open `index.html` in a browser.

## Code Style

mcp4s uses strict compiler settings:

- **`-Werror`** (fatal warnings, enforced in CI via sbt-typelevel) — warnings are errors
- **`-Wunused:all`** — unused imports, parameters, and locals are flagged

Make sure your code compiles cleanly before submitting a PR.

### Formatting

The project uses scalafmt. Format your code before committing:

```bash
sbt scalafmtAll scalafmtSbt
```

## Cross-Compilation

mcp4s targets a single Scala 3 version across three platforms:

| Platform | Notes |
|----------|-------|
| JVM | Reference platform; the `examples` and `docs` modules are JVM-only |
| Scala.js (Node) | Full server and client support via http4s-ember |
| Scala Native | Server and client; the sttp-based WebSocket *client* transport is JVM-only |

Your code must compile on **all three** platforms. Platform-specific code lives
under `<module>/{jvm,js,native}/src`; shared code under `<module>/shared/src`.

See [Version Support](version-support.md) for full dependency details.

## Project Structure

```
mcp4s/
├── core/          # Protocol types, codec derivation (JVM/JS/Native)
├── server/        # Server DSL and runtime (JVM/JS/Native)
├── client/        # Client connection and resilience (JVM/JS/Native)
├── examples/      # Example servers and clients (JVM-only)
├── conformance/   # MCP protocol conformance suite (git submodule)
├── docs/          # Documentation site (Laika, JVM-only)
└── build.sbt      # Build definition (sbt + sbt-typelevel)
```

## Pull Request Process

1. **Fork** the repository and create a feature branch from `main`
2. **Write tests** for new functionality
3. **Ensure all tests pass**: `sbt test`
4. **Ensure clean compilation** on all three platforms (JVM, JS, Native)
5. **Keep commits focused** — one logical change per commit
6. **Write a clear PR description** explaining what and why

### PR Checklist

- Code compiles without warnings on both Scala 3.3.4 and 3.6.4
- Tests pass
- New functionality has test coverage
- No unused imports or variables

## Reporting Issues

- Use [GitHub Issues](https://github.com/oloveluck/mcp4s/issues) to report bugs
- Include Scala version, JVM version, and a minimal reproducer
- Check existing issues before filing a duplicate
