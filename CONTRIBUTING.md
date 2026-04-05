# Contributing to mcp4s

Thank you for your interest in contributing to mcp4s! This guide will help you get started.

## Development Setup

### Prerequisites

- **Java 21+** (GraalVM or Temurin recommended)
- **Mill** build tool (included via bootstrap script, or install globally)

### Clone and Build

```bash
git clone https://github.com/oloveluck/mcp4s.git
cd mcp4s
mill __.compile
```

### Run Tests

```bash
mill __.test
```

### Build the Docs Site

```bash
mill site.devDist
```

The output will be in `out/site/devDist.dest/`. Open `index.html` in a browser.

## Code Style

mcp4s uses strict compiler settings:

- **`-Xfatal-warnings`** — all warnings are treated as errors
- **`-Wunused:all`** — unused imports, parameters, and locals are flagged

Make sure your code compiles cleanly before submitting a PR.

### Formatting

The project uses scalafmt. Format your code before committing:

```bash
mill mill.scalalib.scalafmt.ScalafmtModule/reformatAll __.sources
```

## Cross-Compilation

mcp4s is cross-compiled for two Scala versions:

| Version | Purpose |
|---------|---------|
| Scala 3.3.4 | LTS — widest ecosystem compatibility |
| Scala 3.6.4 | Latest — newer language features |

Your code must compile on **both** versions. Avoid using features only available in Scala 3.5+.

## Project Structure

```
mcp4s/
├── core/          # Protocol types, codec derivation
├── server/        # Server DSL and runtime
├── client/        # Client connection and resilience
├── http/          # HTTP transport (http4s)
├── stdio/         # Stdio transport
├── conformance/   # MCP protocol conformance tests
├── site/          # Documentation site (Scala.js)
└── build.mill     # Build definition
```

## Pull Request Process

1. **Fork** the repository and create a feature branch from `main`
2. **Write tests** for new functionality
3. **Ensure all tests pass**: `mill __.test`
4. **Ensure clean compilation** on both Scala versions
5. **Keep commits focused** — one logical change per commit
6. **Write a clear PR description** explaining what and why

### PR Checklist

- [ ] Code compiles without warnings on both Scala 3.3.4 and 3.6.4
- [ ] Tests pass
- [ ] New functionality has test coverage
- [ ] No unused imports or variables

## Reporting Issues

- Use [GitHub Issues](https://github.com/oloveluck/mcp4s/issues) to report bugs
- Include Scala version, JVM version, and a minimal reproducer
- Check existing issues before filing a duplicate

## License

By contributing, you agree that your contributions will be licensed under the project's existing license.
