package mcp4s.examples

import cats.effect.{IO, IOApp}
import mcp4s.protocol.*
import mcp4s.server.*
import mcp4s.server.auth.*
import mcp4s.server.transport.*
import org.typelevel.otel4s.trace.Tracer

/** Example MCP server with OAuth authentication enabled.
  *
  * This example demonstrates how to configure an MCP server with authentication.
  *
  * Run with: mill examples.runMain mcp4s.examples.SecureCalculatorServer
  *
  * Test with curl:
  *
  * 1. No auth - should get 401:
  *    curl -v http://localhost:3000/mcp
  *
  * 2. Get metadata (works without auth):
  *    curl http://localhost:3000/.well-known/oauth-protected-resource
  *
  * 3. With token (using allowAll validator, any token works in dev mode):
  *    curl -H "Authorization: Bearer test-token" \
  *         -H "Content-Type: application/json" \
  *         -d '{"jsonrpc":"2.0","id":1,"method":"tools/list"}' \
  *         http://localhost:3000/mcp
  *
  * 4. With API key (when using apiKey validator):
  *    curl -H "Authorization: Bearer secret-key-123" \
  *         -H "Content-Type: application/json" \
  *         -d '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-03-26","capabilities":{},"clientInfo":{"name":"test","version":"1.0"}}}' \
  *         http://localhost:3000/mcp
  */
object SecureCalculatorServer extends IOApp.Simple:

  // Auth configuration
  val authConfig: AuthConfig[IO] = AuthConfig[IO](
    metadata = ProtectedResourceMetadata(
      resource = "http://localhost:3000",
      authorizationServers = List("https://auth.example.com"),
      scopesSupported = Some(List("mcp:read", "mcp:write"))
    ),
    // Use allowAll for development - accepts any non-empty token
    // For production, use apiKey or a JWT validator
    validator = TokenValidator.allowAll[IO],
    requiredScopes = Set.empty
  )

  // Alternative: Use API key validation for simple cases:
  // val authConfig: AuthConfig[IO] = AuthConfig[IO](
  //   metadata = ProtectedResourceMetadata(
  //     resource = "http://localhost:3000",
  //     authorizationServers = List("https://auth.example.com"),
  //     scopesSupported = Some(List("mcp:read", "mcp:write"))
  //   ),
  //   validator = TokenValidator.apiKey[IO](Set("secret-key-123", "another-key")),
  //   requiredScopes = Set.empty
  // )

  val server: McpServer[IO] = McpServer
    .builder[IO]
    .withInfo(ServerInfo("secure-calculator-server", "1.0.0"))
    .tool[AddArgs]("add", "Add two numbers") { args =>
      IO.pure(ToolResult.text(s"Result: ${args.a + args.b}"))
    }
    .tool[SubtractArgs]("subtract", "Subtract two numbers") { args =>
      IO.pure(ToolResult.text(s"Result: ${args.a - args.b}"))
    }
    .tool[MultiplyArgs]("multiply", "Multiply two numbers") { args =>
      IO.pure(ToolResult.text(s"Result: ${args.a * args.b}"))
    }
    .tool[DivideArgs]("divide", "Divide two numbers") { args =>
      if args.b == 0 then IO.pure(ToolResult.error("Cannot divide by zero"))
      else IO.pure(ToolResult.text(s"Result: ${args.a / args.b}"))
    }
    .resource("calc://help", "Calculator Help")(
      """Secure Calculator MCP Server
        |
        |This server requires authentication via Bearer token.
        |Available tools: add, subtract, multiply, divide
        |Each tool takes 'a' and 'b' as numbers.""".stripMargin
    )
    .build

  def run: IO[Unit] =
    given Tracer[IO] = Tracer.noop[IO]
    val httpConfig = HttpConfig[IO](auth = Some(authConfig))
    IO.println("Starting Secure Calculator MCP Server on http://localhost:3000") *>
      IO.println("Protected Resource Metadata: http://localhost:3000/.well-known/oauth-protected-resource") *>
      IO.println("Use 'Authorization: Bearer <token>' header to authenticate") *>
      HttpTransport.serve[IO](server, httpConfig).useForever
