package mcp4s.server

import cats.effect.{Async, Resource}
import cats.effect.std.Console
import com.comcast.ip4s.Port
import fs2.io.net.Network
import org.http4s.server.Server
import org.typelevel.otel4s.trace.Tracer
import mcp4s.server.transport.{HttpConfig, HttpTransport, StdioTransport}

/** Convenient extension methods for running MCP servers.
  *
  * Import this to get simple one-liner server startup:
  *
  * {{{
  * import mcp4s.server.syntax.*
  *
  * // Run on stdio (for Claude Desktop, etc)
  * server.runStdio
  *
  * // Run on HTTP with defaults (port 3000)
  * server.serveHttp.useForever
  *
  * // Run on HTTP with custom port
  * server.serveHttp(port"8080").useForever
  * }}}
  */
object syntax:

  extension [F[_]](server: McpServer[F])

    /** Run the server on stdio transport.
      *
      * Reads JSON-RPC messages from stdin and writes responses to stdout.
      * Use this for local tool servers communicating with Claude Desktop.
      *
      * {{{
      * object MyServer extends IOApp.Simple:
      *   def run = myServer.runStdio
      * }}}
      */
    def runStdio(using Async[F], Console[F], Tracer[F]): F[Unit] =
      StdioTransport.run[F](server)

    /** Run the server on stdio transport with noop tracing.
      *
      * Convenience method that provides a noop Tracer automatically.
      */
    def runStdioNoTrace(using Async[F], Console[F]): F[Unit] =
      given Tracer[F] = Tracer.noop[F]
      StdioTransport.run[F](server)

    /** Serve the server over HTTP with default configuration.
      *
      * Defaults: host 0.0.0.0, port 3000, path /mcp, CORS enabled
      *
      * {{{
      * server.serveHttp.useForever
      * }}}
      */
    def serveHttp(using Async[F], Network[F], Tracer[F]): Resource[F, Server] =
      HttpTransport.serve[F](server)

    /** Serve the server over HTTP on a specific port.
      *
      * {{{
      * server.serveHttp(port"8080").useForever
      * }}}
      */
    def serveHttp(port: Port)(using Async[F], Network[F], Tracer[F]): Resource[F, Server] =
      HttpTransport.serve[F](server, HttpConfig[F](port = port))

    /** Serve the server over HTTP with custom configuration.
      *
      * {{{
      * server.serveHttpWith(HttpConfig(port = port"8080", enableCors = false))
      *   .useForever
      * }}}
      */
    def serveHttpWith(config: HttpConfig[F])(using Async[F], Network[F], Tracer[F]): Resource[F, Server] =
      HttpTransport.serve[F](server, config)

    /** Serve the server over HTTP with noop tracing.
      *
      * Convenience method that provides a noop Tracer automatically.
      */
    def serveHttpNoTrace(using Async[F], Network[F]): Resource[F, Server] =
      given Tracer[F] = Tracer.noop[F]
      HttpTransport.serve[F](server)

    /** Serve the server over HTTP on a specific port with noop tracing. */
    def serveHttpNoTrace(port: Port)(using Async[F], Network[F]): Resource[F, Server] =
      given Tracer[F] = Tracer.noop[F]
      HttpTransport.serve[F](server, HttpConfig[F](port = port))
