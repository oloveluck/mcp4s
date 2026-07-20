/*
 * Copyright 2025 MCP4S Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package mcp4s.client

import cats.Applicative
import cats.effect.Concurrent
import cats.syntax.all.*
import mcp4s.protocol.*

/** MCP client for connecting to MCP servers.
  *
  * Provides handlers for server-initiated requests:
  *   - Roots: Expose filesystem boundaries to servers
  *   - Sampling: Handle server-initiated LLM requests
  *   - Elicitation: Handle server-initiated user input requests
  *
  * Use [[McpClient.from]] with the composable DSL (`import mcp4s.client.mcp.*`).
  *
  * @example
  *   {{{
  * import mcp4s.client.mcp.*
  *
  * val sampling = Sampling[IO](params => message("Hello", "model").pure[IO])
  * val roots = Roots[IO]("file:///workspace", "Workspace")
  *
  * val client = McpClient.from[IO](
  *   info = ClientInfo("my-client", "1.0.0"),
  *   roots = Some(roots),
  *   sampling = Some(sampling)
  * )
  *   }}}
  */
trait McpClient[F[_]]:

  /** Client information sent during initialization */
  def info: ClientInfo

  /** Client capabilities */
  def capabilities: ClientCapabilities

  // === Server-Initiated Request Handlers (Client Features) ===

  /** Handle roots/list request from server Spec ref:
    * https://modelcontextprotocol.io/specification/2025-11-25/client/roots
    */
  def listRoots: F[ListRootsResult]

  /** Handle sampling/createMessage request from server Spec ref:
    * https://modelcontextprotocol.io/specification/2025-11-25/client/sampling
    */
  def createMessage(params: CreateMessageParams): F[CreateMessageResult]

  /** Handle elicitation/create request from server Spec ref:
    * https://modelcontextprotocol.io/specification/2025-11-25/client/elicitation
    */
  def elicit(params: ElicitParams): F[ElicitResult]

  /** Handle notifications/elicitation/complete from server Called when URL mode elicitation
    * completes (e.g., OAuth flow finished) Spec ref:
    * https://modelcontextprotocol.io/specification/2025-11-25/client/elicitation
    */
  def onElicitationComplete(params: ElicitationCompleteParams): F[Unit]

object McpClient:

  /** Create a client from composed handlers.
    *
    * This is the compositional alternative to the builder pattern, allowing handlers to be defined
    * separately and combined.
    *
    * @example
    *   {{{
    * import mcp4s.client.mcp.*
    * import cats.syntax.semigroup.*
    *
    * val sampling = Sampling[IO](params => message("Hello", "model").pure[IO])
    * val roots = Roots[IO]("file:///workspace", "Workspace") |+|
    *             Roots[IO]("file:///home", "Home")
    *
    * val client = McpClient.from[IO](
    *   info = ClientInfo("my-client", "1.0.0"),
    *   roots = Some(roots),
    *   sampling = Some(sampling)
    * )
    *   }}}
    */
  def from[F[_]: Concurrent](
      info: ClientInfo,
      roots: Option[Roots[F]] = None,
      sampling: Option[Samplings[F]] = None,
      elicitation: Option[Elicitations[F]] = None
  ): McpClient[F] =
    val caps = ClientCapabilities(
      roots = roots.map(_ => RootsCapability(Some(true))),
      sampling = sampling.map(_ => SamplingCapability()),
      elicitation = elicitation.map(_ => ElicitationCapability())
    )

    new ComposedMcpClient[F](
      info,
      caps,
      Nil,
      roots,
      sampling,
      elicitation
    )

  /** Start assembling a client fluently — the mirror of `McpServer` on the server side. */
  def builder[F[_]: Concurrent](info: ClientInfo): McpClientBuilder[F] =
    McpClientBuilder[F](info)

  extension [F[_]](client: McpClient[F])
    /** Connect by spawning a subprocess and speaking JSON-RPC over its stdin/stdout. */
    def stdio(config: transport.StdioTransportConfig)(using
        cats.effect.Async[F],
        fs2.io.process.Processes[F]
    )(using
        tracer: org.typelevel.otel4s.trace.Tracer[F] = org.typelevel.otel4s.trace.Tracer.noop[F]
    ): cats.effect.Resource[F, McpConnection[F]] =
      transport.StdioClientTransport.connect[F](client, config)

    /** Connect over Streamable HTTP using a caller-provided http4s `Client[F]`. On the JVM, see the
      * no-`Client` overload (`import mcp4s.client.syntax.*`) that builds an Ember client.
      */
    def http(config: transport.HttpTransportConfig[F], httpClient: org.http4s.client.Client[F])(
        using cats.effect.Async[F]
    )(using
        tracer: org.typelevel.otel4s.trace.Tracer[F] = org.typelevel.otel4s.trace.Tracer.noop[F]
    ): cats.effect.Resource[F, McpConnection[F]] =
      transport.HttpClientTransport.connect[F](client, config, httpClient)

/** Client implementation using composed handlers. */
final private[client] class ComposedMcpClient[F[_]: Concurrent](
    val info: ClientInfo,
    val capabilities: ClientCapabilities,
    private val staticRoots: List[Root],
    private val mcpRoots: Option[Roots[F]],
    private val samplingHandler: Option[Samplings[F]],
    private val elicitationHandler: Option[Elicitations[F]]
) extends McpClient[F]:

  def listRoots: F[ListRootsResult] =
    mcpRoots match
      case Some(roots) => roots.list.map(dynamic => ListRootsResult(staticRoots ++ dynamic))
      case None        => Applicative[F].pure(ListRootsResult(staticRoots))

  def createMessage(params: CreateMessageParams): F[CreateMessageResult] =
    samplingHandler match
      case Some(handler) =>
        handler.handle(params).value.flatMap {
          case Some(result) => Concurrent[F].pure(result)
          case None =>
            Concurrent[F].raiseError(McpError.MethodNotSupported("sampling/createMessage"))
        }
      case None => Concurrent[F].raiseError(McpError.MethodNotSupported("sampling/createMessage"))

  def elicit(params: ElicitParams): F[ElicitResult] =
    elicitationHandler match
      case Some(handler) =>
        handler.handle(params).value.flatMap {
          case Some(result) => Concurrent[F].pure(result)
          case None => Concurrent[F].raiseError(McpError.MethodNotSupported("elicitation/create"))
        }
      case None => Concurrent[F].raiseError(McpError.MethodNotSupported("elicitation/create"))

  def onElicitationComplete(params: ElicitationCompleteParams): F[Unit] =
    elicitationHandler match
      case Some(handler) => handler.onComplete(params)
      case None          => Concurrent[F].unit
