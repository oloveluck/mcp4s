package mcp4s.agent

import cats.effect.Concurrent
import io.circe.Json
import mcp4s.protocol.{Tool, ToolResult}
import mcp4s.server.Tools

/** Factory for creating server-side tools that have access to the agent's context.
  *
  * This bridges the agent world and the server module's `Tools[F]` type.
  * The builder stores these as `AgentContext[F] => Tools[F]` factories.
  */
object AgentTools:

  /** Create a server-side tool that has access to the agent's context.
    *
    * The actual `Tools[F]` is created lazily at server construction time
    * (when `AgentContext` is available).
    *
    * @example
    * {{{
    * val factory = AgentTools.single[IO](Tool("chat", Some("Chat"), schema)) { (args, ctx) =>
    *   ctx.llmClient.complete(LlmRequest(...)).map(r => ToolResult.text(r.content))
    * }
    * }}}
    */
  def single[F[_]: Concurrent](tool: Tool)(
      handler: (Json, AgentContext[F]) => F[ToolResult]
  ): AgentContext[F] => Tools[F] =
    ctx => Tools.single(tool)(args => handler(args, ctx))
