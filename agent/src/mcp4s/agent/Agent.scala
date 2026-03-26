package mcp4s.agent

import cats.effect.{Concurrent, Ref}
import cats.effect.std.Queue
import cats.effect.syntax.monadCancel.*
import cats.syntax.all.*
import fs2.Stream
import mcp4s.client.{Elicitations, McpClient, McpConnection, Roots, Samplings}
import mcp4s.protocol.*
import mcp4s.server.{McpTool, Prompts, Resources, Server, Tools}

/** Recursive agent loop that connects an LLM to MCP tools.
  *
  * `Agent` extends `McpClient[F]`, making it a first-class MCP client that can
  * handle server-initiated requests (sampling, elicitation, roots) while also
  * driving an autonomous tool-calling loop.
  *
  * The loop behavior is determined by the `AgentLoop[F]` instance, which defaults
  * to `ToolLoop` (the standard tool-calling cycle). Alternative strategies such
  * as `ReflectionLoop` can be provided via the builder or companion factories.
  *
  * Events are pushed to an `fs2.Queue` and consumed as `Stream[F, AgentEvent]`.
  */
final class Agent[F[_]] private[agent] (
    loop: AgentLoop[F],
    private val llmClient: LlmClient[F],
    private val agentConfig: LlmConfig,
    val info: ClientInfo,
    val capabilities: ClientCapabilities,
    samplingHandler: Option[Samplings[F]],
    elicitationHandler: Option[Elicitations[F]],
    roots: Option[Roots[F]],
    private val serverInfoOpt: Option[ServerInfo],
    private val serverTools: Option[Tools[F]],
    private val serverToolFactories: List[AgentContext[F] => Tools[F]],
    private val serverResources: Option[Resources[F]],
    private val serverPrompts: Option[Prompts[F]],
    private val agentRunTool: Option[(String, String)]
)(using F: Concurrent[F])
    extends McpClient[F]:

  // === McpClient implementation ===

  def listRoots: F[ListRootsResult] =
    roots match
      case Some(r) => r.list.map(ListRootsResult(_))
      case None        => F.raiseError(McpError.MethodNotSupported("roots/list"))

  def createMessage(params: CreateMessageParams): F[CreateMessageResult] =
    samplingHandler match
      case Some(handler) =>
        handler.handle(params).value.flatMap {
          case Some(result) => F.pure(result)
          case None         => F.raiseError(McpError.MethodNotSupported("sampling/createMessage"))
        }
      case None =>
        F.raiseError(McpError.MethodNotSupported("sampling/createMessage"))

  def elicit(params: ElicitParams): F[ElicitResult] =
    elicitationHandler match
      case Some(handler) =>
        handler.handle(params).value.flatMap {
          case Some(result) => F.pure(result)
          case None         => F.raiseError(McpError.MethodNotSupported("elicitation/create"))
        }
      case None =>
        F.raiseError(McpError.MethodNotSupported("elicitation/create"))

  def onElicitationComplete(params: ElicitationCompleteParams): F[Unit] =
    elicitationHandler match
      case Some(handler) => handler.onComplete(params)
      case None          => F.unit

  // === Agent loop ===

  /** Run the agent loop with a single user message. */
  def run(userMessage: String): Stream[F, AgentEvent] =
    runWithHistory(List(Message.User(userMessage)))

  /** Run the agent loop with an existing message history. */
  def runWithHistory(messages: List[Message]): Stream[F, AgentEvent] =
    Stream.eval(Queue.unbounded[F, Option[AgentEvent]]).flatMap { queue =>
      val emit: AgentEvent => F[Unit] = event => queue.offer(Some(event))
      val producer = loop.run(messages, emit)
        .void
        .guarantee(queue.offer(None))
      Stream.fromQueueNoneTerminated(queue).concurrently(Stream.eval(producer))
    }

  // === Agent as Server ===

  /** Create an MCP `Server` exposing this agent's registered server-side tools,
    * resources, and prompts.
    *
    * If `asTool` was called on the builder, the resulting server includes a tool
    * that runs the agent loop with a prompt string and returns the final text.
    *
    * Returns `F[Server[F]]` because it allocates a shared `Ref` for conversation state.
    */
  def toServer: F[Server[F]] =
    Ref.of[F, List[Message]](Nil).map { messagesRef =>
      val ctx = AgentContext(this, llmClient, agentConfig, messagesRef)
      val sInfo = serverInfoOpt.getOrElse(ServerInfo(info.name, info.version))
      val runTool = agentRunTool.map { case (name, description) =>
        McpTool.singleString[F](name, description, "prompt") { prompt =>
          this.run(prompt)
            .collect { case AgentEvent.Finished(content) => content }
            .compile
            .lastOrError
            .map(ToolResult.text)
        }
      }
      val factoryTools = serverToolFactories.map(_(ctx))
      val allTools = (serverTools.toList ++ runTool.toList ++ factoryTools)
        .reduceOption(_ |+| _)
        .getOrElse(Tools.empty[F])
      Server.from[F](
        sInfo,
        allTools,
        serverResources.getOrElse(Resources.empty[F]),
        serverPrompts.getOrElse(Prompts.empty[F])
      )
    }

object Agent:

  /** Create an `Agent` by fetching the tool list from the connection. */
  def create[F[_]: Concurrent](
      llmClient: LlmClient[F],
      connection: McpConnection[F],
      config: LlmConfig = LlmConfig.default
  ): F[Agent[F]] =
    connection.listTools.map { tools =>
      fromSchemas(llmClient, connection, ToolSchema.fromTools(tools), config)
    }

  /** Create an `Agent` when tool schemas are already known. */
  def fromSchemas[F[_]: Concurrent](
      llmClient: LlmClient[F],
      connection: McpConnection[F],
      schemas: List[ToolSchema],
      config: LlmConfig = LlmConfig.default
  ): Agent[F] =
    val ctx = LoopContext(llmClient, connection, schemas, config)
    new Agent(
      loop = ToolLoop(ctx),
      llmClient = llmClient,
      agentConfig = config,
      info = ClientInfo("mcp4s-agent", "0.1.0"),
      capabilities = ClientCapabilities(),
      samplingHandler = None,
      elicitationHandler = None,
      roots = None,
      serverInfoOpt = None,
      serverTools = None,
      serverToolFactories = Nil,
      serverResources = None,
      serverPrompts = None,
      agentRunTool = None
    )

  /** Create a builder for constructing an Agent with handlers. */
  def builder[F[_]: Concurrent](
      llmClient: LlmClient[F],
      connection: McpConnection[F]
  ): AgentBuilder[F] =
    AgentBuilder.create(llmClient, connection)
