package mcp4s.agent

import cats.effect.Concurrent
import cats.syntax.all.*
import mcp4s.client.{Elicitations, McpConnection, Roots, Samplings}
import mcp4s.protocol.*
import mcp4s.server.{Prompts, Resources, Tools}

/** Builder for constructing an [[Agent]] with composable handlers.
  *
  * Follows the same conventions as `McpClientBuilder` from the client module.
  *
  * @example
  * {{{
  * Agent.builder[IO](llm, connection)
  *   .withInfo(ClientInfo("my-agent", "1.0.0"))
  *   .withConfig(LlmConfig(maxTurns = 20))
  *   .withSampling(llm.asSampling("gpt-4"))
  *   .build
  * }}}
  */
final class AgentBuilder[F[_]: Concurrent] private (
    private val state: AgentBuilder.State[F]
):

  /** Set the client info sent during initialization. */
  def withInfo(info: ClientInfo): AgentBuilder[F] =
    new AgentBuilder(state.copy(clientInfo = info))

  /** Set the LLM configuration (system prompt, temperature, maxTurns, etc.). */
  def withConfig(config: LlmConfig): AgentBuilder[F] =
    new AgentBuilder(state.copy(config = config))

  /** Pre-set tool schemas (skips fetching from connection on build). */
  def withToolSchemas(schemas: List[ToolSchema]): AgentBuilder[F] =
    new AgentBuilder(state.copy(toolSchemas = Some(schemas)))

  /** Set composed roots provider. */
  def withRoots(roots: Roots[F]): AgentBuilder[F] =
    new AgentBuilder(state.copy(roots = Some(roots)))

  /** Register composed sampling handler. */
  def withSampling(sampling: Samplings[F]): AgentBuilder[F] =
    new AgentBuilder(state.copy(sampling = Some(sampling)))

  /** Convenience for `.withSampling(llmClient.asSampling(model))`. */
  def withDefaultSampling(model: String): AgentBuilder[F] =
    withSampling(state.llmClient.asSampling(model))

  /** Convenience for `.withSampling(llmClient.asSampling(config))`. */
  def withDefaultSampling: AgentBuilder[F] =
    withSampling(state.llmClient.asSampling(state.config))

  /** Register composed elicitation handler. */
  def withElicitation(elicitation: Elicitations[F]): AgentBuilder[F] =
    new AgentBuilder(state.copy(elicitation = Some(elicitation)))

  /** Provide a custom loop factory. The factory receives a `LoopContext` and
    * returns the `AgentLoop` to drive the agent. Overrides any accumulated hooks.
    */
  def withLoop(factory: LoopContext[F] => AgentLoop[F]): AgentBuilder[F] =
    new AgentBuilder(state.copy(loopFactory = Some(factory)))

  /** Accumulate a turn hook. Multiple hooks compose via `Semigroup`.
    *
    * The factory receives a `LoopContext` so hooks can access the LLM client.
    */
  def withHook(factory: LoopContext[F] => TurnHook[F]): AgentBuilder[F] =
    new AgentBuilder(state.copy(hookFactories = state.hookFactories :+ factory))

  /** Accumulate a loop middleware. Multiple middlewares compose via `Semigroup`. */
  def withMiddleware(mw: LoopMiddleware[F]): AgentBuilder[F] =
    new AgentBuilder(state.copy(middleware = state.middleware :+ mw))

  /** Use a `ReflectionLoop` with the default configuration. */
  def withReflection: AgentBuilder[F] =
    withReflection(ReflectionConfig())

  /** Use a `ReflectionLoop` with the given configuration.
    *
    * Accumulates as a hook — can be combined with other hooks like `withChainOfThought`.
    */
  def withReflection(config: ReflectionConfig): AgentBuilder[F] =
    withHook(ctx => TurnHook.reflection(ctx, config))

  /** Use a `ChainOfThoughtLoop` with the default configuration. */
  def withChainOfThought: AgentBuilder[F] =
    withChainOfThought(ChainOfThoughtConfig())

  /** Use a `ChainOfThoughtLoop` with the given configuration.
    *
    * Accumulates as a hook — can be combined with other hooks like `withReflection`.
    */
  def withChainOfThought(config: ChainOfThoughtConfig): AgentBuilder[F] =
    withHook(ctx => TurnHook.thinking(ctx, config))

  /** Enable context window management with a character-based token estimator.
    *
    * @param budget token budget for the context window
    * @param policyFactory creates a `ContextPolicy` given a `LoopContext` (for LLM access)
    */
  def withContextWindow(
      budget: TokenBudget,
      policyFactory: LoopContext[F] => ContextPolicy[F]
  ): AgentBuilder[F] =
    withHook(ctx => TurnHook.contextManaged(TokenEstimator.charBased[F], policyFactory(ctx), budget))

  /** Enable context window management with a custom token estimator.
    *
    * @param budget token budget for the context window
    * @param estimator custom token estimator
    * @param policyFactory creates a `ContextPolicy` given a `LoopContext` (for LLM access)
    */
  def withContextWindow(
      budget: TokenBudget,
      estimator: TokenEstimator[F],
      policyFactory: LoopContext[F] => ContextPolicy[F]
  ): AgentBuilder[F] =
    withHook(ctx => TurnHook.contextManaged(estimator, policyFactory(ctx), budget))

  // === Server-side registration ===

  /** Set the server info used when exposing this agent as an MCP server. */
  def withServerInfo(info: ServerInfo): AgentBuilder[F] =
    new AgentBuilder(state.copy(serverInfo = Some(info)))

  /** Register plain server-side tools (no agent context access). Composes via `|+|`. */
  def withServerTools(tools: Tools[F]): AgentBuilder[F] =
    val combined = state.serverTools match
      case Some(existing) => Some(existing |+| tools)
      case None           => Some(tools)
    new AgentBuilder(state.copy(serverTools = combined))

  /** Register server-side tools that receive `AgentContext`. Accumulates. */
  def withAgentTools(factory: AgentContext[F] => Tools[F]): AgentBuilder[F] =
    new AgentBuilder(state.copy(serverToolFactories = state.serverToolFactories :+ factory))

  /** Register server-side resources. Composes via `|+|`. */
  def withServerResources(resources: Resources[F]): AgentBuilder[F] =
    val combined = state.serverResources match
      case Some(existing) => Some(existing |+| resources)
      case None           => Some(resources)
    new AgentBuilder(state.copy(serverResources = combined))

  /** Register server-side prompts. Composes via `|+|`. */
  def withServerPrompts(prompts: Prompts[F]): AgentBuilder[F] =
    val combined = state.serverPrompts match
      case Some(existing) => Some(existing |+| prompts)
      case None           => Some(prompts)
    new AgentBuilder(state.copy(serverPrompts = combined))

  /** Expose the agent as a callable server-side tool.
    *
    * When `toServer` is called, the resulting server will include a tool with
    * the given name and description. Calling this tool runs the agent loop with
    * the provided prompt and returns the final text content.
    *
    * @example
    * {{{
    * Agent.builder[IO](llm, conn)
    *   .asTool("ask", "Ask the agent a question")
    *   .build
    *   .flatMap(_.toServer)
    * }}}
    */
  def asTool(name: String, description: String): AgentBuilder[F] =
    new AgentBuilder(state.copy(agentRunTool = Some((name, description))))

  /** Build the agent, fetching tool schemas from the connection if not pre-set. */
  def build: F[Agent[F]] =
    val caps = ClientCapabilities(
      roots = state.roots.map(_ => RootsCapability(Some(true))),
      sampling = state.sampling.map(_ => SamplingCapability()),
      elicitation = state.elicitation.map(_ => ElicitationCapability())
    )

    def resolveLoop(ctx: LoopContext[F]): AgentLoop[F] =
      // loopFactory (via withLoop) is an escape hatch that overrides hooks
      val baseLoop = state.loopFactory match
        case Some(factory) => factory(ctx)
        case None =>
          state.hookFactories match
            case Nil => ToolLoop(ctx)
            case factories =>
              val hooks = factories.map(_(ctx))
              ToolLoop(ctx, hooks.reduce(_ |+| _))
      // Middleware always wraps the resolved loop
      state.middleware match
        case Nil => baseLoop
        case mws => baseLoop.withMiddleware(mws.reduce(_ |+| _))

    def mkAgent(ctx: LoopContext[F]): Agent[F] =
      new Agent(
        resolveLoop(ctx),
        state.llmClient,
        state.config,
        state.clientInfo,
        caps,
        state.sampling,
        state.elicitation,
        state.roots,
        state.serverInfo,
        state.serverTools,
        state.serverToolFactories,
        state.serverResources,
        state.serverPrompts,
        state.agentRunTool
      )

    state.toolSchemas match
      case Some(schemas) =>
        val ctx = LoopContext(state.llmClient, state.connection, schemas, state.config)
        Concurrent[F].pure(mkAgent(ctx))
      case None =>
        state.connection.listTools.map { tools =>
          val ctx = LoopContext(state.llmClient, state.connection, ToolSchema.fromTools(tools), state.config)
          mkAgent(ctx)
        }

object AgentBuilder:
  private[agent] case class State[F[_]](
      llmClient: LlmClient[F],
      connection: McpConnection[F],
      clientInfo: ClientInfo = ClientInfo("mcp4s-agent", "0.1.0"),
      config: LlmConfig = LlmConfig.default,
      toolSchemas: Option[List[ToolSchema]] = None,
      sampling: Option[Samplings[F]] = None,
      elicitation: Option[Elicitations[F]] = None,
      roots: Option[Roots[F]] = None,
      loopFactory: Option[LoopContext[F] => AgentLoop[F]] = None,
      hookFactories: List[LoopContext[F] => TurnHook[F]] = Nil,
      middleware: List[LoopMiddleware[F]] = Nil,
      serverInfo: Option[ServerInfo] = None,
      serverTools: Option[Tools[F]] = None,
      serverToolFactories: List[AgentContext[F] => Tools[F]] = Nil,
      serverResources: Option[Resources[F]] = None,
      serverPrompts: Option[Prompts[F]] = None,
      agentRunTool: Option[(String, String)] = None
  )

  private[agent] def create[F[_]: Concurrent](
      llmClient: LlmClient[F],
      connection: McpConnection[F]
  ): AgentBuilder[F] =
    new AgentBuilder[F](State(llmClient, connection))
