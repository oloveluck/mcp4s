package mcp4s.server

import cats.Applicative
import cats.effect.{Concurrent, Resource as CatsResource}
import cats.syntax.all.*
import mcp4s.protocol.*

/** Lifecycle trait for tools and resources that need initialization/cleanup.
  *
  * Tools and resources can implement this trait to declare resource requirements
  * that need to be managed during the server lifecycle.
  *
  * Example:
  * {{{
  * import mcp4s.server.mcp.*
  *
  * // Use McpLifecycleTool for tools that need managed resources:
  * val dbTool = McpLifecycleTool[IO, QueryArgs, Connection](
  *   "query",
  *   "Run SQL query",
  *   acquire = Database.connection(config)
  * ) { (args, conn) =>
  *   conn.execute(args.sql).map(ToolResult.text)
  * }
  * }}}
  */
trait McpLifecycle[F[_]: Applicative]:
  /** Resource to acquire before the server starts */
  def initialize: CatsResource[F, Unit] = CatsResource.unit

  /** Finalization to run when the server shuts down */
  def shutdown: F[Unit] = Applicative[F].unit

object McpLifecycle:
  /** Create a lifecycle from an initialize resource */
  def fromResource[F[_]: Applicative](resource: CatsResource[F, Unit]): McpLifecycle[F] =
    new McpLifecycle[F]:
      override def initialize: CatsResource[F, Unit] = resource

  /** Create an empty lifecycle (no-op) */
  def empty[F[_]: Applicative]: McpLifecycle[F] = new McpLifecycle[F] {}

/** Extension methods for Server to support lifecycle management */
extension [F[_]: Concurrent](server: Server[F])

  /** Wrap the server in a Resource that manages lifecycle.
    *
    * When the resource is acquired, the server is ready for use.
    * When released, any cleanup is performed.
    *
    * This is useful when tools or resources need managed connections
    * (database pools, HTTP clients, etc.) that should be cleaned up on shutdown.
    */
  def asResource: CatsResource[F, Server[F]] =
    CatsResource.pure(server)

/** Extension methods for Server with lifecycle-managed tools */
object ServerLifecycle:

  /** Create a server with lifecycle-managed tools.
    *
    * The returned Resource will:
    * 1. Acquire all tool resources when used
    * 2. Release all resources when the Resource is released
    *
    * @param info Server information
    * @param tools Tools with their lifecycle resources
    * @param resources Server resources
    * @param prompts Server prompts
    */
  def withLifecycle[F[_]: Concurrent](
      info: ServerInfo,
      tools: CatsResource[F, Tools[F]],
      resources: Resources[F],
      prompts: Prompts[F]
  ): CatsResource[F, Server[F]] =
    tools.map: managedTools =>
      Server.from[F](info, managedTools, resources, prompts)

  /** Create a server with lifecycle-managed tools and empty resources/prompts. */
  def withLifecycleToolsOnly[F[_]: Concurrent](
      info: ServerInfo,
      tools: CatsResource[F, Tools[F]]
  ): CatsResource[F, Server[F]] =
    tools.map: managedTools =>
      Server.from[F](info, managedTools, Resources.empty[F], Prompts.empty[F])

/** Namespace for lifecycle-aware tool creation */
object McpLifecycleTool:

  /** Create a tool with an acquired resource.
    *
    * The resource is acquired when the server starts and released when it stops.
    * The handler receives the acquired resource along with the tool arguments.
    *
    * Example:
    * {{{
    * val dbTools: Resource[IO, Tools[IO]] = McpLifecycleTool[IO, QueryArgs, HikariDataSource](
    *   "query",
    *   "Run SQL query",
    *   acquire = HikariCP.resource(config)
    * ) { (args, dataSource) =>
    *   IO.blocking {
    *     val conn = dataSource.getConnection()
    *     try {
    *       val stmt = conn.createStatement()
    *       val rs = stmt.executeQuery(args.sql)
    *       ToolResult.text(formatResults(rs))
    *     } finally {
    *       conn.close()
    *     }
    *   }
    * }
    * }}}
    */
  def apply[F[_]: Concurrent, A: ToolInput, R](
      name: String,
      description: String,
      acquire: CatsResource[F, R]
  )(handler: (A, R) => F[ToolResult]): CatsResource[F, Tools[F]] =
    acquire.map: resource =>
      McpTool[F, A](name, description): args =>
        handler(args, resource)

  /** Create a no-args tool with an acquired resource.
    *
    * Example:
    * {{{
    * val statusTool: Resource[IO, Tools[IO]] = McpLifecycleTool.noArgs[IO, HttpClient](
    *   "health",
    *   "Check service health",
    *   acquire = HttpClient.resource
    * ) { client =>
    *   client.get("http://service/health").map(resp => ToolResult.text(resp.body))
    * }
    * }}}
    */
  def noArgs[F[_]: Concurrent, R](
      name: String,
      description: String,
      acquire: CatsResource[F, R]
  )(handler: R => F[ToolResult]): CatsResource[F, Tools[F]] =
    acquire.map: resource =>
      McpTool.noArgs[F](name, description):
        handler(resource)

  /** Create a context-aware tool with an acquired resource.
    *
    * The handler receives the ToolContext for server-to-client operations,
    * as well as the acquired resource.
    */
  def withContext[F[_]: Concurrent, A: ToolInput, R](
      name: String,
      description: String,
      acquire: CatsResource[F, R]
  )(handler: (A, R, ToolContext[F]) => F[ToolResult]): CatsResource[F, Tools[F]] =
    acquire.map: resource =>
      McpTool.withContext[F, A](name, description): (args, ctx) =>
        handler(args, resource, ctx)

  /** Combine multiple lifecycle tools into a single resource.
    *
    * All tools' resources will be acquired together and released together.
    *
    * Example:
    * {{{
    * val allTools: Resource[IO, Tools[IO]] = McpLifecycleTool.combine(
    *   dbQueryTool,
    *   httpClientTool,
    *   cacheClientTool
    * )
    * }}}
    */
  def combine[F[_]: Concurrent](
      tools: CatsResource[F, Tools[F]]*
  ): CatsResource[F, Tools[F]] =
      tools.toList.sequence.map: toolsList =>
        toolsList.foldLeft(Tools.empty[F])(_ |+| _)

  /** Combine a lifecycle tool with a static tool.
    *
    * Example:
    * {{{
    * val combined: Resource[IO, Tools[IO]] = McpLifecycleTool.combineWith(
    *   lifecycleTool,
    *   staticTool1 |+| staticTool2
    * )
    * }}}
    */
  def combineWith[F[_]: Concurrent](
      lifecycle: CatsResource[F, Tools[F]],
      static: Tools[F]
  ): CatsResource[F, Tools[F]] =
      lifecycle.map(_ |+| static)