package mcp4s.server

import cats.{Applicative, Semigroup}
import cats.data.OptionT
import cats.effect.Concurrent
import cats.syntax.all.*
import fs2.Stream
import mcp4s.protocol.*

/** Composable resource routes for MCP servers.
  *
  * Resources are standalone typed values that compose via `|+|`.
  * They optionally carry a change stream for subscription support.
  *
  * {{{
  * val readme = McpResource[IO]("file:///readme", "README") {
  *   IO.pure(ResourceContent.text("file:///readme", "Hello world"))
  * }
  *
  * val config = McpResource[IO]("file:///config", "Config") {
  *   IO.pure(ResourceContent.text("file:///config", "{}"))
  * }
  *
  * val allResources: McpResources[IO] = readme |+| config
  * }}}
  */
trait McpResources[F[_]]:
  /** List all resources */
  def list: F[List[Resource]]

  /** List all resource templates */
  def listTemplates: F[List[ResourceTemplate]]

  /** Read a resource by URI, returning None if not handled */
  def read(uri: String): OptionT[F, ResourceContent]

  /** Stream of URIs that have changed. Empty for static resources. */
  def changes: Stream[F, String]

object McpResources:

  def empty[F[_]: Applicative]: McpResources[F] =
    new McpResources[F]:
      def list: F[List[Resource]] = Applicative[F].pure(Nil)
      def listTemplates: F[List[ResourceTemplate]] = Applicative[F].pure(Nil)
      def read(uri: String): OptionT[F, ResourceContent] = OptionT.none
      def changes: Stream[F, String] = Stream.empty

  def combine[F[_]: Concurrent](x: McpResources[F], y: McpResources[F]): McpResources[F] =
    new McpResources[F]:
      def list: F[List[Resource]] =
        for
          xRes <- x.list
          yRes <- y.list
          xUris = xRes.map(_.uri).toSet
        yield xRes ++ yRes.filterNot(r => xUris.contains(r.uri))

      def listTemplates: F[List[ResourceTemplate]] =
        for
          xTemplates <- x.listTemplates
          yTemplates <- y.listTemplates
          xUris = xTemplates.map(_.uriTemplate).toSet
        yield xTemplates ++ yTemplates.filterNot(t => xUris.contains(t.uriTemplate))

      def read(uri: String): OptionT[F, ResourceContent] =
        x.read(uri).orElse(y.read(uri))

      def changes: Stream[F, String] =
        x.changes.merge(y.changes)

  /** Semigroup instance for McpResources composition via |+| */
  given [F[_]: Concurrent]: Semigroup[McpResources[F]] with
    def combine(x: McpResources[F], y: McpResources[F]): McpResources[F] =
      McpResources.combine(x, y)

  extension [F[_]: Concurrent](resources: McpResources[F])
    def <+>(other: McpResources[F]): McpResources[F] =
      combine(resources, other)

  /** Create a template resource that matches URI patterns.
    *
    * Template resources define a URI pattern with placeholders (e.g., `api://users/{id}`)
    * and a handler that receives the actual URI when a matching resource is read.
    *
    * Example:
    * {{{
    * val users = McpResources.template[IO]("api://users/{id}", "User", "Get user by ID") { uri =>
    *   val id = uri.split("/").last
    *   IO.pure(ResourceContent.text(uri, s"""{"id":"$id"}"""))
    * }
    * }}}
    */
  def template[F[_]: Concurrent](
      uriPattern: String,
      name: String,
      description: String = ""
  )(handler: String => F[ResourceContent]): McpResources[F] =
    new McpResources[F]:
      private val resourceTemplate = ResourceTemplate(
        uriTemplate = uriPattern,
        name = name,
        description = if description.isEmpty then None else Some(description)
      )

      def list: F[List[Resource]] = Applicative[F].pure(Nil)
      def listTemplates: F[List[ResourceTemplate]] = Applicative[F].pure(List(resourceTemplate))
      def read(uri: String): OptionT[F, ResourceContent] =
        if matchesTemplate(uriPattern, uri) then OptionT.liftF(handler(uri))
        else OptionT.none[F, ResourceContent]
      def changes: Stream[F, String] = Stream.empty

      private def matchesTemplate(pattern: String, uri: String): Boolean =
        // Convert URI template pattern like "test://template/{id}/data" to regex
        val regexPattern = pattern
          .replace(".", "\\.")
          .replace("/", "\\/")
          .replaceAll("\\{[^}]+\\}", "[^/]+")
        uri.matches(regexPattern)

/** Factory for creating standalone resource values */
object McpResource:

  /** Create a static text resource */
  def apply[F[_]: Concurrent](uri: String, name: String)(content: => String): McpResources[F] =
    val resource = Resource(uri, name, mimeType = Some("text/plain"))
    single(resource)(_ => Concurrent[F].pure(ResourceContent.text(uri, content)))

  /** Create a resource with a handler */
  def handler[F[_]: Concurrent](uri: String, name: String, mimeType: String = "text/plain")(
      handler: String => F[ResourceContent]
  ): McpResources[F] =
    val resource = Resource(uri, name, mimeType = Some(mimeType))
    single(resource)(handler)

  /** Create a resource from a Resource definition and handler */
  def single[F[_]: Concurrent](resource: Resource)(handler: String => F[ResourceContent]): McpResources[F] =
    new McpResources[F]:
      def list: F[List[Resource]] = Applicative[F].pure(List(resource))
      def listTemplates: F[List[ResourceTemplate]] = Applicative[F].pure(Nil)
      def read(uri: String): OptionT[F, ResourceContent] =
        if uri == resource.uri then OptionT.liftF(handler(uri))
        else OptionT.none[F, ResourceContent]
      def changes: Stream[F, String] = Stream.empty

  /** Create a subscribable resource from a change stream.
    *
    * The returned `McpResources` carries a change stream that emits the URI
    * whenever the resource content changes. Use this for resources that need
    * to notify subscribers of updates.
    *
    * Example:
    * {{{
    * val watched = McpResource.subscribable[IO](
    *   "file:///config.json", "Config",
    *   fileWatcher.events.filter(_ == "config.json").void
    * ) { _ =>
    *   IO(ResourceContent.text("file:///config.json", readFile("/config.json")))
    * }
    * }}}
    */
  def subscribable[F[_]: Concurrent](
      uri: String,
      name: String,
      changeStream: Stream[F, Unit]
  )(readHandler: String => F[ResourceContent]): McpResources[F] =
    new McpResources[F]:
      private val resource = Resource(uri, name, mimeType = Some("text/plain"))
      def list: F[List[Resource]] = Applicative[F].pure(List(resource))
      def listTemplates: F[List[ResourceTemplate]] = Applicative[F].pure(Nil)
      def read(reqUri: String): OptionT[F, ResourceContent] =
        if reqUri == uri then OptionT.liftF(readHandler(reqUri))
        else OptionT.none[F, ResourceContent]
      def changes: Stream[F, String] = changeStream.as(uri)

  /** Create a subscribable resource that polls for changes.
    *
    * The resource will be checked periodically and change notifications emitted
    * when `hasChanged` returns true.
    *
    * Example:
    * {{{
    * val metrics = McpResource.polling[IO](
    *   "metrics://cpu", "CPU Metrics",
    *   5.seconds, checkCpuChanged
    * ) { _ => getCpuMetrics.map(text(_, _)) }
    * }}}
    */
  def polling[F[_]: cats.effect.Temporal](
      uri: String,
      name: String,
      pollInterval: scala.concurrent.duration.FiniteDuration,
      hasChanged: F[Boolean]
  )(readHandler: String => F[ResourceContent]): McpResources[F] =
    val changeStream = Stream
      .awakeEvery[F](pollInterval)
      .evalFilter(_ => hasChanged)
      .void
    subscribable(uri, name, changeStream)(readHandler)
