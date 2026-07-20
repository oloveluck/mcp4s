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

package mcp4s.server

import cats.{Applicative, Semigroup}
import cats.data.OptionT
import cats.effect.Concurrent
import cats.syntax.all.*
import fs2.Stream
import mcp4s.protocol.*

/** Composable resource routes for MCP servers.
  *
  * Resources are standalone typed values that compose via `|+|`. They optionally carry a change
  * stream for subscription support.
  *
  * {{{
  * import mcp4s.server.dsl.*
  *
  * val readme = Resource.text[IO]("file:///readme", "README")("Hello world")
  * val config = Resource.text[IO]("file:///config", "Config")("{}")
  *
  * val allResources: Resources[IO] = readme |+| config
  * }}}
  */
trait Resources[F[_]]:
  /** List all resources */
  def list: F[List[Resource]]

  /** List all resource templates */
  def listTemplates: F[List[ResourceTemplate]]

  /** Read a resource by URI, returning None if not handled */
  def read(uri: String): OptionT[F, ResourceContent]

  /** Stream of URIs that have changed. Empty for static resources. */
  def changes: Stream[F, String]

  /** True when no resources are registered. Used to derive server capabilities. */
  def isEmpty: Boolean = false

  /** True when at least one registered resource supports change subscriptions. Drives the
    * `resources.subscribe` capability flag.
    */
  def supportsSubscribe: Boolean = false

  /** Exact-URI read handlers when statically known; `None` for dynamic implementations and
    * pattern-matched templates. Lets [[Resources.combine]] dispatch by Map lookup instead of
    * scanning the composition chain — any template or dynamic instance in a composition falls back
    * to the first-match-wins `orElse` chain.
    */
  private[server] def exactReads: Option[Map[String, String => F[ResourceContent]]] = None

object Resources:

  def empty[F[_]: Applicative]: Resources[F] =
    new Resources[F]:
      def list: F[List[Resource]]                        = Applicative[F].pure(Nil)
      def listTemplates: F[List[ResourceTemplate]]       = Applicative[F].pure(Nil)
      def read(uri: String): OptionT[F, ResourceContent] = OptionT.none
      def changes: Stream[F, String]                     = Stream.empty
      override def isEmpty: Boolean                      = true
      private[server] override val exactReads            = Some(Map.empty)

  /** Create resource routes from a raw Resource definition and a handler. */
  def single[F[_]: Concurrent](resource: Resource)(
      handler: String => F[ResourceContent]
  ): Resources[F] =
    McpResource.single[F](resource)(handler)

  def combine[F[_]: Concurrent](x: Resources[F], y: Resources[F]): Resources[F] =
    new Resources[F]:
      override def isEmpty: Boolean           = x.isEmpty && y.isEmpty
      override def supportsSubscribe: Boolean = x.supportsSubscribe || y.supportsSubscribe
      // Left side wins on duplicate URIs, matching the orElse chain's shadowing.
      private[server] override val exactReads =
        (x.exactReads, y.exactReads).mapN((xr, yr) => yr ++ xr)
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
        exactReads match
          case Some(table) => OptionT(table.get(uri).traverse(h => h(uri)))
          case None        => x.read(uri).orElse(y.read(uri))

      def changes: Stream[F, String] =
        x.changes.merge(y.changes)

  /** Semigroup instance for Resources composition via |+| */
  given [F[_]: Concurrent]: Semigroup[Resources[F]] with
    def combine(x: Resources[F], y: Resources[F]): Resources[F] =
      Resources.combine(x, y)

  /** Create a template resource that matches URI patterns.
    *
    * Template resources define a URI pattern with placeholders (e.g., `api://users/{id}`) and a
    * handler that receives the actual URI when a matching resource is read.
    *
    * Example:
    * {{{
    * val users = Resources.template[IO]("api://users/{id}", "User", "Get user by ID") { uri =>
    *   val id = uri.split("/").last
    *   IO.pure(ResourceContent.text(uri, s"""{"id":"$id"}"""))
    * }
    * }}}
    */
  def template[F[_]: Concurrent](
      uriPattern: String,
      name: String,
      description: String = ""
  )(handler: String => F[ResourceContent]): Resources[F] =
    new Resources[F]:
      private val resourceTemplate = ResourceTemplate(
        uriTemplate = uriPattern,
        name = name,
        description = if description.isEmpty then None else Some(description)
      )

      // Compile the match pattern once. A URI template like "test://template/{id}/data" becomes
      // a regex with `{...}` placeholders turned into `[^/]+` segments; everything between
      // placeholders is quoted so regex metacharacters in the pattern (`?`, `+`, `(`, ...)
      // match literally.
      private val templateRegex =
        uriPattern
          .split("\\{[^}]+\\}", -1)
          .map(scala.util.matching.Regex.quote)
          .mkString("[^/]+")
          .r

      def list: F[List[Resource]]                  = Applicative[F].pure(Nil)
      def listTemplates: F[List[ResourceTemplate]] = Applicative[F].pure(List(resourceTemplate))
      def read(uri: String): OptionT[F, ResourceContent] =
        if templateRegex.matches(uri) then OptionT.liftF(handler(uri))
        else OptionT.none[F, ResourceContent]
      def changes: Stream[F, String] = Stream.empty

/** Internal resource factory. Use `Resource` from `import mcp4s.server.dsl.*` instead. */
private[server] object McpResource:

  /** Create a static text resource */
  def apply[F[_]: Concurrent](uri: String, name: String)(content: => String): Resources[F] =
    val resource = Resource(uri, name, mimeType = Some("text/plain"))
    single(resource)(_ => Concurrent[F].pure(ResourceContent.text(uri, content)))

  /** Create a resource with a handler */
  def handler[F[_]: Concurrent](uri: String, name: String, mimeType: String = "text/plain")(
      handler: String => F[ResourceContent]
  ): Resources[F] =
    val resource = Resource(uri, name, mimeType = Some(mimeType))
    single(resource)(handler)

  /** Create a resource from a Resource definition and handler */
  def single[F[_]: Concurrent](resource: Resource)(
      handler: String => F[ResourceContent]
  ): Resources[F] =
    new Resources[F]:
      def list: F[List[Resource]]                  = Applicative[F].pure(List(resource))
      def listTemplates: F[List[ResourceTemplate]] = Applicative[F].pure(Nil)
      private[server] override val exactReads      = Some(Map(resource.uri -> handler))
      def read(uri: String): OptionT[F, ResourceContent] =
        if uri == resource.uri then OptionT.liftF(handler(uri))
        else OptionT.none[F, ResourceContent]
      def changes: Stream[F, String] = Stream.empty

  /** Create a subscribable resource from a change stream.
    *
    * The returned `Resources` carries a change stream that emits the URI whenever the resource
    * content changes. Use this for resources that need to notify subscribers of updates.
    *
    * Example:
    * {{{
    * val watched = Resource.subscribable[IO](
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
  )(readHandler: String => F[ResourceContent]): Resources[F] =
    new Resources[F]:
      private val resource        = Resource(uri, name, mimeType = Some("text/plain"))
      def list: F[List[Resource]] = Applicative[F].pure(List(resource))
      def listTemplates: F[List[ResourceTemplate]] = Applicative[F].pure(Nil)
      private[server] override val exactReads      = Some(Map(uri -> readHandler))
      def read(reqUri: String): OptionT[F, ResourceContent] =
        if reqUri == uri then OptionT.liftF(readHandler(reqUri))
        else OptionT.none[F, ResourceContent]
      def changes: Stream[F, String]          = changeStream.as(uri)
      override def supportsSubscribe: Boolean = true

  /** Create a subscribable resource that polls for changes.
    *
    * The resource will be checked periodically and change notifications emitted when `hasChanged`
    * returns true.
    *
    * Example:
    * {{{
    * val metrics = Resource.polling[IO](
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
  )(readHandler: String => F[ResourceContent]): Resources[F] =
    val changeStream = Stream
      .awakeEvery[F](pollInterval)
      .evalFilter(_ => hasChanged)
      .void
    subscribable(uri, name, changeStream)(readHandler)
