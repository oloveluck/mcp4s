import sbt._

/** Extracts ```scala fences from every markdown page under docs/content into compilable managed
  * test sources for the examples module, so `examples/Test/compile` verifies that every
  * documentation snippet still matches the published API.
  *
  * All snippets in one page share a single object scope (docs snippets build on each other), so
  * later blocks see earlier definitions. Markers, placed as an HTML comment on the line directly
  * above a fence: <!-- doc-snippet: skip --> never compiled (signature listings, pseudo-code) <!--
  * doc-snippet: reset --> start a fresh scope (page redefines earlier names)
  */
object DocSnippets {

  private val Directive = """<!--\s*doc-snippet:\s*(\w+)\s*-->""".r

  private def fileHeader(md: String, pkg: String): String =
    s"""// GENERATED from $md - do not edit; edit the markdown, then sbt examples/Test/compile
       |package docsnippets.$pkg
       |
       |import cats.effect.{IO, IOApp, Resource}
       |import cats.syntax.all.*
       |import com.comcast.ip4s.*
       |import fs2.Stream
       |import io.circe.Json
       |import org.typelevel.otel4s.trace.Tracer
       |import mcp4s.protocol.*
       |import mcp4s.server.{McpServer, Prompts, Resources, Server, ServiceRoutes, ToolContext, Tools}
       |import mcp4s.server.transport.{HttpConfig, SessionConfig, WebSocketConfig}
       |import mcp4s.client.{McpClient, McpClientBuilder, McpConnection}
       |import mcp4s.client.transport.{HttpTransportConfig, McpAuth, StdioTransportConfig, WebSocketTransportConfig}
       |import mcp4s.transport.Timeouts
       |
       |object stubs:
       |  def conn: McpConnection[IO]                  = ???
       |  def connection: McpConnection[IO]            = ???
       |  def httpClient: org.http4s.client.Client[IO] = ???
       |  def server: Server[IO]                       = ???
       |  def client: McpClient[IO]                    = ???
       |""".stripMargin

  final private case class Snippet(mode: String, code: Vector[String], mdLine: Int)

  /** A directive survives blank lines, is cancelled by any other non-blank line, and is consumed by
    * the next ```scala fence.
    */
  private def snippetsOf(lines: Vector[String]): List[Snippet] = {
    val out                       = List.newBuilder[Snippet]
    var directive: Option[String] = None
    var i                         = 0
    while (i < lines.length) {
      val stripped = lines(i).trim
      Directive.findPrefixMatchOf(stripped) match {
        case Some(m) => directive = Some(m.group(1))
        case None    =>
          if (stripped == "```scala") {
            var j = i + 1
            while (j < lines.length && lines(j).trim != "```") j += 1
            out += Snippet(directive.getOrElse("compile"), lines.slice(i + 1, j), i + 2)
            directive = None
            i = j
          } else if (stripped.nonEmpty) directive = None
      }
      i += 1
    }
    out.result()
  }

  /** None when the page has no compiled snippets (only skips, or none at all). */
  private def render(rel: String, pkg: String, snippets: List[Snippet]): Option[String] = {
    val parts = Vector.newBuilder[String]
    parts += fileHeader(rel, pkg)
    var scope     = 0
    var openScope = false
    snippets.filter(_.mode != "skip").foreach { s =>
      if (s.mode == "reset" || !openScope) {
        scope += 1
        openScope = true
        parts += s"object scope_$scope:"
        parts += "  import stubs.{*, given}"
      }
      parts += s"  // ---- snippet at line ${s.mdLine}"
      parts ++= s.code.map(c => if (c.trim.nonEmpty) "  " + c else "")
      parts += ""
    }
    if (openScope) Some(parts.result().mkString("\n") + "\n") else None
  }

  /** Idempotent: rewrites only files whose content changed (stable mtimes keep Zinc quiet) and
    * prunes outputs whose source page disappeared. Returns every generated file, as the
    * sourceGenerators contract requires.
    */
  def generate(docsDir: File, outDir: File, log: Logger): Seq[File] = {
    IO.createDirectory(outDir)
    val mdFiles   = (docsDir ** "*.md").get.sortBy(_.getAbsolutePath)
    val generated = mdFiles.flatMap { md =>
      val rel = IO.relativize(docsDir, md).getOrElse(md.getName).replace('\\', '/')
      val pkg = rel.stripSuffix(".md").replaceAll("[^A-Za-z0-9]", "_")
      render(rel, pkg, snippetsOf(IO.readLines(md).toVector)).map { content =>
        val out = outDir / (pkg + ".scala")
        if (!out.exists || IO.read(out) != content) IO.write(out, content)
        out
      }
    }
    val keep = generated.map(_.getName).toSet
    IO.listFiles(outDir, "*.scala").filterNot(f => keep(f.getName)).foreach(IO.delete(_))
    log.info(
      s"DocSnippets: ${generated.size} pages with compiled snippets (of ${mdFiles.size} markdown files)"
    )
    generated
  }
}
