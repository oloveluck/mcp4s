package mcp4s.docs

import cats.effect.{ExitCode, IO, IOApp}
import laika.api.Transformer
import laika.ast.LengthUnit.px
import laika.ast.Path.Root
import laika.config.SyntaxHighlighting
import laika.format.{HTML, Markdown}
import laika.helium.Helium
import laika.helium.config.*
import laika.io.syntax.*
import laika.theme.config.Color

object Main extends IOApp:

  private val theme = Helium.defaults
    .all.metadata(
      title = Some("mcp4s"),
      description = Some("MCP (Model Context Protocol) for Scala"),
      language = Some("en"),
      version = Some("0.1.6")
    )
    .all.themeColors(
      primary = Color.hex("1a5276"),
      secondary = Color.hex("2980b9"),
      primaryMedium = Color.hex("5dade2"),
      primaryLight = Color.hex("eaf2f8"),
      text = Color.hex("2c3e50"),
      background = Color.hex("ffffff"),
      bgGradient = (Color.hex("0e3d5c"), Color.hex("1a5276"))
    )
    .all.syntaxHighlightingColors(
      base = ColorQuintet(
        Color.hex("f8f8f8"), Color.hex("6a737d"), Color.hex("24292e"),
        Color.hex("24292e"), Color.hex("24292e")
      ),
      wheel = ColorQuintet(
        Color.hex("d73a49"), Color.hex("e36209"), Color.hex("6f42c1"),
        Color.hex("22863a"), Color.hex("005cc5")
      )
    )
    .site.fontSizes(
      body = px(16),
      code = px(14),
      title = px(34),
      header2 = px(28),
      header3 = px(20),
      header4 = px(16),
      small = px(12)
    )
    .site.topNavigationBar(
      homeLink = IconLink.internal(Root / "README.md", HeliumIcon.home),
      navLinks = Seq(
        IconLink.external("https://github.com/oloveluck/mcp4s", HeliumIcon.github)
      )
    )
    .site.mainNavigation(depth = 3)
    .site.landingPage(
      title = Some("mcp4s"),
      subtitle = Some("Type-safe MCP (Model Context Protocol) for Scala, powered by Cats Effect and fs2"),
      latestReleases = Seq(
        ReleaseInfo("Latest Release", "0.1.6")
      ),
      license = Some("Apache-2.0"),
      projectLinks = Seq(
        ButtonLink.internal(Root / "getting-started" / "README.md", "Get Started"),
        ButtonLink.external("https://central.sonatype.com/artifact/io.github.oloveluck/mcp4s-core_3", "Maven Central")
      ),
      teasers = Seq(
        Teaser("Type-Safe Tools", "Define MCP tools as plain Scala case classes with automatic schema derivation. Compile-time safety with zero boilerplate."),
        Teaser("Cats Effect & fs2", "Built on Cats Effect for safe concurrency and fs2 for streaming. Fully functional, resource-safe, and composable."),
        Teaser("Multiple Transports", "Connect via stdio, HTTP with SSE, or WebSocket. Switch transports without changing your application code."),
        Teaser("Client Resilience", "Automatic reconnection with configurable backoff. Transparent session recovery keeps your client connected.")
      )
    )
    .site.footer(
      """mcp4s is a <a href="https://typelevel.org/">Typelevel</a>-adjacent project, built with Cats Effect and fs2."""
    )
    .build

  def run(args: List[String]): IO[ExitCode] =
    val inputDir = System.getProperty("user.dir") + "/docs/content"
    val outputDir =
      if args.nonEmpty then args.head
      else System.getProperty("user.dir") + "/out/docs/site"

    Transformer
      .from(Markdown)
      .to(HTML)
      .using(Markdown.GitHubFlavor, SyntaxHighlighting)
      .parallel[IO]
      .withTheme(theme)
      .build
      .use { transformer =>
        transformer
          .fromDirectory(inputDir)
          .toDirectory(outputDir)
          .transform
      }
      .as(ExitCode.Success)
