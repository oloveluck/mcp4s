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
      version = Some("0.1.8")
    )
    .all.themeColors(
      primary = Color.hex("d4d4d8"),
      secondary = Color.hex("d4d4d8"),
      primaryMedium = Color.hex("3f3f46"),
      primaryLight = Color.hex("18181b"),
      text = Color.hex("d4d4d8"),
      background = Color.hex("09090b"),
      bgGradient = (Color.hex("09090b"), Color.hex("18181b"))
    )
    .site.darkMode.themeColors(
      primary = Color.hex("d4d4d8"),
      secondary = Color.hex("d4d4d8"),
      primaryMedium = Color.hex("3f3f46"),
      primaryLight = Color.hex("18181b"),
      text = Color.hex("d4d4d8"),
      background = Color.hex("09090b"),
      bgGradient = (Color.hex("09090b"), Color.hex("18181b"))
    )
    .all.syntaxHighlightingColors(
      base = ColorQuintet(
        Color.hex("18181b"), Color.hex("52525b"), Color.hex("71717a"),
        Color.hex("a1a1aa"), Color.hex("d4d4d8")
      ),
      wheel = ColorQuintet(
        Color.hex("d4d4d8"), Color.hex("d4d4d8"), Color.hex("a1a1aa"),
        Color.hex("a1a1aa"), Color.hex("71717a")
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
      .use: transformer =>
        transformer
          .fromDirectory(inputDir)
          .toDirectory(outputDir)
          .transform
      .as(ExitCode.Success)
