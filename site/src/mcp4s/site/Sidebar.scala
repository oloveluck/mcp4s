package mcp4s.site

import scalatags.JsDom.all.*
import scalatags.JsDom.tags2.nav
import org.scalajs.dom

case class Page(title: String, hash: String)
case class Section(title: String, index: Option[Page], pages: List[Page])

object Sidebar {

  val sections: List[Section] = List(
    Section(
      "Getting Started",
      Some(Page("Overview", "getting-started")),
      List(
        Page("First Server", "getting-started/first-server"),
        Page("First Client", "getting-started/first-client")
      )
    ),
    Section(
      "Server Guide",
      Some(Page("Overview", "server")),
      List(
        Page("Tools", "server/tools"),
        Page("Resources", "server/resources"),
        Page("Prompts", "server/prompts"),
        Page("HTTP Security", "server/auth")
      )
    ),
    Section(
      "Client Guide",
      Some(Page("Overview", "client")),
      List(
        Page("Connection", "client/connection"),
        Page("Resilience", "client/resilience")
      )
    ),
    Section(
      "Transports",
      Some(Page("Overview", "transports")),
      List(
        Page("HTTP", "transports/http"),
        Page("WebSocket", "transports/websocket"),
        Page("Stdio", "transports/stdio")
      )
    ),
    Section(
      "Advanced",
      Some(Page("Overview", "advanced")),
      List(
        Page("Architecture", "advanced/architecture"),
        Page("Tasks", "advanced/tasks"),
        Page("Bidirectional", "advanced/bidirectional")
      )
    ),
    Section(
      "Reference",
      Some(Page("Overview", "reference")),
      List(
        Page("Protocol", "reference/protocol"),
        Page("Errors", "reference/errors"),
        Page("Type Derivation", "reference/type-derivation"),
        Page("Testing", "reference/testing")
      )
    ),
    Section(
      "Project",
      None,
      List(
        Page("Contributing", "project/contributing"),
        Page("Changelog", "project/changelog"),
        Page("Version Support", "project/version-support")
      )
    )
  )

  def render(): dom.Element = {
    val children: Seq[Frag] =
      Seq[Frag](a(href := "#", cls := "sidebar-brand")("mcp4s")) ++ sections.map(renderSection)
    nav(cls := "sidebar")(children*).render
  }

  private def renderSection(section: Section): Frag =
    div(cls := "sidebar-section")(
      section.index match {
        case Some(idx) =>
          h3(cls := "sidebar-heading")(
            a(href := s"#${idx.hash}", cls := "sidebar-link")(section.title)
          )
        case None =>
          h3(cls := "sidebar-heading")(section.title)
      },
      ul(cls := "sidebar-pages")(
        section.pages.map { page =>
          li(a(href := s"#${page.hash}", cls := "sidebar-link")(page.title))
        }*
      )
    )
}
