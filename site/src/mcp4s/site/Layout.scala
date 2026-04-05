package mcp4s.site

import scalatags.JsDom.all.*
import org.scalajs.dom
import scala.scalajs.js

object Layout {

  val contentContainer: dom.html.Div =
    div(cls := "content").render

  def mount(): Unit = {
    val app = dom.document.getElementById("app")
    val sidebarEl = Sidebar.render()
    val backdropEl = div(cls := "sidebar-backdrop").render
    val layout = div(cls := "layout")(
      headerBar,
      div(cls := "main")(
        backdropEl,
        sidebarEl,
        contentContainer
      )
    ).render
    val _ = app.appendChild(layout)
    initTheme()
    initMobileMenu(sidebarEl, backdropEl)
  }

  private val githubSvg: Frag = raw(
    """<svg viewBox="0 0 16 16" width="22" height="22" fill="currentColor" aria-hidden="true"><path d="M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.013 8.013 0 0016 8c0-4.42-3.58-8-8-8z"></path></svg>"""
  )

  private val moonSvg: Frag = raw(
    """<svg class="icon-moon" viewBox="0 0 24 24" width="20" height="20" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M21 12.79A9 9 0 1 1 11.21 3 7 7 0 0 0 21 12.79z"></path></svg>"""
  )

  private val sunSvg: Frag = raw(
    """<svg class="icon-sun" viewBox="0 0 24 24" width="20" height="20" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><circle cx="12" cy="12" r="5"></circle><line x1="12" y1="1" x2="12" y2="3"></line><line x1="12" y1="21" x2="12" y2="23"></line><line x1="4.22" y1="4.22" x2="5.64" y2="5.64"></line><line x1="18.36" y1="18.36" x2="19.78" y2="19.78"></line><line x1="1" y1="12" x2="3" y2="12"></line><line x1="21" y1="12" x2="23" y2="12"></line><line x1="4.22" y1="19.78" x2="5.64" y2="18.36"></line><line x1="18.36" y1="5.64" x2="19.78" y2="4.22"></line></svg>"""
  )

  private val headerBar: Frag =
    scalatags.JsDom.all.header(cls := "header")(
      div(cls := "header-inner")(
        button(cls := "menu-toggle", attr("aria-label") := "Toggle navigation")(
          span(),
          span(),
          span()
        ),
        a(href := "#", cls := "header-brand")("mcp4s"),
        span(cls := "header-tagline")("Model Context Protocol for Scala"),
        div(cls := "header-actions")(
          button(cls := "theme-toggle", attr("aria-label") := "Toggle dark mode")(
            moonSvg,
            sunSvg
          ),
          a(
            href := "https://github.com/oloveluck/mcp4s",
            cls := "header-github",
            attr("target") := "_blank",
            attr("rel") := "noopener",
            attr("aria-label") := "GitHub repository"
          )(githubSvg)
        )
      )
    )

  private def initTheme(): Unit = {
    val stored = js.Dynamic.global.localStorage.getItem("theme")
    if (!js.isUndefined(stored) && stored != null)
      dom.document.documentElement.setAttribute("data-theme", stored.asInstanceOf[String])

    val toggleBtn = dom.document.querySelector(".theme-toggle")
    if (toggleBtn != null) {
      toggleBtn.addEventListener("click", (_: dom.MouseEvent) => {
        val html = dom.document.documentElement
        val current = html.getAttribute("data-theme")
        val isDark = if (current == "dark") true
          else if (current == "light") false
          else js.Dynamic.global.matchMedia("(prefers-color-scheme: dark)").matches.asInstanceOf[Boolean]

        val next = if (isDark) "light" else "dark"
        html.setAttribute("data-theme", next)
        js.Dynamic.global.localStorage.setItem("theme", next)
      })
    }
  }

  private def initMobileMenu(sidebar: dom.Element, backdrop: dom.Element): Unit = {
    val menuBtn = dom.document.querySelector(".menu-toggle")
    if (menuBtn != null) {
      menuBtn.addEventListener("click", (_: dom.MouseEvent) => {
        sidebar.classList.toggle("open")
        backdrop.classList.toggle("active")
      })
    }
    backdrop.addEventListener("click", (_: dom.MouseEvent) => {
      sidebar.classList.remove("open")
      backdrop.classList.remove("active")
    })
  }
}
