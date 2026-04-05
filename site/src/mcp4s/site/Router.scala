package mcp4s.site

import org.scalajs.dom

object Router {

  def init(): Unit = {
    dom.window.onhashchange = (_: dom.HashChangeEvent) => navigate()
    navigate()
  }

  private def navigate(): Unit = {
    val hash = dom.window.location.hash.stripPrefix("#")
    ContentLoader.load(hash, Layout.contentContainer)
  }
}
