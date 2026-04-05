package mcp4s.site

import org.scalajs.dom
import scala.scalajs.js

object ContentLoader {

  def load(hash: String, container: dom.html.Div): Unit = {
    val path = resolvePath(hash)
    val _ = dom.fetch(path).`then`[Unit] { response =>
      if (response.ok) {
        val _ = response.text().`then`[Unit] { text =>
          container.innerHTML = Marked.parse(text)
          fixInternalLinks(container, hash)
          highlightCode(container)
          addCopyButtons(container)
          addHeadingAnchors(container)
          styleAlerts(container)
          updatePageTitle(container)
          updateActiveLink(hash)
          closeMobileMenu()
          container.scrollTop = 0
        }
      } else {
        container.innerHTML = s"<h1>Page not found</h1><p>Could not load <code>$path</code></p>"
      }
    }
  }

  private def resolvePath(hash: String): String = {
    val clean = hash.stripPrefix("#").stripPrefix("/")
    if (clean.isEmpty) "content/index.md"
    else if (clean.contains("/")) s"content/$clean.md"
    else s"content/$clean/index.md"
  }

  /** Rewrite relative `<a>` hrefs to hash-based paths so internal links work with
    * the SPA hash router. E.g. on page `#server/tools`, a link to `resources` becomes
    * `#server/resources`, and `../client` becomes `#client`.
    */
  private def fixInternalLinks(container: dom.html.Div, hash: String): Unit = {
    val links = container.querySelectorAll("a[href]")
    val currentDir = {
      val clean = hash.stripPrefix("#").stripPrefix("/")
      val idx = clean.lastIndexOf('/')
      if (idx >= 0) clean.substring(0, idx) else ""
    }
    var i = 0
    while (i < links.length) {
      val anchor = links.item(i).asInstanceOf[dom.html.Anchor]
      val href = anchor.getAttribute("href")
      if (href != null && !href.startsWith("http") && !href.startsWith("#") &&
          !href.startsWith("javascript:") && !href.startsWith("mailto:")) {
        // Resolve relative path against the current page's directory
        val parts = if (currentDir.isEmpty) href.split("/").toList else (currentDir + "/" + href).split("/").toList
        val resolved = parts.foldLeft(List.empty[String]) { (acc, seg) =>
          if (seg == "..") acc.dropRight(1)
          else if (seg == "." || seg.isEmpty) acc
          else acc :+ seg
        }
        anchor.setAttribute("href", "#" + resolved.mkString("/"))
      }
      i += 1
    }
  }

  private def highlightCode(container: dom.html.Div): Unit = {
    val hljs = js.Dynamic.global.hljs
    if (!js.isUndefined(hljs)) {
      val blocks = container.querySelectorAll("pre code")
      var i = 0
      while (i < blocks.length) {
        val _ = hljs.highlightElement(blocks.item(i))
        i += 1
      }
    }
  }

  private def addCopyButtons(container: dom.html.Div): Unit = {
    val pres = container.querySelectorAll("pre")
    var i = 0
    while (i < pres.length) {
      val pre = pres.item(i).asInstanceOf[dom.html.Element]
      val button = dom.document.createElement("button").asInstanceOf[dom.html.Button]
      button.className = "copy-btn"
      button.textContent = "Copy"
      button.onclick = (_: dom.MouseEvent) => {
        val code = pre.querySelector("code")
        dom.window.navigator.asInstanceOf[js.Dynamic].clipboard.writeText(code.textContent)
        button.textContent = "Copied!"
        val _ = dom.window.setTimeout(() => button.textContent = "Copy", 2000)
      }
      val _ = pre.appendChild(button)
      i += 1
    }
  }

  private def addHeadingAnchors(container: dom.html.Div): Unit = {
    val headings = container.querySelectorAll("h2, h3")
    var i = 0
    while (i < headings.length) {
      val heading = headings.item(i).asInstanceOf[dom.html.Element]
      val text = heading.textContent.trim
      val slug = text.toLowerCase
        .replaceAll("[^a-z0-9\\s-]", "")
        .replaceAll("\\s+", "-")
        .replaceAll("-+", "-")
        .stripPrefix("-").stripSuffix("-")
      heading.id = slug
      val anchor = dom.document.createElement("a").asInstanceOf[dom.html.Anchor]
      anchor.className = "heading-anchor"
      anchor.textContent = "#"
      anchor.href = "javascript:void(0)"
      anchor.setAttribute("aria-label", s"Copy link to $text")
      anchor.onclick = (_: dom.MouseEvent) => {
        dom.window.navigator.asInstanceOf[js.Dynamic].clipboard.writeText(
          s"${dom.window.location.href.split("#")(0)}#${dom.window.location.hash.stripPrefix("#")}#$slug"
        )
      }
      heading.insertBefore(anchor, heading.firstChild)
      i += 1
    }
  }

  private def styleAlerts(container: dom.html.Div): Unit = {
    val blockquotes = container.querySelectorAll("blockquote")
    var i = 0
    while (i < blockquotes.length) {
      val bq = blockquotes.item(i).asInstanceOf[dom.html.Element]
      val strong = bq.querySelector("strong")
      if (strong != null) {
        val label = strong.textContent.trim.toLowerCase
        if (label.startsWith("note"))
          bq.classList.add("alert-note")
        else if (label.startsWith("warning"))
          bq.classList.add("alert-warning")
        else if (label.startsWith("tip"))
          bq.classList.add("alert-tip")
      }
      i += 1
    }
  }

  private def updatePageTitle(container: dom.html.Div): Unit = {
    val h1 = container.querySelector("h1")
    if (h1 != null) dom.document.title = s"${h1.textContent} - mcp4s"
    else dom.document.title = "mcp4s"
  }

  private def updateActiveLink(hash: String): Unit = {
    val links = dom.document.querySelectorAll(".sidebar-link")
    var i = 0
    while (i < links.length) {
      val link = links.item(i).asInstanceOf[dom.html.Anchor]
      val linkHash = link.getAttribute("href").stripPrefix("#")
      if (linkHash == hash)
        link.classList.add("active")
      else
        link.classList.remove("active")
      i += 1
    }
  }

  private def closeMobileMenu(): Unit = {
    val sidebar = dom.document.querySelector(".sidebar")
    val backdrop = dom.document.querySelector(".sidebar-backdrop")
    if (sidebar != null) sidebar.classList.remove("open")
    if (backdrop != null) backdrop.classList.remove("active")
  }
}
