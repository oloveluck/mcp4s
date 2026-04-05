package mcp4s.site

object Main {

  def main(args: Array[String]): Unit = {
    Layout.mount()
    Router.init()
  }
}
