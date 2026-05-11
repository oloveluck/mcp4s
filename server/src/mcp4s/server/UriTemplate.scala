package mcp4s.server

import scala.util.matching.Regex

/** Shared utility for matching URI template patterns against concrete URIs.
  *
  * Converts patterns like "api://users/{id}/data" to a regex that matches concrete URIs.
  * Used by both `Resources.template` and `BuiltServer` template handlers.
  */
private[server] object UriTemplate:

  /** Compile a URI template pattern to a Regex.
    *
    * Converts `{placeholder}` segments to `[^/]+` matchers and escapes
    * literal dots and slashes.
    */
  def compile(pattern: String): Regex =
    pattern
      .replace(".", "\\.")
      .replace("/", "\\/")
      .replaceAll("\\{[^}]+\\}", "[^/]+")
      .r

  /** Check if a concrete URI matches a URI template pattern. */
  def matches(pattern: String, uri: String): Boolean =
    compile(pattern).matches(uri)
