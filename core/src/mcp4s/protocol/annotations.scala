package mcp4s.protocol

import scala.annotation.StaticAnnotation

/** Annotation to describe a field for JSON schema generation.
  *
  * Example:
  * {{{
  * case class AddArgs(
  *   @description("First number to add") a: Double,
  *   @description("Second number to add") b: Double
  * ) derives ToolInput
  * }}}
  */
final class description(val value: String) extends StaticAnnotation
