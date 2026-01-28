package mcp4s.postgres

import io.circe.Json
import io.circe.syntax.*

object QueryResult:

  def formatAsTable(columns: List[String], rows: List[List[String]]): String =
    if rows.isEmpty then s"Query returned 0 rows.\nColumns: ${columns.mkString(", ")}"
    else
      val header    = columns.mkString(" | ")
      val separator = columns.map(_ => "---").mkString(" | ")
      val dataRows  = rows.map(_.mkString(" | ")).mkString("\n")
      s"$header\n$separator\n$dataRows\n\n(${rows.size} row${if rows.size == 1 then "" else "s"})"

  def formatAsJson(columns: List[String], rows: List[List[String]]): Json =
    Json.obj(
      "columns"  -> columns.asJson,
      "rows"     -> rows.map(row => columns.zip(row).toMap).asJson,
      "rowCount" -> rows.size.asJson
    )
