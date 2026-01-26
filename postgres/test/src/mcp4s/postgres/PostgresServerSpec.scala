package mcp4s.postgres

import munit.CatsEffectSuite

class PostgresServerSpec extends CatsEffectSuite:

  test("QueryResult.formatAsTable formats empty result") {
    val result = QueryResult.formatAsTable(List("id", "name"), Nil)
    assert(result.contains("0 rows"))
    assert(result.contains("id, name"))
  }

  test("QueryResult.formatAsTable formats rows as markdown table") {
    val columns = List("id", "name")
    val rows = List(List("1", "Alice"), List("2", "Bob"))
    val result = QueryResult.formatAsTable(columns, rows)

    assert(result.contains("id | name"))
    assert(result.contains("--- | ---"))
    assert(result.contains("1 | Alice"))
    assert(result.contains("2 | Bob"))
    assert(result.contains("2 rows"))
  }

  test("QueryResult.formatAsTable handles single row") {
    val columns = List("count")
    val rows = List(List("42"))
    val result = QueryResult.formatAsTable(columns, rows)

    assert(result.contains("1 row)"))
    assert(!result.contains("rows)"))
  }

  test("QueryResult.formatAsJson produces valid structure") {
    val columns = List("id", "name")
    val rows = List(List("1", "Alice"))
    val json = QueryResult.formatAsJson(columns, rows)

    assertEquals(json.hcursor.get[List[String]]("columns"), Right(columns))
    assertEquals(json.hcursor.get[Int]("rowCount"), Right(1))
  }

  test("QueryResult.formatAsJson handles empty rows") {
    val columns = List("id")
    val rows = List.empty[List[String]]
    val json = QueryResult.formatAsJson(columns, rows)

    assertEquals(json.hcursor.get[Int]("rowCount"), Right(0))
  }
