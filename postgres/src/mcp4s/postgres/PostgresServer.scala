package mcp4s.postgres

import cats.effect.{Async, Resource}
import cats.effect.std.Console
import cats.syntax.all.*
import fs2.io.net.Network
import io.circe.*
import mcp4s.protocol.*
import mcp4s.server.*
import org.typelevel.otel4s.trace.Tracer.Implicits.noop
import skunk.*
import skunk.codec.all.*
import skunk.implicits.*

object PostgresServer:

  val DefaultMaxRows: Int = 1000

  def resource[F[_]: Async: Network: Console](
      config: PostgresConfig
  ): Resource[F, McpServer[F]] =
    sessionPool(config).map(pool => build(pool, config))

  private def sessionPool[F[_]: Async: Network: Console](
      config: PostgresConfig
  ): Resource[F, Resource[F, Session[F]]] =
    val builder = Session
      .Builder[F]
      .withHost(config.host)
      .withPort(config.port)
      .withDatabase(config.database)
    config.password
      .fold(builder.withUser(config.user))(pw => builder.withUserAndPassword(config.user, pw))
      .pooled(config.maxConnections)

  private def build[F[_]: Async](
      pool: Resource[F, Session[F]],
      config: PostgresConfig
  ): McpServer[F] =
    McpServer
      .builder[F]
      .withInfo(ServerInfo("postgres-server", "1.0.0"))
      .withTool(queryTool, queryHandler(pool))
      .withResource(schemaResource(config), schemaHandler(pool))
      .build

  private val queryTool: Tool = Tool(
    name = "query",
    description = Some(
      """Execute a read-only SQL query against the PostgreSQL database.
        |
        |Results are returned as a markdown table by default.
        |Maximum 1000 rows returned. Use LIMIT for large tables.
        |
        |Examples:
        |  - SELECT * FROM users LIMIT 10
        |  - SELECT name, email FROM users WHERE active = true
        |  - SELECT COUNT(*) FROM orders""".stripMargin
    ),
    inputSchema = JsonSchema.obj(
      properties = Map(
        "sql" -> JsonSchema.string(Some("The SQL query to execute")),
        "format" -> JsonSchema.stringEnum(
          List("table", "json"),
          Some("Output format: 'table' (default) or 'json'")
        ),
        "limit" -> JsonSchema.number(Some("Maximum rows to return (default/max: 1000)"))
      ),
      required = List("sql")
    ),
    annotations = Some(
      ToolAnnotations(
        readOnlyHint = Some(true),
        destructiveHint = Some(false),
        idempotentHint = Some(true),
        openWorldHint = Some(true)
      )
    )
  )

  private def queryHandler[F[_]: Async](
      pool: Resource[F, Session[F]]
  ): Json => F[ToolResult] = args => {
    val cursor = args.hcursor

    val result = for
      sql    <- cursor.get[String]("sql")
      format <- cursor.get[Option[String]]("format")
      limit  <- cursor.get[Option[Int]]("limit")
    yield (sql, format.getOrElse("table"), limit.getOrElse(DefaultMaxRows).min(DefaultMaxRows))

    result.fold(
      err => Async[F].pure(ToolResult.error(s"Invalid arguments: ${err.getMessage}")),
      { case (sql, format, limit) =>
        executeQuery(pool, sql, format, limit).handleError { err =>
          ToolResult.error(s"Query error: ${err.getMessage}")
        }
      }
    )
  }

  private def executeQuery[F[_]: Async](
      pool: Resource[F, Session[F]],
      sql: String,
      format: String,
      limit: Int
  ): F[ToolResult] =
    val trimmed = sql.trim.toUpperCase
    if !isReadOnlyStatement(trimmed) then
      Async[F].pure(
        ToolResult.error(
          "Only SELECT, EXPLAIN, SHOW, WITH, and TABLE statements are allowed."
        )
      )
    else
      pool.use { session =>
        session.transaction.use { _ =>
          for
            _      <- session.execute(sql"SET TRANSACTION READ ONLY".command)
            result <- executeRawQuery(session, sql, limit)
          yield result match
            case Right((columns, rows)) =>
              format match
                case "json" =>
                  ToolResult(List(TextContent(QueryResult.formatAsJson(columns, rows).spaces2)))
                case _ =>
                  ToolResult.text(QueryResult.formatAsTable(columns, rows))
            case Left(errorMsg) =>
              ToolResult.error(errorMsg)
        }
      }

  private def executeRawQuery[F[_]: Async](
      session: Session[F],
      sql: String,
      limit: Int
  ): F[Either[String, (List[String], List[List[String]])]] =
    val limitedSql =
      if sql.trim.toUpperCase.contains("LIMIT") then sql
      else s"$sql LIMIT $limit"

    val fragment = sql"#$limitedSql"
    session
      .execute(fragment.query(text))
      .map { rows =>
        Right((List("result"), rows.map(r => List(r))))
      }
      .handleError(e => Left(e.getMessage))

  private def isReadOnlyStatement(sql: String): Boolean =
    sql.startsWith("SELECT") ||
      sql.startsWith("EXPLAIN") ||
      sql.startsWith("SHOW") ||
      sql.startsWith("WITH") ||
      sql.startsWith("TABLE")

  private def schemaResource(config: PostgresConfig): mcp4s.protocol.Resource =
    mcp4s.protocol.Resource(
      uri = s"postgres://${config.database}/schema",
      name = "Database Schema",
      description = Some("List of tables and columns in the database"),
      mimeType = Some("text/plain")
    )

  private def schemaHandler[F[_]: Async](
      pool: Resource[F, Session[F]]
  ): String => F[ResourceContent] = _ =>
    pool.use { session =>
      val schemaQuery = sql"""
        SELECT
          table_name,
          column_name,
          data_type,
          is_nullable
        FROM information_schema.columns
        WHERE table_schema = 'public'
        ORDER BY table_name, ordinal_position
      """.query(varchar *: varchar *: varchar *: varchar)

      session.execute(schemaQuery).map { rows =>
        val content = rows
          .groupBy(_._1)
          .toList
          .sortBy(_._1)
          .map { case (table, cols) =>
            val columnDefs = cols
              .map { case (_, col, dtype, nullable) =>
                s"  - $col: $dtype${if nullable == "YES" then " (nullable)" else ""}"
              }
              .mkString("\n")
            s"## $table\n$columnDefs"
          }
          .mkString("\n\n")

        ResourceContent.text(
          s"postgres://schema",
          if content.isEmpty then "No tables found in public schema." else content,
          Some("text/markdown")
        )
      }
    }
