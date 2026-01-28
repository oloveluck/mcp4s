package mcp4s.postgres

import cats.effect.Sync
import cats.syntax.all.*

final case class PostgresConfig(
    host: String,
    port: Int,
    database: String,
    user: String,
    password: Option[String],
    maxConnections: Int = 10
)

object PostgresConfig:

  def fromEnv[F[_]: Sync]: F[PostgresConfig] =
    for
      host    <- envOrDefault("PGHOST", "localhost")
      portStr <- envOrDefault("PGPORT", "5432")
      port <- Sync[F].fromEither(
        portStr.toIntOption.toRight(
          new IllegalArgumentException(s"Invalid PGPORT: $portStr")
        )
      )
      database    <- envRequired("PGDATABASE")
      user        <- envRequired("PGUSER")
      password    <- env("PGPASSWORD")
      maxConnsStr <- envOrDefault("PG_MAX_CONNECTIONS", "10")
      maxConns <- Sync[F].fromEither(
        maxConnsStr.toIntOption.toRight(
          new IllegalArgumentException(s"Invalid PG_MAX_CONNECTIONS: $maxConnsStr")
        )
      )
    yield PostgresConfig(host, port, database, user, password, maxConns)

  private def env[F[_]: Sync](name: String): F[Option[String]] =
    Sync[F].delay(sys.env.get(name))

  private def envRequired[F[_]: Sync](name: String): F[String] =
    env(name).flatMap {
      case Some(v) => Sync[F].pure(v)
      case None =>
        Sync[F].raiseError(
          new IllegalArgumentException(s"Required environment variable $name not set")
        )
    }

  private def envOrDefault[F[_]: Sync](name: String, default: String): F[String] =
    env(name).map(_.getOrElse(default))
