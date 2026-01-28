package mcp4s.postgres

import cats.effect.*
import cats.effect.std.Console
import fs2.io.net.Network
import org.typelevel.otel4s.trace.Tracer
import mcp4s.server.transport.*

object PostgresStdio extends IOApp:

  def run(args: List[String]): IO[ExitCode] =
    given Tracer[IO]  = Tracer.noop[IO]
    given Network[IO] = Network.forIO
    given Console[IO] = Console.make[IO]

    val program = for
      config <- Resource.eval(PostgresConfig.fromEnv[IO])
      server <- PostgresServer.resource[IO](config)
    yield server

    program
      .use(server => StdioTransport.run[IO](server))
      .as(ExitCode.Success)
      .handleErrorWith { err =>
        IO.consoleForIO.errorln(s"Error: ${err.getMessage}").as(ExitCode.Error)
      }
