package oxygen.executable

import oxygen.cli.*
import oxygen.predef.core.*
import oxygen.schema.JsonSchema
import oxygen.schema.instances.given
import zio.*

final case class ExampleApp(
    @named
    @doc("Optional root-level", "configuration flag")
    myOpt: Option[String],
) extends CliApp[Any, String] {

  @doc("Provides runtime environment", "for subcommands")
  def env(
      @named host: String = "localhost",
  ): EnvLayer =
    ZLayer.succeed { host }

  @doc("Run the example client", "against a remote host")
  @command
  def client(
      @config("APP_CONFIG") cfg: ClientConfig,
      @config("OPTIONAL_CONFIG") extraCfg: Option[ClientConfig],
      @named i: Option[String],
      @named p1: List[String],
      @named p2: NonEmptyList[String],
      @named p5: (String, Int),
      @named p6: List[(Int, String)],
  ): Effect =
    ZIO.logInfo(s"Hello Client!\n$i\n$cfg\nextraCfg=$extraCfg\np1=$p1\np2=$p2\np5=$p5\np6=$p6")

  @command
  def server(
      @named port: Int = 8080,
  ): Effect =
    ZIO.serviceWithZIO[String] { host =>
      ZIO.logInfo(s"Hello Server! (host: $host, port: $port, myOpt: $myOpt)")
    }

  @command
  def nested1: Nested1 = Nested1()

  @command
  def nested2(@named n2: Double): Nested2 = Nested2(n2)

  @command
  def nested3(
      @positional @longName("n-3")
      @doc("This is some", "documentation, ya know!?")
      n3: Double,
  ): Nested2 = Nested2(n3)

}
object ExampleApp extends CliApp.Executable[ExampleApp]

final case class ClientConfig(
    p1: ClientConfig.Part1,
    name: String,
) derives JsonSchema
object ClientConfig {

  final case class Part1(
      a: Int,
      b: String,
  ) derives JsonSchema

}

final case class Nested1(
) extends CliApp[String, Any] {

  @command
  def run(): Effect =
    ZIO.logInfo("Nested1")

}

final case class Nested2(
    n2: Double,
) extends CliApp[String, Any] {

  @execute
  def run(): Effect =
    ZIO.logInfo(s"Nested2: $n2")

}
