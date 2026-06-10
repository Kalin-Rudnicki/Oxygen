package oxygen.executable

import oxygen.cli.*
import zio.*

final case class ExampleApp(
) extends CliApp[Any, String] {

  def env(
      @named host: String = "localhost",
  ): EnvLayer =
    ZLayer.succeed { host }

  @command
  def client(
  ): Effect =
    ZIO.logInfo("Hello Client!")

  @command
  def server(
      @named port: Int = 8080,
  ): Effect =
    ZIO.logInfo(s"Hello Server! (port: $port)")

  @command
  def nested1: Nested1 = Nested1()

  @command
  def nested2(@named n2: Double): Nested2 = Nested2(n2)

  @command
  def nested3(@positional @longName("n-3") n3: Double): Nested2 = Nested2(n3)

}
object ExampleApp extends CliApp.Executable[ExampleApp]

final case class Nested1(
) extends CliApp[String, Any] {

  @execute
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