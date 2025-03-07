package oxygen.executable

import oxygen.cli.*
import oxygen.predef.core.*
import oxygen.zio.*
import oxygen.zio.typeclass.ErrorLogger
import zio.*
import zio.json.*
import zio.json.ast.Json

sealed trait Executable {

  // TODO (KR) :

}
object Executable {;

  trait Single extends Executable {

    protected type JsonConfig
    protected type CLIConfig

    protected type Env
    protected type Error

    protected implicit val jsonDecoder: JsonDecoder[JsonConfig]
    protected implicit val errorLogger: ErrorLogger[Error]
    protected implicit val envTag: EnvironmentTag[Env]
    protected val cliParser: Parser[CommandLineConfig]

    protected def logger(jsonConfig: JsonConfig, cliConfig: CLIConfig): RIO[Scope, OxygenEnv.LoggerEnv]
    protected def telemetry(jsonConfig: JsonConfig, cliConfig: CLIConfig): RIO[Scope, OxygenEnv.TelemetryEnv] = ZIO.succeed(OxygenEnv.TelemetryEnv(targets = Chunk.empty))

    protected def env(jsonConfig: JsonConfig, cliConfig: CLIConfig): ZLayer[Scope, Error, Env]

    // TODO (KR) :

    def doTheThing(config: Json, values: List[Arg.ValueLike], params: List[Arg.ParamLike]): ZIO[Scope, Any, ExitCode] =
      ??? // TODO (KR) :;

  }

  final case class Many(options: NonEmptyList[(String, Executable)]) extends Executable {

    // TODO (KR) :

  }

}
