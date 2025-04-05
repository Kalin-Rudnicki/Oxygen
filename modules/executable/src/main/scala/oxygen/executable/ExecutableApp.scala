package oxygen.executable

import oxygen.cli.*
import oxygen.executable.error.ExecuteError
import oxygen.json.{EncodedThrowable, KeyedMapDecoder}
import oxygen.json.syntax.conversion.*
import oxygen.predef.core.*
import oxygen.predef.zio.*
import oxygen.zio.logger.{LogCause, LogEvent, LogTarget}
import oxygen.zio.telemetry.TelemetryTarget
import zio.{Cause, ExitCode, LogSpan, StackTrace, ZIOAppArgs, ZIOAppDefault}
import zio.json.*
import zio.json.ast.Json

trait ExecutableApp extends ZIOAppDefault {

  val executable: Executable

  val additionalLoggerParsers: Chunk[KeyedMapDecoder.Decoder[LogTarget.ConfigBuilder]] = Chunk.empty

  val additionalTelemetryParsers: Chunk[KeyedMapDecoder.Decoder[TelemetryTarget.ConfigBuilder]] = Chunk.empty

  private def parseAndExecute(args: List[String]): ZIO[Scope, ExecuteError, Unit] =
    for {
      (internalArgs, executableArgs) <- ZIO.succeed(Arg.splitOn_--(args))
      config <- Parsing.parse(ExecutableApp.Config.parser, internalArgs)
      _ <- config.initialLoggerDefault match
        case ExecutableApp.Config.InitialLoggerDefault.Oxygen => Logger.defaultToOxygen.set
        case ExecutableApp.Config.InitialLoggerDefault.ZIO    => Logger.defaultToZio.set
      parsedJsons <- ZIO.foreach(config.sources)(ExecutableApp.Config.Source.eval)
      jsonConfig = parsedJsons.foldLeft[Json](Json.Obj())(_ merge _)
      context = ExecutableContext(
        KeyedMapDecoder(LogTarget.ConfigBuilder.default ++ additionalLoggerParsers),
        KeyedMapDecoder(TelemetryTarget.ConfigBuilder.default ++ additionalTelemetryParsers),
        config,
      )
      _ <- executable(jsonConfig, executableArgs, context)
    } yield ()

  private final case class CapturedError(level: LogLevel, message: String, cause: LogCause, spans: List[LogSpan], context: Logger.LogContext, stackTrace: Option[StackTrace], code: ExitCode) {

    def removeStackTrace: CapturedError = copy(stackTrace = None)

    private def toLogEvent: LogEvent =
      LogEvent(level, message, context, cause, zio.Trace.empty, stackTrace)

    def log: UIO[Unit] =
      Logger.handleEvent(toLogEvent) @@ Logger.addSpan(spans.map(s => Logger.Span(s.label, s.startTime.some)))

  }
  private object CapturedError {

    def fromFail(error: Cause.Fail[ExecuteError]): CapturedError =
      error.value match { // TODO (KR) : use `helpType`
        case ExecuteError.Parsing.Help(help, _)           => CapturedError(LogLevel.Info, help.toString, LogCause.Empty, error.spans, error.annotations, None, ExitCode.success)
        case ExecuteError.ProgramError(message, logLevel) => CapturedError(logLevel, "", LogCause.Fail(message, error.trace.some), error.spans, error.annotations, None, ExitCode.failure)
        case e => CapturedError(LogLevel.Fatal, "", LogCause.Fail(Json.Str(e.getMessage), error.trace.some), error.spans, error.annotations, None, ExitCode.failure)
      }

    // TODO (KR) : add configurability for how these log events are displayed
    def fromCause(cause: Cause[ExecuteError]): Chunk[CapturedError] =
      cause.foldLog[Chunk[CapturedError]](
        empty0 = Chunk.empty,
        failCase0 = (e, stackTrace, spans, context) => Chunk.single(CapturedError.fromFail(Cause.Fail(e, stackTrace, spans, context))),
        dieCase0 = (e, stackTrace, spans, context) =>
          Chunk.single(CapturedError(LogLevel.Fatal, "Fiber Death", LogCause.Die(EncodedThrowable.fromThrowable(e), stackTrace.some), spans, context, None, ExitCode.failure)),
        interruptCase0 =
          (id, stackTrace, spans, context) => Chunk.single(CapturedError(LogLevel.Fatal, "Fiber Interruption", LogCause.Interrupt(id, stackTrace.some), spans, context, None, ExitCode.failure)),
      )(
        thenCase0 = _ ++ _,
        bothCase0 = _ ++ _,
        stacklessCase0 = {
          case (events, true)  => events.map(_.removeStackTrace)
          case (events, false) => events
        },
      )

  }

  private def parseExecuteAndRecover(args: List[String]): URIO[Scope, ExitCode] =
    parseAndExecute(args).exit.flatMap {
      case zio.Exit.Success(_)     => ZIO.succeed(ExitCode.success)
      case zio.Exit.Failure(cause) => ZIO.foreach(CapturedError.fromCause(cause))(e => e.log.as(e.code)).map { _.maxByOption(_.code).getOrElse(ExitCode.failure) }
    }

  override final def run: URIO[ZIOAppArgs, Unit] =
    ZIO
      .scoped { ZIOAppArgs.getArgs.flatMap { args => parseExecuteAndRecover(args.toList) } }
      .flatMap(exit)

}
object ExecutableApp {

  final case class Config(
      initialLoggerDefault: Config.InitialLoggerDefault,
      sources: List[Config.Source],
  )
  object Config {

    enum InitialLoggerDefault extends Enum[InitialLoggerDefault] { case ZIO, Oxygen }
    object InitialLoggerDefault extends Enum.Companion[InitialLoggerDefault] {

      val parser: Params[InitialLoggerDefault] =
        Params
          .`enum`(
            "default-logger",
            Defaultable.None,
            hints = List(
              "Whether to default oxygen logger to use default oxygen sources or default zio sources.",
              "Note that this will be overridden by the executables logger parser.",
            ),
          )
          .withDefault(InitialLoggerDefault.Oxygen)

    }

    enum Source {

      case File(path: String)
      case JarResource(path: String)
      case EnvVar(nesting: List[String], varName: String)
      case Raw(nesting: List[String], json: Json)

    }
    object Source {

      private final case class WithOptionalNesting(nesting: List[String], value: String)
      private object WithOptionalNesting {

        private val nestedRegex = "^([a-zA-z0-9_]+(?:\\.[a-zA-z0-9_]+)*):(.*)$".r

        given StringDecoder[WithOptionalNesting] =
          StringDecoder.string.map {
            case nestedRegex(nesting, value) => WithOptionalNesting(nesting.split('.').toList, value)
            case value                       => WithOptionalNesting(Nil, value)
          }

      }

      def nestJson(nesting: List[String], json: Json): Json =
        nesting.foldRight(json) { (key, acc) => Json.Obj(key -> acc) }

      def eval(source: Source): IO[ExecuteError.SourceError, Json] = {
        val tmp1: IO[Throwable | ExecuteError.SourceError.Cause, (List[String], String | Json)] = source match {
          case Source.File(path) =>
            for {
              exists <- ZIO.attempt(java.io.File(path).exists())
              _ <- ZIO.fail(ExecuteError.SourceError.Cause.SourceDNE).unless(exists)
              str <- ZIO.readFile(path)
            } yield (Nil, str)

          case Source.JarResource(path)        => JarUtils.findString(path).someOrFail(ExecuteError.SourceError.Cause.SourceDNE).map((Nil, _))
          case Source.EnvVar(nesting, varName) => System.env(varName).someOrFail(ExecuteError.SourceError.Cause.SourceDNE).map((nesting, _))
          case Source.Raw(nesting, json)       => ZIO.succeed((nesting, json))
        }
        val tmp2: IO[ExecuteError.SourceError.Cause, Json] =
          tmp1
            .mapError {
              case cause: ExecuteError.SourceError.Cause => cause
              case throwable: Throwable                  => ExecuteError.SourceError.Cause.Generic(throwable)
            }
            .flatMap {
              case (nesting, string: String) => ZIO.fromEither(string.fromJson[Json]).mapBoth(ExecuteError.SourceError.Cause.InvalidJson(string, _), nestJson(nesting, _))
              case (nesting, json: Json)     => ZIO.succeed(nestJson(nesting, json))
            }

        tmp2.mapError(ExecuteError.SourceError(source, _))
      }

      val parser: Params[Source] =
        Params.firstOf(
          Params
            .value[String](
              "file",
              'f',
              hints = List("Read json config from the specified file path"),
            )
            .map(Source.File(_)),
          Params
            .value[String](
              "jar",
              'j',
              hints = List("Read json config from the specified jar resource path"),
            )
            .map(Source.JarResource(_)),
          Params
            .value[WithOptionalNesting](
              "env",
              'e',
              hints = List(
                "Read json config from the specified environment variable.",
                "Can be prefixed with `--env=json.path.ex:env-var` to nest the value within the env var as such:",
                """    { "json": { "path": { "ex": $ENV_VAR_VALUE$ } } }""",
              ),
            )
            .map(a => Source.EnvVar(a.nesting, a.value)),
          Params
            .value[WithOptionalNesting](
              "raw",
              'r',
              hints = List(
                "Read json config from the specified environment variable.",
                "Can be prefixed with `--raw=json.path.ex:your-json` to nest the json as such:",
                """    { "json": { "path": { "ex": $YOUR_JSON$ } } }""",
              ),
            )
            .map(a => Source.Raw(a.nesting, a.value.toJsonASTDefaultString)),
        )

    }

    val parser: Params[Config] =
      (
        Config.InitialLoggerDefault.parser &&
          Config.Source.parser.repeated
      ).map { Config.apply }

  }

}
