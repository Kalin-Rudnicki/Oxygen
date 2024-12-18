package oxygen.executable

import oxygen.cli.*
import oxygen.cli.given
import oxygen.core.ColorMode
import oxygen.executable.error.ExecuteError
import oxygen.json.KeyedMapDecoder
import oxygen.predef.core.*
import oxygen.predef.zio.*
import oxygen.zio.logger.LogTarget
import oxygen.zio.telemetry.TelemetryTarget
import oxygen.zio.typeclass.ErrorLogger
import zio.json.*
import zio.json.ast.Json

sealed trait Executable {

  private[executable] def apply(
      jsonConfig: Json,
      args: List[String],
      context: ExecutableContext,
  ): ZIO[Scope, ExecuteError, Unit]

}
object Executable extends SingleBuilders.Builder0 {

  trait Single extends Executable {

    protected type JsonConfig
    protected type CLIConfig

    protected type Env
    protected type Error

    protected val jsonDecoder: JsonDecoder[JsonConfig]
    protected val errorLogger: ErrorLogger[Error]
    protected val envTag: EnvironmentTag[Env]
    protected val cliParser: ExecutableContext => Parser[CLIConfig]

    protected def logger(jsonConfig: JsonConfig, cliConfig: CLIConfig, context: ExecutableContext): ZIO[Scope, ExecuteError, OxygenEnv.LoggerEnv]
    protected def telemetry(jsonConfig: JsonConfig, cliConfig: CLIConfig, context: ExecutableContext): ZIO[Scope, ExecuteError, OxygenEnv.TelemetryEnv]

    protected def env(jsonConfig: JsonConfig, cliConfig: CLIConfig): Layer[Error, Env]

    protected def execute(jsonConfig: JsonConfig, cliConfig: CLIConfig): ZIO[Scope & Env, Error, Unit]

    override private[executable] def apply(
        jsonConfig: Json,
        args: List[String],
        context: ExecutableContext,
    ): ZIO[Scope, ExecuteError, Unit] = {
      given EnvironmentTag[Env] = envTag
      for {
        decodedConfig <- ZIO.fromEither(jsonDecoder.decodeJson(jsonConfig.toString)).mapError(ExecuteError.InvalidConfig(_))
        parsedCLI <- Parsing.parse(cliParser(context), args)
        _ <- logger(decodedConfig, parsedCLI, context).flatMap(Logger.env(_).set)
        _ <- telemetry(decodedConfig, parsedCLI, context).flatMap(Telemetry.env(_).set)
        _ <- execute(decodedConfig, parsedCLI).provideSomeLayer[Scope](env(decodedConfig, parsedCLI)).mapError(e => ExecuteError.ProgramError(errorLogger.show(e), errorLogger.logLevel(e)))
      } yield ()
    }

  }

  final case class Many(options: NonEmptyList[(String, Executable)]) extends Executable {

    private val optionMap: Map[String, Executable] = options.toMap

    override private[executable] def apply(
        jsonConfig: Json,
        args: List[String],
        context: ExecutableContext,
    ): ZIO[Scope, ExecuteError, Unit] = args match
      case optionMap(child) :: tail => child(jsonConfig, tail, context)
      case command :: _             => ZIO.fail(ExecuteError.SubCommandError.InvalidSubCommand(command, options))
      case Nil                      => ZIO.fail(ExecuteError.SubCommandError.MissingSubCommand(options))

  }

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Single Builders
//////////////////////////////////////////////////////////////////////////////////////////////////////

object SingleBuilders {

  class Builder0 extends Builder1[Unit]((_, _) => ()) {

    final def withJsonConfig[_JsonConfig](using jsonDecoder: JsonDecoder[_JsonConfig]): Builder1[_JsonConfig] =
      Builder1(
        jsonDecoder,
      )

  }

  class Builder1[_JsonConfig](
      _jsonDecoder: JsonDecoder[_JsonConfig],
  ) extends Builder2[_JsonConfig, Unit](
        _jsonDecoder,
        _ => Parser.Empty,
      ) {

    final def withCLIParser[_CLIConfig](parser: Parser[_CLIConfig]): Builder2[_JsonConfig, _CLIConfig] =
      Builder2(
        _jsonDecoder,
        _ => parser,
      )

  }

  class Builder2[_JsonConfig, _CLIConfig](
      _jsonDecoder: JsonDecoder[_JsonConfig],
      _cliParser: ExecutableContext => Parser[_CLIConfig],
  ) extends Builder3[_JsonConfig, _CLIConfig, _CLIConfig](
        _jsonDecoder,
        _cliParser,
        identity,
        // TODO (KR) : what should the default be?
        (_, _, _) =>
          ZIO.succeed(
            OxygenEnv.LoggerEnv(
              targets = Chunk.single(LogTarget.StdOut.defaultWithAdditionalContext),
              context = Map.empty,
              spans = Nil,
              level = LogLevel.Info,
              logToZio = false,
            ),
          ),
      ) {

    final def withLoggerFromJson(toLoggerConfig: _JsonConfig => Logger.Config): Builder3[_JsonConfig, _CLIConfig, _CLIConfig] =
      Builder3(
        _jsonDecoder,
        _cliParser,
        identity,
        (jsonConfig, _, ctx) => makeLoggerEnv(toLoggerConfig(jsonConfig), ctx.logTargetDecoder),
      )

    final def withLoggerFromCLI(using zip: Zip[OxygenEnv.LoggerEnv, _CLIConfig]): Builder3[_JsonConfig, zip.Out, _CLIConfig] =
      Builder3(
        _jsonDecoder,
        ctx =>
          ctx.cfg.initialLoggerDefault match {
            case ExecutableApp.Config.InitialLoggerDefault.Oxygen => LoggerCLIConfig.defaultOxygenLogParser ^>> _cliParser(ctx)
            case ExecutableApp.Config.InitialLoggerDefault.ZIO    => LoggerCLIConfig.defaultZioLogParser ^>> _cliParser(ctx)
          },
        zip.unzip(_)._2,
        (_, cliConfig, _) => ZIO.succeed(zip.unzip(cliConfig)._1),
      )

  }

  class Builder3[_JsonConfig, _FullCLIConfig, _CLIConfig](
      _jsonDecoder: JsonDecoder[_JsonConfig],
      _cliParser: ExecutableContext => Parser[_FullCLIConfig],
      _mapCLIConfig: _FullCLIConfig => _CLIConfig,
      _logger: (_JsonConfig, _FullCLIConfig, ExecutableContext) => ZIO[Scope, ExecuteError, OxygenEnv.LoggerEnv],
  ) extends Builder4[_JsonConfig, _FullCLIConfig, _CLIConfig](
        _jsonDecoder,
        _cliParser,
        _mapCLIConfig,
        _logger,
        (_, _, _) => ZIO.succeed(OxygenEnv.TelemetryEnv(targets = Chunk.empty)),
      ) {

    final def withTelemetryFromJson(toTelemetryConfig: _JsonConfig => Telemetry.Config): Builder4[_JsonConfig, _FullCLIConfig, _CLIConfig] =
      Builder4(
        _jsonDecoder,
        _cliParser,
        _mapCLIConfig,
        _logger,
        (jsonConfig, _, ctx) => makeTelemetryEnv(toTelemetryConfig(jsonConfig), ctx.telemetryTargetDecoder),
      )

  }

  class Builder4[_JsonConfig, _FullCLIConfig, _CLIConfig](
      _jsonDecoder: JsonDecoder[_JsonConfig],
      _cliParser: ExecutableContext => Parser[_FullCLIConfig],
      _mapCLIConfig: _FullCLIConfig => _CLIConfig,
      _logger: (_JsonConfig, _FullCLIConfig, ExecutableContext) => ZIO[Scope, ExecuteError, OxygenEnv.LoggerEnv],
      _telemetry: (_JsonConfig, _FullCLIConfig, ExecutableContext) => ZIO[Scope, ExecuteError, OxygenEnv.TelemetryEnv],
  ) extends Builder5[_JsonConfig, _FullCLIConfig, _CLIConfig, Throwable](
        _jsonDecoder,
        _cliParser,
        _mapCLIConfig,
        _logger,
        _telemetry,
        ErrorLogger.throwableGetMessage.atLevel.fatal,
      ) {

    final def withError[_Error](using errorLogger: ErrorLogger[_Error]): Builder5[_JsonConfig, _FullCLIConfig, _CLIConfig, _Error] =
      Builder5(
        _jsonDecoder,
        _cliParser,
        _mapCLIConfig,
        _logger,
        _telemetry,
        errorLogger,
      )

  }

  class Builder5[_JsonConfig, _FullCLIConfig, _CLIConfig, _Error](
      _jsonDecoder: JsonDecoder[_JsonConfig],
      _cliParser: ExecutableContext => Parser[_FullCLIConfig],
      _mapCLIConfig: _FullCLIConfig => _CLIConfig,
      _logger: (_JsonConfig, _FullCLIConfig, ExecutableContext) => ZIO[Scope, ExecuteError, OxygenEnv.LoggerEnv],
      _telemetry: (_JsonConfig, _FullCLIConfig, ExecutableContext) => ZIO[Scope, ExecuteError, OxygenEnv.TelemetryEnv],
      _errorLogger: ErrorLogger[_Error],
  ) extends Builder6[_JsonConfig, _FullCLIConfig, _CLIConfig, _Error, Any](
        _jsonDecoder,
        _cliParser,
        _mapCLIConfig,
        _logger,
        _telemetry,
        _errorLogger,
        EnvironmentTag[Any],
        (_, _) => ZLayer.empty,
      ) {

    final def withEnv[_Env](_env: (_JsonConfig, _CLIConfig) => Layer[_Error, _Env])(using _envTag: EnvironmentTag[_Env]): Builder6[_JsonConfig, _FullCLIConfig, _CLIConfig, _Error, _Env] =
      Builder6(
        _jsonDecoder,
        _cliParser,
        _mapCLIConfig,
        _logger,
        _telemetry,
        _errorLogger,
        _envTag,
        _env,
      )

    final def withEnv[_Env](_env: Layer[_Error, _Env])(using _envTag: EnvironmentTag[_Env]): Builder6[_JsonConfig, _FullCLIConfig, _CLIConfig, _Error, _Env] =
      withEnv((_, _) => _env)

  }

  class Builder6[_JsonConfig, _FullCLIConfig, _CLIConfig, _Error, _Env](
      _jsonDecoder: JsonDecoder[_JsonConfig],
      _cliParser: ExecutableContext => Parser[_FullCLIConfig],
      _mapCLIConfig: _FullCLIConfig => _CLIConfig,
      _logger: (_JsonConfig, _FullCLIConfig, ExecutableContext) => ZIO[Scope, ExecuteError, OxygenEnv.LoggerEnv],
      _telemetry: (_JsonConfig, _FullCLIConfig, ExecutableContext) => ZIO[Scope, ExecuteError, OxygenEnv.TelemetryEnv],
      _errorLogger: ErrorLogger[_Error],
      _envTag: EnvironmentTag[_Env],
      _env: (_JsonConfig, _CLIConfig) => Layer[_Error, _Env],
  ) {

    final def withExecute(_execute: (_JsonConfig, _CLIConfig) => ZIO[Scope & _Env, _Error, Unit]): Executable =
      new Executable.Single {

        override protected type JsonConfig = _JsonConfig
        override protected type CLIConfig = _FullCLIConfig
        override protected type Env = _Env
        override protected type Error = _Error

        override protected val jsonDecoder: JsonDecoder[JsonConfig] = _jsonDecoder
        override protected val errorLogger: ErrorLogger[Error] = _errorLogger
        override protected val envTag: EnvironmentTag[Env] = _envTag
        override protected val cliParser: ExecutableContext => Parser[CLIConfig] = _cliParser

        override protected def logger(jsonConfig: JsonConfig, cliConfig: CLIConfig, context: ExecutableContext): ZIO[Scope, ExecuteError, OxygenEnv.LoggerEnv] =
          _logger(jsonConfig, cliConfig, context)
        override protected def telemetry(
            jsonConfig: JsonConfig,
            cliConfig: CLIConfig,
            context: ExecutableContext,
        ): ZIO[Scope, ExecuteError, OxygenEnv.TelemetryEnv] =
          _telemetry(jsonConfig, cliConfig, context)

        override protected def env(jsonConfig: JsonConfig, cliConfig: CLIConfig): Layer[Error, Env] = _env(jsonConfig, _mapCLIConfig(cliConfig))

        override protected def execute(jsonConfig: JsonConfig, cliConfig: CLIConfig): ZIO[Scope & Env, Error, Unit] = _execute(jsonConfig, _mapCLIConfig(cliConfig))

      }

    final def withExecute(_execute: _CLIConfig => ZIO[Scope & _Env, _Error, Unit]): Executable =
      withExecute((_, cli) => _execute(cli))

    final def withExecute(_execute: => ZIO[Scope & _Env, _Error, Unit]): Executable =
      withExecute((_, _) => _execute)

  }

  // =====|  |=====

  private def makeLoggerEnv(config: Logger.Config, decoder: KeyedMapDecoder[LogTarget.ConfigBuilder]): ZIO[Scope, ExecuteError, OxygenEnv.LoggerEnv] =
    for {
      targets <- config.decodeTargets(decoder).mapError {
        case string: String       => ExecuteError.InvalidConfig(string)
        case throwable: Throwable => ExecuteError.Generic("building log targets", throwable)
      }
    } yield OxygenEnv.LoggerEnv(
      targets = targets,
      context = config.context.getOrElse(Map.empty),
      spans = config.spans.getOrElse(Nil),
      level = config.level,
      logToZio = config.logToZio.getOrElse(false),
    )

  private def makeTelemetryEnv(config: Telemetry.Config, decoder: KeyedMapDecoder[TelemetryTarget.ConfigBuilder]): ZIO[Scope, ExecuteError, OxygenEnv.TelemetryEnv] =
    for {
      targets <- config.decodeTargets(decoder).mapError {
        case string: String       => ExecuteError.InvalidConfig(string)
        case throwable: Throwable => ExecuteError.Generic("building telemetry targets", throwable)
      }
    } yield OxygenEnv.TelemetryEnv(
      targets = targets,
    )

  // =====|  |=====

  private object LoggerCLIConfig {

    private val stdOutParser: Params[LogTarget.StdOut] =
      (
        Params.`enum`[LogLevel]("min-log-level", 'l').optional &&
          Params.`enum`[ColorMode]("color-mode").withDefault(ColorMode.Extended) &&
          Params.toggle.prefixFalse("no", "log-timestamp").withDefault(true) &&
          Params.toggle.prefixFalse("no", "log-trace").withDefault(true) &&
          Params.toggle.prefixFalse("no", "log-stack").withDefault(true) &&
          Params.toggle.prefixFalse("no", "log-fiber-id").withDefault(true)
      ).map { LogTarget.StdOut.apply }

    private val stdOutJsonParser: Params[LogTarget.StdOutJson] =
      (
        Params.`enum`[LogLevel]("min-log-level", 'l').optional
      ).map { LogTarget.StdOutJson.apply }

    private val logTargetParser: Params[LogTarget] =
      Params.firstOf(
        Params.valueWith("std-out", Defaultable.None, hints = List("logs to std-out in a pretty format"))(
          stdOutParser.bracketed("std-out").withDefault(LogTarget.StdOut.defaultWithAdditionalContext),
        ),
        Params.valueWith("std-out-json", Defaultable.None, hints = List("lots to std-out in json format"))(
          stdOutJsonParser.bracketed("std-out-json").withDefault(LogTarget.StdOutJson(None)),
        ),
      )

    private val logContextParser: Params[Map[String, String]] =
      Params
        .valueWith("log-context", 'l', hints = List("log context key-value pairs"))(
          Values.value[String]("key", hints = List("context key")) ^>>
            Values.value[String]("value", hints = List("context value")),
        )
        .repeated
        .map(_.toMap)

    private val logSpanParser: Params[Logger.Span] =
      Params.valueWith[Logger.Span]("span", hints = List("adds a given log span"))(
        (
          Values.value[String]("label", hints = List("label of the log span")) ^>>
            Values.value[Long]("start-time", hints = List("start time of the log span")).optional
        ).map(Logger.Span(_, _)),
      )

    val defaultOxygenLogParser: Params[OxygenEnv.LoggerEnv] =
      (
        logTargetParser.repeatedNel.withDefault(NonEmptyList.one(LogTarget.StdOut.defaultWithAdditionalContext)).map(ts => Chunk.fromIterable(ts.toList)) &&
          logContextParser &&
          logSpanParser.repeated &&
          Params.`enum`[LogLevel]("log-level", hints = List("default log level")).withDefault(LogLevel.Info) &&
          Params.toggle.prefixFalse("no", "log-to-zio", hints = List("log to standard zio logger")).withDefault(false)
      ).map { OxygenEnv.LoggerEnv.apply }

    val defaultZioLogParser: Params[OxygenEnv.LoggerEnv] =
      (
        logTargetParser.repeated.map(Chunk.fromIterable) &&
          logContextParser &&
          logSpanParser.repeated &&
          Params.`enum`[LogLevel]("log-level", hints = List("default log level")).withDefault(LogLevel.Info) &&
          Params.toggle.prefixFalse("no", "log-to-zio", hints = List("log to standard zio logger")).withDefault(true)
      ).map { OxygenEnv.LoggerEnv.apply }

  }

}
