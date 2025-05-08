package oxygen.executable

import oxygen.cli.*
import oxygen.executable.error.ExecuteError
import oxygen.predef.color.*
import oxygen.predef.core.*
import oxygen.predef.json.*
import oxygen.predef.zio.*
import oxygen.zio.logging.*
import zio.LogSpan

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

    protected val jsonDecoder: JsonDecoder[JsonConfig]
    protected val envTag: EnvironmentTag[Env]
    protected val cliParser: ExecutableContext => Parser[CLIConfig]

    protected def logger(jsonConfig: JsonConfig, cliConfig: CLIConfig, context: ExecutableContext): ZIO[Scope, ExecuteError, LogConfig]

    protected def env(jsonConfig: JsonConfig, cliConfig: CLIConfig): Layer[Any, Env]

    protected def execute(jsonConfig: JsonConfig, cliConfig: CLIConfig): ZIO[Scope & Env, Any, Unit]

    override private[executable] def apply(
        jsonConfig: Json,
        args: List[String],
        context: ExecutableContext,
    ): ZIO[Scope, ExecuteError, Unit] = {
      given EnvironmentTag[Env] = envTag
      for {
        decodedConfig <- ZIO.fromEither(jsonDecoder.decodeJsonAST(jsonConfig)).mapError(e => ExecuteError.InvalidConfig(e.toString))
        parsedCLI <- Parsing.parse(cliParser(context), args)
        _ <- logger(decodedConfig, parsedCLI, context).flatMap(LogConfig.usingConfig(_).set)
        _ <- execute(decodedConfig, parsedCLI).provideSomeLayer[Scope](env(decodedConfig, parsedCLI)).mapError(ExecuteError.ProgramError(_))
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

  private val unitDecoder: JsonDecoder[Unit] =
    new JsonDecoder[Unit] {
      override def decodeJsonAST(ast: Json): Either[JsonError, Unit] = ().asRight
    }

  class Builder0 extends Builder1[Unit](unitDecoder) {

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
  ) extends Builder3[_JsonConfig, (LogConfig, _CLIConfig), _CLIConfig](
        _jsonDecoder,
        ctx => LoggerCLIConfig.logConfigParser(ctx) ^>> _cliParser(ctx),
        _._2,
        (_, cliConfig, _) => ZIO.succeed(cliConfig._1),
      ) {

    final def withLoggerFromJson(toLoggerConfig: _JsonConfig => LogConfig.Repr): Builder3[_JsonConfig, _CLIConfig, _CLIConfig] =
      Builder3(
        _jsonDecoder,
        _cliParser,
        identity,
        (jsonCfg, _, ctx) => ZIO.fromEither(toLoggerConfig(jsonCfg).decode(ctx.logTargetDecoder)).mapError { e => ExecuteError.InvalidConfig(s"Unable to decode loggers - $e") },
      )

    final def withLoggerFromCLI(using zip: Zip[LogConfig, _CLIConfig]): Builder3[_JsonConfig, zip.Out, _CLIConfig] =
      Builder3(
        _jsonDecoder,
        ctx => LoggerCLIConfig.logConfigParser(ctx) ^>> _cliParser(ctx),
        zip.unzip(_)._2,
        (_, cliConfig, _) => ZIO.succeed(zip.unzip(cliConfig)._1),
      )

  }

  class Builder3[_JsonConfig, _FullCLIConfig, _CLIConfig](
      _jsonDecoder: JsonDecoder[_JsonConfig],
      _cliParser: ExecutableContext => Parser[_FullCLIConfig],
      _mapCLIConfig: _FullCLIConfig => _CLIConfig,
      _logger: (_JsonConfig, _FullCLIConfig, ExecutableContext) => ZIO[Scope, ExecuteError, LogConfig],
  ) extends Builder4[_JsonConfig, _FullCLIConfig, _CLIConfig, Throwable](
        _jsonDecoder,
        _cliParser,
        _mapCLIConfig,
        _logger,
      ) {

    final def limitError[_Error]: Builder4[_JsonConfig, _FullCLIConfig, _CLIConfig, _Error] =
      Builder4(
        _jsonDecoder,
        _cliParser,
        _mapCLIConfig,
        _logger,
      )

  }

  class Builder4[_JsonConfig, _FullCLIConfig, _CLIConfig, _Error](
      _jsonDecoder: JsonDecoder[_JsonConfig],
      _cliParser: ExecutableContext => Parser[_FullCLIConfig],
      _mapCLIConfig: _FullCLIConfig => _CLIConfig,
      _logger: (_JsonConfig, _FullCLIConfig, ExecutableContext) => ZIO[Scope, ExecuteError, LogConfig],
  ) extends Builder5[_JsonConfig, _FullCLIConfig, _CLIConfig, _Error, Any](
        _jsonDecoder,
        _cliParser,
        _mapCLIConfig,
        _logger,
        EnvironmentTag[Any],
        (_, _) => ZLayer.empty,
      ) {

    final def withEnv[_Env](_env: (_JsonConfig, _CLIConfig) => Layer[_Error, _Env])(using _envTag: EnvironmentTag[_Env]): Builder5[_JsonConfig, _FullCLIConfig, _CLIConfig, _Error, _Env] =
      Builder5(
        _jsonDecoder,
        _cliParser,
        _mapCLIConfig,
        _logger,
        _envTag,
        _env,
      )

    final def withEnv[_Env](_env: Layer[_Error, _Env])(using _envTag: EnvironmentTag[_Env]): Builder5[_JsonConfig, _FullCLIConfig, _CLIConfig, _Error, _Env] =
      withEnv((_, _) => _env)

  }

  class Builder5[_JsonConfig, _FullCLIConfig, _CLIConfig, _Error, _Env](
      _jsonDecoder: JsonDecoder[_JsonConfig],
      _cliParser: ExecutableContext => Parser[_FullCLIConfig],
      _mapCLIConfig: _FullCLIConfig => _CLIConfig,
      _logger: (_JsonConfig, _FullCLIConfig, ExecutableContext) => ZIO[Scope, ExecuteError, LogConfig],
      _envTag: EnvironmentTag[_Env],
      _env: (_JsonConfig, _CLIConfig) => Layer[_Error, _Env],
  ) {

    final def withExecute(_execute: (_JsonConfig, _CLIConfig) => ZIO[Scope & _Env, _Error, Unit]): Executable =
      new Executable.Single {

        override protected type JsonConfig = _JsonConfig
        override protected type CLIConfig = _FullCLIConfig
        override protected type Env = _Env

        override protected val jsonDecoder: JsonDecoder[JsonConfig] = _jsonDecoder
        override protected val envTag: EnvironmentTag[Env] = _envTag
        override protected val cliParser: ExecutableContext => Parser[CLIConfig] = _cliParser

        override protected def logger(jsonConfig: JsonConfig, cliConfig: CLIConfig, context: ExecutableContext): ZIO[Scope, ExecuteError, LogConfig] =
          _logger(jsonConfig, cliConfig, context)

        override protected def env(jsonConfig: JsonConfig, cliConfig: CLIConfig): Layer[Any, Env] = _env(jsonConfig, _mapCLIConfig(cliConfig))

        override protected def execute(jsonConfig: JsonConfig, cliConfig: CLIConfig): ZIO[Scope & Env, Any, Unit] = _execute(jsonConfig, _mapCLIConfig(cliConfig))

      }

    final def withExecute(_execute: _CLIConfig => ZIO[Scope & _Env, _Error, Unit]): Executable =
      withExecute((_, cli) => _execute(cli))

    final def withExecute(_execute: => ZIO[Scope & _Env, _Error, Unit]): Executable =
      withExecute((_, _) => _execute)

  }

  // =====|  |=====

  private object LoggerCLIConfig {

    private val logContextParser: Params[Map[String, String]] =
      Params
        .valueWith("log-context", 'l', hints = List("log context key-value pairs"))(
          Values.value[String]("key", hints = List("context key")) ^>>
            Values.value[String]("value", hints = List("context value")),
        )
        .repeated
        .map(_.toMap)

    private val logSpansParser: Params[List[LogSpan]] =
      Params
        .valueWith[LogSpan]("span", hints = List("adds a given log span"))(
          (
            Values.value[String]("label", hints = List("label of the log span")) ^>>
              Values.value[Long]("start-time", hints = List("start time of the log span"))
          ).map(LogSpan(_, _)),
        )
        .repeated

    private val logLevelParser: Params[LogLevel] =
      Params.`enum`[RichLogLevel]("log-level", hints = List("default log level")).withDefault(RichLogLevel.Info).map(_.level)

    private val oxygenLoggerParser: Params[Logger] =
      Params.valueWith(
        "oxygen-logger",
        'O',
        hints = List("Uses the default oxygen logger,", " which is more focused on human readability in the console"),
      ) {
        (
          Params.`enum`[ColorMode]("color-mode").withDefault(ColorMode.Extended) &&
            Params.toggle.prefixFalse("no", "log-trace", hints = List("Whether to log trace location")).withDefault(true) &&
            Params.toggle.prefixFalse("no", "log-fiber", hints = List("Whether to log trace location")).withDefault(true) &&
            Params.toggle.prefixFalse("no", "log-annotations", hints = List("Whether to log trace location")).withDefault(true) &&
            Params.toggle.prefixFalse("no", "log-spans", hints = List("Whether to log trace location")).withDefault(true) &&
            Params.toggle.prefixFalse("no", "log-timestamp", hints = List("Whether to log trace location")).withDefault(true)
        ).bracketed("oxygen-params")
          .map { Logger.oxygen(_, _, _, _, _, _) }
          .withDefault { Logger.oxygenDefault }

      }

    private val jsonLoggerParser: Params[Logger] =
      Params.ifPresent(
        "json-logger",
        Logger.zioDefault,
        'J',
        hints = List("Logs in a json format"),
      )

    private val zioLoggerParser: Params[Logger] =
      Params.ifPresent(
        "zio-logger",
        Logger.zioDefault,
        'Z',
        hints = List("Uses the default zio logger"),
      )

    private def loggerParser(otherParsers: Contiguous[Params[Logger]])(default: Logger): Params[Logger] = {
      val base: NonEmptyList[Params[Logger]] = NonEmptyList.of(oxygenLoggerParser, jsonLoggerParser, zioLoggerParser)
      val withExtras: NonEmptyList[Params[Logger]] = base :++ otherParsers

      Params
        .FirstOfByArgIndex(withExtras)
        .withDefault(default)
    }

    def logConfigParser(config: ExecutableContext): Params[LogConfig] =
      (
        logContextParser &&
          logSpansParser &&
          logLevelParser &&
          loggerParser(config.additionalLoggerParsers) {
            if (config.executableConfig.keepZioLogger) Logger.zioDefault
            else Logger.oxygenDefault
          }
      ).map { case (annotations, spans, logLevel, logger) => LogConfig(annotations, spans, Contiguous.single(LogConfig.LoggerElem(logger, logLevel))) }

  }

}
