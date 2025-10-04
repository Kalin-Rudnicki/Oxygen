package oxygen.executable

import oxygen.cli.*
import oxygen.executable.error.ExecuteError
import oxygen.json.KeyedMapDecoder
import oxygen.predef.core.*
import oxygen.predef.json.*
import oxygen.predef.zio.*
import oxygen.zio.JarUtils
import oxygen.zio.logging.*
import zio.{ExitCode, ZIOAppArgs, ZIOAppDefault}

trait ExecutableApp extends ZIOAppDefault {

  val executable: Executable

  val additionalLoggerDecoders: ArraySeq[KeyedMapDecoder.Decoder[LogConfig.LoggerElem]] = ArraySeq.empty[KeyedMapDecoder.Decoder[LogConfig.LoggerElem]]
  val additionalLoggerParsers: ArraySeq[Params[Logger]] = ArraySeq.empty[Params[Logger]]

  private def parseAndExecute(args: List[String]): ZIO[Scope, ExecuteError, Unit] =
    for {
      (internalArgs, executableArgs) <- ZIO.succeed(Arg.splitOn_--(args))
      executableConfig <- Parsing.parse(ExecutableApp.Config.parser, internalArgs)
      _ <- LogConfig.usingConfig(LogConfig.oxygenDefault).set.unlessDiscard(executableConfig.keepZioLogger)
      parsedJsons <- ZIO.foreach(executableConfig.sources)(ExecutableApp.Config.Source.eval)
      jsonConfig = parsedJsons.foldLeft[Json](Json.obj())(_ merge _)
      context = ExecutableContext(
        logTargetDecoder = KeyedMapDecoder(LogConfig.elemDecoders.default ++ additionalLoggerDecoders),
        additionalLoggerParsers = additionalLoggerParsers,
        executableConfig = executableConfig,
      )
      _ <- executable(jsonConfig, executableArgs, context)
    } yield ()

  private def parseExecuteAndRecover(args: List[String]): URIO[Scope, ExitCode] =
    parseAndExecute(args)
      .as(ExitCode.success)
      .catchSome { case ExecuteError.Parsing.Help(help, _) => Console.printLine(help.toString).orDie.as(ExitCode.success) }
      .catchSome { case e: ExecuteError.Parsing.FailedToParse => Console.printLine(e.getMessage).orDie.as(ExitCode.failure) }
      .catchAllCause { ZIO.logFatalCause(_).as(ExitCode.failure) }

  override final def run: URIO[ZIOAppArgs, Unit] =
    ZIO
      .scoped { ZIOAppArgs.getArgs.flatMap { args => parseExecuteAndRecover(args.toList) } }
      .flatMap(exit)

}
object ExecutableApp {

  final case class Config(
      sources: List[Config.Source],
      keepZioLogger: Boolean,
  )
  object Config {

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
        nesting.foldRight(json) { (key, acc) => Json.obj(key -> acc) }

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
              case (nesting, string: String) => ZIO.fromEither(Json.parse(string)).mapBoth(e => ExecuteError.SourceError.Cause.InvalidJson(string, e.toString), nestJson(nesting, _))
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
            .map(a => Source.Raw(a.nesting, Json.parseOrJsonString(a.value))),
        )

    }

    val parser: Params[Config] =
      (
        Config.Source.parser.repeated &&
          Params.flag("keep-zio-logger", shortName = 'Z', hints = List("Specifying this flag stops oxygen from setting the default logger"))
      ).map { Config.apply }

  }

}
