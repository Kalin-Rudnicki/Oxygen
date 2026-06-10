package oxygen.executable.generic

import oxygen.cli.*
import oxygen.executable.CompiledCliApp
import zio.*

private[executable] object DeriveCliAppRuntime {

  def flattenTuple(parsed: Any, count: Int): List[Any] =
    if count <= 1 then List(parsed)
    else
      val tuple = parsed.asInstanceOf[(Any, Any)]
      flattenTuple(tuple._1, count - 1) :+ tuple._2

  def runEffect(effect: Any): URIO[Scope, ExitCode] =
    effect.asInstanceOf[RIO[Any, Unit | ExitCode]].flatMap {
      case ec: ExitCode => ZIO.succeed(ec)
      case ()           => ZIO.succeed(ExitCode.success)
    }.orDie

  def provideEnvAndRun(layer: Any, effect: Any): URIO[Scope, ExitCode] =
    runEffect(effect).provideSomeLayer(layer.asInstanceOf[ZLayer[Any, Nothing, Any]])

  def runParsed(
      parser: ArgsParser[?],
      args: Args,
      runParsed: Any => URIO[Scope, ExitCode],
  ): URIO[Scope, ExitCode] =
    parser.parseArgs(args) match
      case CliParseResult.Fail(_, help) => Console.printLine(help.toString).orDie.as(ExitCode.success)
      case CliParseResult.Success(parsed, remaining) =>
        ArgsParser.toFinal(parser.parseArgs(Args(remaining.positional, remaining.named))) match
          case Left((_, help)) => Console.printLine(help.toString).orDie.as(ExitCode.success)
          case Right(_)        => runParsed(parsed)

  def runParsedN(
      parsers: List[ArgsParser[?]],
      args: Args,
      runParsed: List[Any] => URIO[Scope, ExitCode],
  ): URIO[Scope, ExitCode] = {
    def loop(remainingParsers: List[ArgsParser[?]], parsed: List[Any], remainingArgs: Args): Either[(CliParseError, Help), List[Any]] =
      remainingParsers match
        case Nil => Right(parsed)
        case parser :: rest =>
          parser.parseArgs(remainingArgs) match
            case CliParseResult.Fail(error, help) => Left((error, help))
            case CliParseResult.Success(value, _) => loop(rest, parsed :+ value, remainingArgs)

    loop(parsers, Nil, args) match
      case Left((_, help)) => Console.printLine(help.toString).orDie.as(ExitCode.success)
      case Right(parsed)   => runParsed(parsed)
  }

  def runParsed2(
      envParser: ArgsParser[?],
      cmdParser: ArgsParser[?],
      args: Args,
      runParsed: (Any, Any) => URIO[Scope, ExitCode],
  ): URIO[Scope, ExitCode] =
    envParser.parseArgs(args) match
      case CliParseResult.Fail(_, help) => Console.printLine(help.toString).orDie.as(ExitCode.success)
      case CliParseResult.Success(envParsed, afterEnv) =>
        cmdParser.parseArgs(afterEnv) match
          case CliParseResult.Fail(_, help) => Console.printLine(help.toString).orDie.as(ExitCode.success)
          case CliParseResult.Success(cmdParsed, remaining) =>
            ArgsParser.toFinal(cmdParser.parseArgs(Args(remaining.positional, remaining.named))) match
              case Left((_, help)) => Console.printLine(help.toString).orDie.as(ExitCode.success)
              case Right(_)        => runParsed(envParsed, cmdParsed)

  def subAppStub(name: String): URIO[Scope, ExitCode] =
    Console.printLine(s"Sub-app command '$name' is not yet supported").orDie.as(ExitCode.failure)

  def runWithSubCommands(
      rootParser: ArgsParser[?],
      subCommands: Map[String, CompiledCliApp[Any]],
      args: List[String],
  ): URIO[Scope, ExitCode] =
    Args.parse(args) match
      case Left(message) => Console.printLine(message).orDie.as(ExitCode.failure)
      case Right(parsedArgs) =>
        rootParser.parseArgs(parsedArgs) match
          case CliParseResult.Fail(_, help) => Console.printLine(help.toString).orDie.as(ExitCode.success)
          case CliParseResult.Success(rootParsed, remaining) =>
            remaining.positional.args match
              case PositionalArg(_, name) :: posTail =>
                subCommands.get(name) match
                  case Some(cmd) =>
                    val cmdArgs = Args(PositionalArgs(posTail), remaining.named)
                    cmd.asInstanceOf[CompiledCliApp.Impl[Any]].runWithRoot(rootParsed, cmdArgs)
                  case None =>
                    val known = subCommands.keySet.toList.sorted.mkString(", ")
                    Console.printLine(s"Unknown command '$name'. Known commands: $known").orDie.as(ExitCode.failure)
              case Nil =>
                val known = subCommands.keySet.toList.sorted.mkString(", ")
                Console.printLine(s"Missing command. Known commands: $known").orDie.as(ExitCode.failure)

}