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

  def printHelp(help: Help): URIO[Scope, ExitCode] =
    Console.printLine(help.toString).orDie.as(ExitCode.success)

  def printComposedHelp(
      helpParser: ArgsParser[?],
      helpType: HelpType,
      subCommands: Map[String, CompiledCliApp[Any]] = Map.empty,
      title: Option[String] = None,
  ): URIO[Scope, ExitCode] =
    printHelp(CliHelp.compose(helpParser, helpType, subCommands, title))

  def handleHelpOr(
      args: Args,
      helpParser: ArgsParser[?],
      subCommands: Map[String, CompiledCliApp[Any]],
      title: Option[String],
      run: Args => URIO[Scope, ExitCode],
  ): URIO[Scope, ExitCode] =
    CliHelp.peelArgs(args) match
      case (stripped, Some(helpType)) =>
        stripped.positional.args match
          case PositionalArg(_, name) :: _ if subCommands.nonEmpty =>
            subCommands.get(name) match
              case Some(cmd) => printComposedHelp(cmd.helpParser, helpType, title = Some(s"Command: $name"))
              case None      => printComposedHelp(helpParser, helpType, subCommands, title)
          case _ => printComposedHelp(helpParser, helpType, subCommands, title)
      case (stripped, None) => run(stripped)

  def runParsed(
      parser: ArgsParser[?],
      args: Args,
      runParsed: Any => URIO[Scope, ExitCode],
  ): URIO[Scope, ExitCode] =
    parser.parseArgs(args) match
      case CliParseResult.Fail(_, help) => printHelp(help)
      case CliParseResult.Success(parsed, remaining) =>
        ArgsParser.toFinal(parser.parseArgs(Args(remaining.positional, remaining.named))) match
          case Left((_, help)) => printHelp(help)
          case Right(_)        => runParsed(parsed)

  def runParsedN(
      parsers: List[ArgsParser[?]],
      args: Args,
      validateRemaining: Boolean,
      runParsed: (List[Any], Args) => URIO[Scope, ExitCode],
  ): URIO[Scope, ExitCode] = {
    def loop(remainingParsers: List[ArgsParser[?]], parsed: List[Any], remainingArgs: Args): Either[(CliParseError, Help), (List[Any], Args)] =
      remainingParsers match
        case Nil => Right((parsed, remainingArgs))
        case parser :: rest =>
          parser.parseArgs(remainingArgs) match
            case CliParseResult.Fail(error, help)         => Left((error, help))
            case CliParseResult.Success(value, remaining) => loop(rest, parsed :+ value, remaining)

    loop(parsers, Nil, args) match
      case Left((_, help)) => printHelp(help)
      case Right((parsed, remaining)) =>
        if validateRemaining then
          ArgsParser.toFinal(CliParseResult.Success((), remaining)) match
            case Left((_, help)) => printHelp(help)
            case Right(_)        => runParsed(parsed, remaining)
        else runParsed(parsed, remaining)
  }

  def runParsed2(
      envParser: ArgsParser[?],
      cmdParser: ArgsParser[?],
      args: Args,
      runParsed: (Any, Any) => URIO[Scope, ExitCode],
  ): URIO[Scope, ExitCode] =
    envParser.parseArgs(args) match
      case CliParseResult.Fail(_, help) => printHelp(help)
      case CliParseResult.Success(envParsed, afterEnv) =>
        cmdParser.parseArgs(afterEnv) match
          case CliParseResult.Fail(_, help) => printHelp(help)
          case CliParseResult.Success(cmdParsed, remaining) =>
            ArgsParser.toFinal(cmdParser.parseArgs(Args(remaining.positional, remaining.named))) match
              case Left((_, help)) => printHelp(help)
              case Right(_)        => runParsed(envParsed, cmdParsed)

  def provideEnvLayer(layer: Any, run: URIO[Scope, ExitCode]): URIO[Scope, ExitCode] =
    run.provideSomeLayer(layer.asInstanceOf[ZLayer[Any, Nothing, Any]])

  private def stripHelpFlags(rawArgs: List[String]): List[String] =
    rawArgs.filterNot(arg => arg == "--help" || arg == "-h" || arg == "--help-extra" || arg == "-H")

  private def findSubCommandRaw(rawArgs: List[String], subCommands: Map[String, CompiledCliApp[Any]]): Option[(String, Int)] =
    rawArgs.zipWithIndex.collectFirst { case (tok, idx) if subCommands.contains(tok) => (tok, idx) }

  private def parseArgsOrDie(rawArgs: List[String]): URIO[Scope, Args] =
    ZIO.fromEither(Args.parse(rawArgs).left.map(new RuntimeException(_))).orDie

  private def runWithSubCommandsSplit(
      rootParser: ArgsParser[?],
      subCommands: Map[String, CompiledCliApp[Any]],
      rawArgs: List[String],
  ): URIO[Scope, ExitCode] = {
    val raw: List[String] = stripHelpFlags(rawArgs)
    findSubCommandRaw(raw, subCommands) match
      case None =>
        val known = subCommands.keySet.toList.sorted.mkString(", ")
        Console.printLine(s"Missing command. Known commands: $known").orDie.as(ExitCode.failure)
      case Some((cmdName, cmdIdx)) =>
        val beforeRaw: List[String] = raw.take(cmdIdx)
        val afterRaw: List[String] = raw.drop(cmdIdx + 1)
        if beforeRaw.nonEmpty then
          Console.printLine("Parent arguments must appear after the command name").orDie.as(ExitCode.failure)
        else
          for {
            after <- parseArgsOrDie(afterRaw)
            exitCode <- {
              val rootArgs: Args = Args(after.positional, after.named)
              rootParser.parseArgs(rootArgs) match
                case CliParseResult.Fail(_, help) => printHelp(help)
                case CliParseResult.Success(rootParsed, rootRemaining) =>
                  val cmdArgs: Args = Args(rootRemaining.positional, rootRemaining.named)
                  subCommands.get(cmdName) match
                    case Some(cmd) =>
                      handleHelpOr(cmdArgs, cmd.helpParser, Map.empty, title = Some(s"Command: $cmdName"), run = cmdStripped =>
                        cmd.asInstanceOf[CompiledCliApp.Impl[Any]].runWithRoot(rootParsed, cmdStripped),
                      )
                    case None =>
                      val known = subCommands.keySet.toList.sorted.mkString(", ")
                      Console.printLine(s"Unknown command '$cmdName'. Known commands: $known").orDie.as(ExitCode.failure)
            }
          } yield exitCode
  }

  private def runWithSubCommandsLegacy(
      rootParser: ArgsParser[?],
      subCommands: Map[String, CompiledCliApp[Any]],
      stripped: Args,
  ): URIO[Scope, ExitCode] =
    rootParser.parseArgs(stripped) match
      case CliParseResult.Fail(_, help) => printHelp(help)
      case CliParseResult.Success(rootParsed, remaining) =>
        remaining.positional.args match
          case PositionalArg(_, name) :: posTail =>
            subCommands.get(name) match
              case Some(cmd) =>
                val cmdArgs = Args(PositionalArgs(posTail), remaining.named)
                handleHelpOr(cmdArgs, cmd.helpParser, Map.empty, title = Some(s"Command: $name"), run = cmdStripped =>
                  cmd.asInstanceOf[CompiledCliApp.Impl[Any]].runWithRoot(rootParsed, cmdStripped),
                )
              case None =>
                val known = subCommands.keySet.toList.sorted.mkString(", ")
                Console.printLine(s"Unknown command '$name'. Known commands: $known").orDie.as(ExitCode.failure)
          case Nil =>
            val known = subCommands.keySet.toList.sorted.mkString(", ")
            Console.printLine(s"Missing command. Known commands: $known").orDie.as(ExitCode.failure)

  def runWithSubCommandsFromArgs(
      rootParser: ArgsParser[?],
      helpParser: ArgsParser[?],
      subCommands: Map[String, CompiledCliApp[Any]],
      parsedArgs: Args,
      rawArgs: Option[List[String]] = None,
  ): URIO[Scope, ExitCode] =
    handleHelpOr(parsedArgs, helpParser, subCommands, title = Some("Usage"), run = stripped =>
      rawArgs match
        case Some(raw) => runWithSubCommandsSplit(rootParser, subCommands, raw)
        case None      => runWithSubCommandsLegacy(rootParser, subCommands, stripped),
    )

  def runWithSubCommands(
      rootParser: ArgsParser[?],
      helpParser: ArgsParser[?],
      subCommands: Map[String, CompiledCliApp[Any]],
      args: List[String],
  ): URIO[Scope, ExitCode] =
    Args.parse(args) match
      case Left(message)     => Console.printLine(message).orDie.as(ExitCode.failure)
      case Right(parsedArgs) => runWithSubCommandsFromArgs(rootParser, helpParser, subCommands, parsedArgs, rawArgs = Some(args))

}