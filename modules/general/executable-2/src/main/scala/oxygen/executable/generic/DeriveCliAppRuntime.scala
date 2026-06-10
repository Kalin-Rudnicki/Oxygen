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

  private val commandBlockSeparator: Help = Help.BlankLine

  private def joinWithSpacing(helps: List[Help]): Help =
    helps match
      case Nil          => Help.Empty
      case one :: Nil   => one
      case head :: tail => tail.foldLeft(head)((acc, next) => Help.And(Help.And(acc, commandBlockSeparator), next))

  private def titleForPath(path: List[String]): Option[Help] =
    if path.isEmpty then Some(Help.Raw("Usage")) else Some(Help.CommandTitle(path.mkString(" ")))

  private def expandSubCommandsHelp(subCommands: Map[String, CompiledCliApp[Any]], pathPrefix: List[String]): Help =
    joinWithSpacing(
      subCommands.toList.sortBy(_._1).map { case (name, cmd) =>
        val path = pathPrefix :+ name
        Help.And(Help.CommandTitle(path.mkString(" ")), expandCommandHelp(cmd, path))
      },
    )

  private def expandCommandHelp(cmd: CompiledCliApp[Any], pathPrefix: List[String]): Help =
    if cmd.subCommands.isEmpty then cmd.helpParser.help
    else Help.And(Help.And(cmd.helpParser.help, commandBlockSeparator), expandSubCommandsHelp(cmd.subCommands, pathPrefix))

  def printComposedHelp(
      helpParser: ArgsParser[?],
      helpType: HelpType,
      subCommands: Map[String, CompiledCliApp[Any]] = Map.empty,
      commandPath: List[String] = Nil,
  ): URIO[Scope, ExitCode] =
    val expandedSubCommands: Option[Help] =
      if helpType == HelpType.HelpExtra && subCommands.nonEmpty then Some(expandSubCommandsHelp(subCommands, commandPath))
      else None
    printHelp(
      CliHelp.compose(
        helpParser,
        helpType,
        subCommands.keySet,
        titleForPath(commandPath),
        expandedSubCommands,
      ),
    )

  private def drillHelp(
      helpParser: ArgsParser[?],
      subCommands: Map[String, CompiledCliApp[Any]],
      args: Args,
      helpType: HelpType,
      commandPath: List[String],
  ): URIO[Scope, ExitCode] =
    args.positional.args match
      case PositionalArg(_, name) :: tail if subCommands.nonEmpty =>
        subCommands.get(name) match
          case Some(cmd) if tail.nonEmpty =>
            drillHelp(cmd.helpParser, cmd.subCommands, Args(PositionalArgs(tail), args.named), helpType, commandPath :+ name)
          case Some(cmd) =>
            printComposedHelp(cmd.helpParser, helpType, cmd.subCommands, commandPath = commandPath :+ name)
          case None =>
            printComposedHelp(helpParser, helpType, subCommands, commandPath = commandPath)
      case _ =>
        printComposedHelp(helpParser, helpType, subCommands, commandPath = commandPath)

  def handleHelpOr(
      args: Args,
      helpParser: ArgsParser[?],
      subCommands: Map[String, CompiledCliApp[Any]],
      commandPath: List[String] = Nil,
      run: Args => URIO[Scope, ExitCode],
  ): URIO[Scope, ExitCode] =
    CliHelp.peelArgs(args) match
      case (stripped, Some(helpType)) => drillHelp(helpParser, subCommands, stripped, helpType, commandPath)
      case (stripped, None)           => run(stripped)

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
                      handleHelpOr(cmdArgs, cmd.helpParser, cmd.subCommands, commandPath = List(cmdName), run = cmdStripped =>
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
      commandPath: List[String],
  ): URIO[Scope, ExitCode] =
    rootParser.parseArgs(stripped) match
      case CliParseResult.Fail(_, help) => printHelp(help)
      case CliParseResult.Success(rootParsed, remaining) =>
        remaining.positional.args match
          case PositionalArg(_, name) :: posTail =>
            subCommands.get(name) match
              case Some(cmd) =>
                val cmdArgs = Args(PositionalArgs(posTail), remaining.named)
                handleHelpOr(cmdArgs, cmd.helpParser, cmd.subCommands, commandPath = commandPath :+ name, run = cmdStripped =>
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
      commandPath: List[String] = Nil,
  ): URIO[Scope, ExitCode] =
    handleHelpOr(parsedArgs, helpParser, subCommands, commandPath = commandPath, run = stripped =>
      rawArgs match
        case Some(raw) => runWithSubCommandsSplit(rootParser, subCommands, raw)
        case None      => runWithSubCommandsLegacy(rootParser, subCommands, stripped, commandPath),
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