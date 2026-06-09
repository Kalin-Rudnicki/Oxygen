package oxygen.executable
import oxygen.cli.*
import zio.*

object CompletionEngine {
  private def flagCompletions(value: String, paramCompletions: List[String]): List[String] =
    val flags: List[String] = paramCompletions.filter(_.startsWith("-"))
    CliHelp.mergeCompletions(CliHelp.builtinFlagCompletions(value), flags)

  def complete(app: CompiledCliApp[?], request: CompletionRequest): Task[List[String]] =
    val value: String = request.args.lift(request.argIdx).getOrElse("")
    request.args match
      case _ if request.argIdx == 0 && app.subCommands.nonEmpty =>
        completeSubCommand(app, value, request)
      case _ =>
        activeApp(app, request) match
          case (sub, adjusted) =>
            val paramCompletions: Task[List[String]] =
              if value.startsWith("-") || value.isEmpty then sub.helpParser.complete(adjusted, value)
              else sub.rootParser.complete(adjusted, value)
            paramCompletions.map { paramCompletions =>
              if value.startsWith("-") then flagCompletions(value, paramCompletions)
              else if value.isEmpty then CliHelp.mergeCompletions(CliHelp.builtinFlagCompletions(value), paramCompletions)
              else paramCompletions
            }

  private def activeApp(app: CompiledCliApp[?], request: CompletionRequest): (CompiledCliApp[?], CompletionRequest) =
    if app.subCommands.isEmpty then (app, request)
    else
      request.args.lift(0).flatMap(app.subCommands.get) match
        case Some(sub) =>
          val adjusted = CompletionRequest(
            numWords = request.numWords,
            argIdx = (request.argIdx - 1).max(0),
            args = request.args.tail,
            joinStr = request.joinStr,
          )
          (sub, adjusted)
        case None => (app, request)

  private def completeSubCommand(app: CompiledCliApp[?], value: String, request: CompletionRequest): Task[List[String]] =
    if app.subCommands.isEmpty then
      if value.startsWith("-") then app.helpParser.complete(request, value).map(flagCompletions(value, _))
      else if value.isEmpty then
        app.helpParser.complete(request, value).map(pc => CliHelp.mergeCompletions(CliHelp.builtinFlagCompletions(value), pc))
      else app.helpParser.complete(request, value)
    else
      val names: List[String] = app.subCommands.keySet.toList.sorted
      val commandMatches: List[String] = names.filter(_.startsWith(value))
      val commands: List[String] = if commandMatches.nonEmpty then commandMatches else names
      if value.startsWith("-") then app.helpParser.complete(request, value).map(flagCompletions(value, _))
      else if value.isEmpty then ZIO.succeed(CliHelp.mergeCompletions(CliHelp.builtinFlagCompletions(value), commands))
      else ZIO.succeed(commands)
}
