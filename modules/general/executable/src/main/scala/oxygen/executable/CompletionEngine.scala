package oxygen.executable
import oxygen.cli.*
import zio.*

object CompletionEngine {
  private def flagCompletions(value: String, paramCompletions: List[String]): List[String] =
    val flags: List[String] = paramCompletions.filter(_.startsWith("-"))
    CliHelp.mergeCompletions(CliHelp.builtinFlagCompletions(value), flags)

  def complete(app: CompiledCliApp[?], request: CompletionRequest): Task[List[String]] =
    val value: String = request.currentValue
    request.args match
      case _ if request.argIdx == 0 && app.subCommands.nonEmpty =>
        completeSubCommand(app, value, request)
      case _ =>
        activeApp(app, request) match
          case (sub, adjusted) if drilledIntoSubCommand(request, adjusted) =>
            complete(sub, adjusted)
          case (sub, adjusted) =>
            completeParser(sub, adjusted, value)

  private def completeParser(app: CompiledCliApp[?], request: CompletionRequest, value: String): Task[List[String]] =
    if value.isEmpty then
      app.helpParser.complete(request, value).zipWith(app.rootParser.complete(request, value)) { (fromHelp, fromRoot) =>
        val valueCompletions = (fromHelp ::: fromRoot).filterNot(_.startsWith("-")).distinct
        if valueCompletions.nonEmpty then valueCompletions
        else
          CliHelp.mergeCompletions(
            CliHelp.builtinFlagCompletions(value),
            CliHelp.paramNameCompletions(app.helpParser.help, value),
          )
      }
    else if value.startsWith("-") then
      app.helpParser.complete(request, value).map(flagCompletions(value, _))
    else
      app.rootParser.complete(request, value)

  private def drilledIntoSubCommand(request: CompletionRequest, adjusted: CompletionRequest): Boolean =
    adjusted.args != request.args || adjusted.argIdx != request.argIdx

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
    if app.subCommands.isEmpty then completeParser(app, request, value)
    else
      app.subCommands.get(value) match
        case Some(sub) if request.args.length == 1 && !value.startsWith("-") =>
          complete(sub, CompletionRequest(0, 0, Nil, request.joinStr))
        case _ =>
          val names: List[String] = app.subCommands.keySet.toList.sorted
          val commandMatches: List[String] = names.filter(_.startsWith(value))
          val commands: List[String] = if commandMatches.nonEmpty then commandMatches else names
          if value.startsWith("-") then completeParser(app, request, value)
          else if value.isEmpty then ZIO.succeed(CliHelp.mergeCompletions(CliHelp.builtinFlagCompletions(value), commands))
          else ZIO.succeed(commands)
}
