package oxygen.executable
import oxygen.cli.*

object CompletionEngine {
  def complete(app: CompiledCliApp[?], request: CompletionRequest): List[String] =
    request.args match
      case value :: _ if request.argIdx == 0 && app.subCommands.nonEmpty => completeSubCommand(app, value, request)
      case _ =>
        activeApp(app, request) match
          case (sub, adjusted) => sub.helpParser.complete(adjusted, request.args.lift(request.argIdx).getOrElse(""))

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

  private def completeSubCommand(app: CompiledCliApp[?], value: String, request: CompletionRequest): List[String] =
    if app.subCommands.isEmpty then app.helpParser.complete(request, value)
    else
      val names = app.subCommands.keySet.toList.sorted
      val matches = names.filter(_.startsWith(value))
      if matches.nonEmpty then matches else names
}