package oxygen.executable
import oxygen.cli.*
object CompletionEngine {
  def complete(app: CompiledCliApp[?], request: CompletionRequest): List[String] =
    request.args match {
      case value :: remaining if request.argIdx == 0 => completeSubCommand(app, value, remaining, request)
      case _ => app.rootParser.complete(request, request.args.lift(request.argIdx).getOrElse(""))
    }
  private def completeSubCommand(app: CompiledCliApp[?], value: String, remaining: List[String], request: CompletionRequest): List[String] =
    if app.subCommands.isEmpty then app.rootParser.complete(request, value)
    else
      val names = app.subCommands.keySet.toList.sorted
      val matches = names.filter(_.startsWith(value))
      if matches.nonEmpty then matches else names
}
