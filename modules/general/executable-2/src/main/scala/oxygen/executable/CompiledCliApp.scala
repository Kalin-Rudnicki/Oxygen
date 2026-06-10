package oxygen.executable
import oxygen.cli.*
sealed trait CompiledCliApp[R] {
  def rootParser: ArgsParser[?]
  def subCommands: Map[String, CompiledCliApp[R]] = Map.empty
  final def complete(request: CompletionRequest): List[String] = CompletionEngine.complete(this, request)
}
object CompiledCliApp {
  final case class Unimplemented[R]() extends CompiledCliApp[R] {
    override val rootParser: ArgsParser[?] = ArgsParser.Unimplemented
  }
}
