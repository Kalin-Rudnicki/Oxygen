package oxygen.executable

import oxygen.cli.*
import zio.*

sealed trait CompiledCliApp[R] {
  def rootParser: ArgsParser[?]
  def helpParser: ArgsParser[?]
  def subCommands: Map[String, CompiledCliApp[R]] = Map.empty
  final def complete(request: CompletionRequest): Task[List[String]] = CompletionEngine.complete(this, request)
  def run(args: List[String]): URIO[Scope, ExitCode]
}
object CompiledCliApp {

  final case class Impl[R](
      rootParser: ArgsParser[?],
      helpParser: ArgsParser[?],
      override val subCommands: Map[String, CompiledCliApp[R]],
      runFn: List[String] => URIO[Scope, ExitCode],
      runWithRootFn: Option[(Any, Args) => URIO[Scope, ExitCode]] = None,
  ) extends CompiledCliApp[R] {
    override def run(args: List[String]): URIO[Scope, ExitCode] = runFn(args)
    def runWithRoot(rootParsed: Any, args: Args): URIO[Scope, ExitCode] =
      runWithRootFn.fold(ZIO.dieMessage("subcommand runWithRoot not configured"))(_(rootParsed, args))
  }

  final case class Unimplemented[R]() extends CompiledCliApp[R] {
    override val rootParser: ArgsParser[?] = ArgsParser.Unimplemented
    override val helpParser: ArgsParser[?] = ArgsParser.Unimplemented
    override def run(args: List[String]): URIO[Scope, ExitCode] = ZIO.dieMessage("unimplemented")
  }

}
