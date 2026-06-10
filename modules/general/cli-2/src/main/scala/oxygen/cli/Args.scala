package oxygen.cli

final case class Args(positional: PositionalArgs, named: NamedArgs)
object Args {

  def parse(args: List[String]): Either[String, Args] =
    Arg.parse(args)

  def empty: Args =
    Args(PositionalArgs(Nil), NamedArgs(Nil))

  final def isFullyConsumed: Boolean =
    positional.args.isEmpty && named.args.isEmpty

}
final case class PositionalArgs(args: List[PositionalArg])
object PositionalArgs {

  def empty: PositionalArgs = PositionalArgs(Nil)

}
final case class NamedArgs(args: List[NamedArg])
object NamedArgs {

  def empty: NamedArgs = NamedArgs(Nil)

}