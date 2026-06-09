package oxygen.cli

final case class Args(positional: PositionalArgs, named: NamedArgs)
object Args {
  
  def parse(args: List[String]): Either[String, Args] =
    ??? // FIX-PRE-MERGE (KR) : 
  
}

final case class PositionalArgs(args: List[PositionalArg])

final case class NamedArgs(args: List[NamedArg])
