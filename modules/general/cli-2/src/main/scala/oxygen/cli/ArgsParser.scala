package oxygen.cli

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      ArgsParser
//////////////////////////////////////////////////////////////////////////////////////////////////////

sealed trait ArgsParser[+A] {

  def help: Help

  def parseArgs(input: Args): CliParseResult[A, Args]

  def complete(request: CompletionRequest, partialValue: String): List[String] = Nil

}
object ArgsParser {
  object Unimplemented extends ArgsParser[Unit] {
    override val help: Help = Help.Empty
    override def parseArgs(input: Args): CliParseResult[Unit, Args] = ???
  }
}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      PositionalArgsParser
//////////////////////////////////////////////////////////////////////////////////////////////////////

sealed trait PositionalArgsParser[+A] extends ArgsParser[A] {

  override final def parseArgs(input: Args): CliParseResult[A, Args] =
    parsePositionalArgs(input.positional).mapRemaining { remaining => Args(remaining, input.named) }

  def parsePositionalArgs(input: PositionalArgs): CliParseResult[A, PositionalArgs]

}
object PositionalArgsParser {

  trait Builder[+A] {
    def build(name: String, help: SubHelp): PositionalArgsParser[A]
  }

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      NamedArgsParser
//////////////////////////////////////////////////////////////////////////////////////////////////////

sealed trait NamedArgsParser[+A] extends ArgsParser[A] {

  override final def parseArgs(input: Args): CliParseResult[A, Args] =
    parseNamedArgs(input.named).mapRemaining { remaining => Args(input.positional, remaining) }

  def parseNamedArgs(input: NamedArgs): CliParseResult[A, NamedArgs]

}
object NamedArgsParser {

  trait Builder[+A] {
    def build(name: String, help: SubHelp): NamedArgsParser[A]
  }

}
