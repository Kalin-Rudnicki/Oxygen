package oxygen.cli

sealed trait CliParseError {

  final def onlyContainsMissingRequired: Boolean = this match
    case CliParseError.MissingRequiredPositional(_)  => true
    case CliParseError.MissingRequiredNamed(_)       => true
    case CliParseError.PositionalOr(left, right)     => left.onlyContainsMissingRequired && right.onlyContainsMissingRequired
    case CliParseError.NamedOr(left, right)          => left.onlyContainsMissingRequired && right.onlyContainsMissingRequired
    case CliParseError.RootOr(left, right)           => left.onlyContainsMissingRequired && right.onlyContainsMissingRequired
    case CliParseError.RootAnd(left, right)          => left.onlyContainsMissingRequired && right.onlyContainsMissingRequired
    case _                                           => false

}
object CliParseError {

  final case class UnableToParseArgs(message: String) extends CliParseError

  final case class MissingRequiredPositional(name: String) extends CliParseError
  final case class MissingRequiredNamed(name: String) extends CliParseError

  final case class ExpectedPositionalArg(name: String) extends CliParseError
  final case class FailedValidation(message: String) extends CliParseError

  final case class UnparsedPositional(args: NonEmptyList[PositionalArg]) extends CliParseError
  final case class UnparsedNamed(args: NonEmptyList[NamedArg]) extends CliParseError

  final case class PositionalOr(left: CliParseError, right: CliParseError) extends CliParseError
  final case class NamedOr(left: CliParseError, right: CliParseError) extends CliParseError
  final case class RootOr(left: CliParseError, right: CliParseError) extends CliParseError
  final case class RootAnd(left: CliParseError, right: CliParseError) extends CliParseError

  final case class NamedUnexpectedValues(name: String, values: NonEmptyList[PositionalArg]) extends CliParseError

}