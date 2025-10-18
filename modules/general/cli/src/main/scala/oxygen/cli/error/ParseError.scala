package oxygen.cli.error

import oxygen.cli.*
import oxygen.predef.base.*

sealed trait ParseError {

  def toIndentedString: IndentedString =
    this match {
      case ParseError.SingleValueError(scope, cause) => IndentedString.section(scope.map(_.simpleShow).mkString(", "))(cause.toIndentedString)
      case ParseError.ValueErrorOr(left, right)      => IndentedString.inline(IndentedString.indented(left.toIndentedString), "OR", IndentedString.indented(right.toIndentedString))
      case ParseError.UnparsedValues(args)           => IndentedString.section("Unparsed values:")(args.toList.map(_.toString))
      case ParseError.SingleParamError(scope, cause) => IndentedString.section(scope.map(_.simpleShow).mkString(", "))(cause.toIndentedString)
      case ParseError.ParamErrorAnd(left, right)     => IndentedString.inline(left.toIndentedString, right.toIndentedString)
      case ParseError.ParamErrorOr(left, right)      => IndentedString.inline(IndentedString.indented(left.toIndentedString), "OR", IndentedString.indented(right.toIndentedString))
      case ParseError.UnparsedParams(args)           => IndentedString.section("Unparsed params:")(args.toList.map(_.toString))
      case ParseError.UnableToParseArgs(error)       => s"Unable to parse args: $error"
      case ParseError.RootAnd(left, right)           => IndentedString.inline(left.toIndentedString, right.toIndentedString)
      case ParseError.RootOr(left, right)            => IndentedString.inline(IndentedString.indented(left.toIndentedString), "OR", IndentedString.indented(right.toIndentedString))
      case ParseError.RootValidation(scope, error)   => IndentedString.section(scope.map(_.simpleShow).mkString(", "))(error.toIndentedString)
    }

  override def toString: String = toIndentedString.toString

}
object ParseError {

  final case class FailedValidation(error: String) extends ValueCause with ParamCause {

    override def toIndentedString: IndentedString = s"Failed validation - $error"

  }

  // =====| Value |=====

  sealed trait ValueError extends ParseError {

    final def onlyContainsMissingRequiredValue: Boolean = this match
      case SingleValueError(_, MissingRequiredValue) => true
      case SingleValueError(_, _)                    => false
      case ValueErrorOr(left, right)                 => left.onlyContainsMissingRequiredValue && right.onlyContainsMissingRequiredValue

  }
  object ValueError {

    def apply(name: LongName, cause: ValueCause): ValueError =
      SingleValueError(ParsedValueArg(name :: Nil, Nil) :: Nil, cause)

    def apply(name: LongName, arg: Arg.ValueLike, cause: ValueCause): ValueError =
      SingleValueError(ParsedValueArg(name :: Nil, arg :: Nil) :: Nil, cause)

  }

  sealed trait ValueCause {

    def toIndentedString: IndentedString = this match
      case MissingRequiredValue  => "Missing required value"
      case ExpectedValueArg      => "Expected plain value arg"
      case ExpectedBracketedArg  => "Expected bracketed value arg"
      case BracketedError(error) => IndentedString.section("Error within brackets:")(error.toIndentedString)
      case FailedValidation(_)   => ??? // scala compiler... why...

    override def toString: String = toIndentedString.toString

  }

  case object MissingRequiredValue extends ValueCause
  case object ExpectedValueArg extends ValueCause
  case object ExpectedBracketedArg extends ValueCause
  final case class BracketedError(error: ParseError) extends ValueCause

  final case class SingleValueError(scope: List[ParsedValueArg], cause: ValueCause) extends ValueError
  final case class ValueErrorOr(left: ValueError, right: ValueError) extends ValueError
  final case class UnparsedValues(args: NonEmptyList[Arg.ValueLike]) extends ParseError

  // =====| Param |=====

  sealed trait ParamError extends ParseError {

    final def onlyContainsMissingRequiredParam: Boolean = this match
      case SingleParamError(_, MissingRequiredParam) => true
      case SingleParamError(_, _)                    => false
      case ParamErrorAnd(left, right)                => left.onlyContainsMissingRequiredParam && right.onlyContainsMissingRequiredParam
      case ParamErrorOr(left, right)                 => left.onlyContainsMissingRequiredParam && right.onlyContainsMissingRequiredParam // TODO (KR) : `||` (?)

  }
  object ParamError {

    def apply(name: LongReference, cause: ParamCause): ParamError =
      SingleParamError(ParsedParamArg(name :: Nil, Nil) :: Nil, cause)

    def apply(name: LongReference, arg: Arg.ParamLike, cause: ParamCause): ParamError =
      SingleParamError(ParsedParamArg(name :: Nil, arg :: Nil) :: Nil, cause)

  }

  sealed trait ParamCause {

    def toIndentedString: IndentedString = this match
      case MissingRequiredParam                         => "Missing required param"
      case ParamValuesValidation(error: ValueError)     => error.toIndentedString
      case ParamValuesValidation(error: UnparsedValues) => error.toIndentedString
      case FailedValidation(_)                          => ??? // scala compiler... why...

    override def toString: String = toIndentedString.toString

  }

  case object MissingRequiredParam extends ParamCause
  final case class ParamValuesValidation(error: ValueError | UnparsedValues) extends ParamCause

  final case class SingleParamError(scope: List[ParsedParamArg], cause: ParamCause) extends ParamError
  final case class ParamErrorAnd(left: ParamError, right: ParamError) extends ParamError
  final case class ParamErrorOr(left: ParamError, right: ParamError) extends ParamError
  final case class UnparsedParams(args: NonEmptyList[Arg.ParamLike]) extends ParseError

  // =====| Root |=====

  final case class UnableToParseArgs(error: String) extends ParseError

  final case class RootAnd(left: ParseError, right: ParseError) extends ParseError
  final case class RootOr(left: ParseError, right: ParseError) extends ParseError
  final case class RootValidation(scope: List[ParsedArg], error: FailedValidation) extends ParseError

}
