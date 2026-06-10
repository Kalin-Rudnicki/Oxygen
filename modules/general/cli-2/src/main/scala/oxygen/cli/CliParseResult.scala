package oxygen.cli

sealed trait CliParseResult[+V, A] {

  final def mapRemaining[A2](f: A => A2): CliParseResult[V, A2] = this match
    case CliParseResult.Success(value, remaining) => CliParseResult.Success(value, f(remaining))
    case fail @ CliParseResult.Fail(_, _)         => fail

  final def map[B](f: V => B): CliParseResult[B, A] = this match
    case CliParseResult.Success(value, remaining) => CliParseResult.Success(f(value), remaining)
    case fail @ CliParseResult.Fail(_, _)         => fail

  final def flatMap[B](f: V => CliParseResult[B, A]): CliParseResult[B, A] = this match
    case CliParseResult.Success(value, remaining) =>
      f(value) match
        case CliParseResult.Success(value2, remaining2) => CliParseResult.Success(value2, remaining2)
        case fail @ CliParseResult.Fail(_, _)           => fail
    case fail @ CliParseResult.Fail(_, _) => fail

  final def mapOrFail[B](f: V => Either[String, B])(help: Help): CliParseResult[B, A] = this match
    case CliParseResult.Success(value, remaining) =>
      f(value) match
        case Right(value2) => CliParseResult.Success(value2, remaining)
        case Left(message) => CliParseResult.Fail(CliParseError.FailedValidation(message), help.withHints(HelpHint.Error(message) :: Nil))
    case fail @ CliParseResult.Fail(_, _) => fail

}
object CliParseResult {

  final case class Success[+V, A](value: V, remaining: A) extends CliParseResult[V, A]

  final case class Fail(error: CliParseError, help: Help) extends CliParseResult[Nothing, Nothing]

}