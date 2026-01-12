package oxygen.executable

import oxygen.cli.*
import oxygen.executable.error.ExecuteError
import zio.*

object Parsing {

  def parse[A](parser: Parser[A], args: List[String]): IO[ExecuteError.Parsing, A] =
    for {
      built <- ZIO.fromEither(parser.build).mapError(ExecuteError.Parsing.FailedToBuild(_))
      helpOrValue <- ZIO.fromEither(built(args).toEither).mapError(ExecuteError.Parsing.FailedToParse(_, _))
      value <- helpOrValue match {
        case Right(value)   => ZIO.succeed(value)
        case Left(helpType) => ZIO.fail(ExecuteError.Parsing.Help(built.helpMessage, helpType))
      }
    } yield value

}
