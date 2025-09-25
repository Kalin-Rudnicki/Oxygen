package oxygen.sql.generic.parsing

import scala.quoted.*

private[generic] trait Parser[-A, +B] {

  def parse(input: A)(using ParseContext, Quotes): ParseResult[B]

  object required {
    def unapply(input: A)(using ParseContext, Quotes): Some[ParseResult[B]] =
      Some(parse(input))
  }

  object optional {
    def unapply(input: A)(using ParseContext, Quotes): Option[ParseResult.Known[B]] =
      parse(input).toKnown
  }

  final def map[C](f: B => C): Parser[A, C] = Parser.Mapped(this, f)

}
object Parser {

  final case class Mapped[A, B, C](inner: Parser[A, B], f: B => C) extends Parser[A, C] {
    override def parse(input: A)(using ParseContext, Quotes): ParseResult[C] = inner.parse(input).map(f)
  }

}
