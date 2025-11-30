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
  final def contraMap[C](f: C => A): Parser[C, B] = Parser.ContraMapped(this, f)

}
object Parser {

  final case class Mapped[A, B, C](inner: Parser[A, B], f: B => C) extends Parser[A, C] {
    override def parse(input: A)(using ParseContext, Quotes): ParseResult[C] = inner.parse(input).map(f)
  }

  final case class ContraMapped[A, B, C](inner: Parser[B, C], f: A => B) extends Parser[A, C] {
    override def parse(input: A)(using ParseContext, Quotes): ParseResult[C] = inner.parse(f(input))
  }

  abstract class Deferred[A, B] extends Parser[A, B] {
    val parser: Parser[A, B]
    override final def parse(input: A)(using ParseContext, Quotes): ParseResult[B] = parser.parse(input)
  }

}
