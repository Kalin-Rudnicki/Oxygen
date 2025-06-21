package oxygen.sql.generic

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

}
