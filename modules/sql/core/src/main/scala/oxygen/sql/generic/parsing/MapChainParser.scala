package oxygen.sql.generic.parsing

import oxygen.core.typeclass.Zip3
import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.model.part.*
import scala.annotation.tailrec
import scala.quoted.*

private[generic] trait MapChainParser[+A] {

  def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): MapChainParseResult[A]

}
private[generic] object MapChainParser {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Ops
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  extension [A](self: MapChainParser[A]) {

    def >>>[B](that: MapChainParser[B])(using zip: Zip[A, B]): MapChainParser[zip.Out] =
      Then(self, that, zip)

    def ||[B >: A](that: MapChainParser[B]): MapChainParser[B] =
      Or(self, that)

    def withReturning(using zip: Zip3[A, ReturningPart, RefMap]): Parser[Term, zip.Out] =
      new Parser[Term, zip.Out] {
        override def parse(input: Term)(using ParseContext, Quotes): ParseResult[zip.Out] =
          for {
            MapChainResult(aValue, aFunct, aRefs, aTerm) <- self.parse(input, RefMap.empty, "<init>")
            ret <- ParseContext.add("Returning") {
              ParseResult.validate(aFunct == "map")(aTerm, s"expected final `map`, not $aFunct\n\nparsed:\n$aValue").flatMap { _ =>
                ReturningPart.parse((aTerm, aRefs))
              }
            }
          } yield zip.zip(aValue, ret, aRefs)
      }

    def map[B](f: A => B): MapChainParser[B] =
      Mapped(self, f)

    def maybe: MapChainParser[Option[A]] =
      Maybe(self)

    def many: MapChainParser[List[A]] =
      Many(self)

    def withContext(ctx: String): MapChainParser[A] =
      WithContext(self, ctx)

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Combinators
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait Deferred[+A] extends MapChainParser[A] {

    lazy val deferTo: MapChainParser[A]

    override final def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): MapChainParseResult[A] =
      deferTo.parse(term, refs, prevFunction)

  }

  final case class Maybe[A](inner: MapChainParser[A]) extends MapChainParser[Option[A]] {

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): MapChainParseResult[Option[A]] =
      inner.parse(term, refs, prevFunction) match {
        case ParseResult.Success(MapChainResult(a, funct, refs, term)) => ParseResult.Success(MapChainResult(a.some, funct, refs, term))
        case _: ParseResult.Unknown                                    => ParseResult.Success(MapChainResult(None, prevFunction, refs, term))
        case error: ParseResult.Error                                  => error
      }

  }

  final case class Many[A](inner: MapChainParser[A]) extends MapChainParser[List[A]] {

    @tailrec
    private def loop(
        acc: Growable[A],
        refs: RefMap,
        term: Term,
        prevFunction: String,
    )(using ParseContext, Quotes): ParseResult.Known[MapChainResult[List[A]]] =
      inner.parse(term, refs, prevFunction) match
        case ParseResult.Success(MapChainResult(a, funct, refs, term)) => loop(acc :+ a, refs, term, funct)
        case _: ParseResult.Unknown                                    => ParseResult.Success(MapChainResult(acc.to[List], prevFunction, refs, term))
        case error: ParseResult.Error                                  => error

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): MapChainParseResult[List[A]] =
      loop(Growable.empty, refs, term, prevFunction)

  }

  final case class Mapped[A, B](a: MapChainParser[A], ab: A => B) extends MapChainParser[B] {

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): MapChainParseResult[B] =
      a.parse(term, refs, prevFunction).map { case MapChainResult(a, funct, refs, term) => MapChainResult(ab(a), funct, refs, term) }

  }

  final case class Then[A, B, C](a: MapChainParser[A], b: MapChainParser[B], zip: Zip.Out[A, B, C]) extends MapChainParser[C] {

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): MapChainParseResult[C] =
      for {
        MapChainResult(aValue, aFunct, aRefs, aTerm) <- a.parse(term, refs, prevFunction)
        MapChainResult(bValue, bFunct, bRefs, bTerm) <- b.parse(aTerm, aRefs, aFunct)
      } yield MapChainResult(zip.zip(aValue, bValue), bFunct, bRefs, bTerm)

  }

  final case class Or[A](a: MapChainParser[A], b: MapChainParser[A]) extends MapChainParser[A] {

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): MapChainParseResult[A] =
      a.parse(term, refs, prevFunction) match {
        case success @ ParseResult.Success(_) => success
        case unknown1: ParseResult.Unknown    =>
          b.parse(term, refs, prevFunction) match {
            case success @ ParseResult.Success(_) => success
            case unknown2: ParseResult.Unknown    => unknown1 ++ unknown2
            case error: ParseResult.Error         => error
          }
        case error: ParseResult.Error => error
      }

  }

  final case class WithContext[A](a: MapChainParser[A], ctx: String) extends MapChainParser[A] {

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): MapChainParseResult[A] =
      ParseContext.add(ctx) { a.parse(term, refs, prevFunction) }

  }

}
