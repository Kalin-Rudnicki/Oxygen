package oxygen.sql.generic.parsing

import oxygen.core.typeclass.Zip3
import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.parsing.part.*
import scala.annotation.tailrec
import scala.quoted.*

private[generic] trait QueryParser[+A] {

  def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): ParseResult[(A, String, RefMap, Term)]

}
private[generic] object QueryParser {

  final case class Result[+A](
      value: A,
      funName: String,
      b: RefMap,
      c: Term,
  )

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Parts
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Aggregate
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  enum PartialQuery {

    case InsertQuery(
        insert: InsertQ,
        into: Into,
    )

    case SelectQuery(
        select: SelectQ,
        joins: List[Join],
        where: Option[Where],
    )

    case UpdateQuery(
        update: UpdateQ,
        joins: List[Join],
        where: Option[Where],
        set: SetPart,
    )

    case DeleteQuery(
        delete: DeleteQ,
        joins: List[Join],
        where: Option[Where],
    )

  }

  val partialInsert: QueryParser[PartialQuery.InsertQuery] =
    (
      InsertQ.withContext("Insert") >>>
        Into.withContext("Into")
    ).withContext("Insert Query").map { PartialQuery.InsertQuery.apply }

  val partialSelect: QueryParser[PartialQuery.SelectQuery] =
    (
      SelectQ.withContext("Select") >>>
        Join.many.withContext("Join") >>>
        Where.maybe.withContext("Where")
    ).withContext("Select Query").map { PartialQuery.SelectQuery.apply }

  val partialUpdate: QueryParser[PartialQuery.UpdateQuery] =
    (
      UpdateQ.withContext("Update") >>>
        Join.many.withContext("Join") >>>
        Where.maybe.withContext("Where") >>>
        SetPart.withContext("Set")
    ).withContext("Update Query").map { PartialQuery.UpdateQuery.apply }

  val partialDelete: QueryParser[PartialQuery.DeleteQuery] =
    (
      DeleteQ.withContext("Delete") >>>
        Join.many.withContext("Join") >>>
        Where.maybe.withContext("Where")
    ).withContext("Delete Query").map { PartialQuery.DeleteQuery.apply }

  val partialQuery: QueryParser[PartialQuery] =
    partialInsert || partialSelect || partialUpdate || partialDelete

  val finished: Parser[Term, (List[Input], PartialQuery, Returning, RefMap)] =
    (Input.many.withContext("Input") >>> partialQuery).withReturning

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Ops
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  extension [A](self: QueryParser[A]) {

    def >>>[B](that: QueryParser[B])(using zip: Zip[A, B]): QueryParser[zip.Out] =
      Then(self, that, zip)

    def ||[B >: A](that: QueryParser[B]): QueryParser[B] =
      Or(self, that)

    def withReturning(using zip: Zip3[A, Returning, RefMap]): Parser[Term, zip.Out] =
      new Parser[Term, zip.Out] {
        override def parse(input: Term)(using ParseContext, Quotes): ParseResult[zip.Out] =
          for {
            (aValue, aFunct, aRefs, aTerm) <- self.parse(input, RefMap.empty, "<init>")
            ret <- ParseContext.add("Returning") {
              ParseResult.validate(aFunct == "map")(aTerm, s"expected final `map`, not $aFunct\n\nparsed:\n$aValue").flatMap { _ =>
                Returning.parse((aTerm, aRefs))
              }
            }
          } yield zip.zip(aValue, ret, aRefs)
      }

    def map[B](f: A => B): QueryParser[B] =
      Mapped(self, f)

    def maybe: QueryParser[Option[A]] =
      Maybe(self)

    def many: QueryParser[List[A]] =
      Many(self)

    def withContext(ctx: String): QueryParser[A] =
      WithContext(self, ctx)

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Combinators
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Maybe[A](inner: QueryParser[A]) extends QueryParser[Option[A]] {

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): ParseResult[(Option[A], String, RefMap, Term)] =
      inner.parse(term, refs, prevFunction) match {
        case ParseResult.Success(a, funct, refs, term) => ParseResult.Success(a.some, funct, refs, term)
        case _: ParseResult.Unknown                    => ParseResult.Success(None, prevFunction, refs, term)
        case error: ParseResult.Error                  => error
      }

  }

  final case class Many[A](inner: QueryParser[A]) extends QueryParser[List[A]] {

    @tailrec
    private def loop(
        acc: Growable[A],
        refs: RefMap,
        term: Term,
        prevFunction: String,
    )(using ParseContext, Quotes): ParseResult.Known[(List[A], String, RefMap, Term)] =
      inner.parse(term, refs, prevFunction) match
        case ParseResult.Success((a, funct, refs, term)) => loop(acc :+ a, refs, term, funct)
        case _: ParseResult.Unknown                      => ParseResult.Success((acc.to[List], prevFunction, refs, term))
        case error: ParseResult.Error                    => error

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): ParseResult[(List[A], String, RefMap, Term)] =
      loop(Growable.empty, refs, term, prevFunction)

  }

  final case class Mapped[A, B](a: QueryParser[A], ab: A => B) extends QueryParser[B] {

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): ParseResult[(B, String, RefMap, Term)] =
      a.parse(term, refs, prevFunction).map { case (a, funct, refs, term) => (ab(a), funct, refs, term) }

  }

  final case class Then[A, B, C](a: QueryParser[A], b: QueryParser[B], zip: Zip.Out[A, B, C]) extends QueryParser[C] {

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): ParseResult[(C, String, RefMap, Term)] =
      for {
        (aValue, aFunct, aRefs, aTerm) <- a.parse(term, refs, prevFunction)
        (bValue, bFunct, bRefs, bTerm) <- b.parse(aTerm, aRefs, aFunct)
      } yield (zip.zip(aValue, bValue), bFunct, bRefs, bTerm)

  }

  final case class Or[A](a: QueryParser[A], b: QueryParser[A]) extends QueryParser[A] {

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): ParseResult[(A, String, RefMap, Term)] =
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

  final case class WithContext[A](a: QueryParser[A], ctx: String) extends QueryParser[A] {

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): ParseResult[(A, String, RefMap, Term)] =
      ParseContext.add(ctx) { a.parse(term, refs, prevFunction) }

  }

}
