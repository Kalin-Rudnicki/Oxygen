package oxygen.sql.generic

import oxygen.predef.core.*
import oxygen.quoted.*
import scala.annotation.tailrec
import scala.quoted.*

// FIX-PRE-MERGE (KR) : remove?
private[generic] trait QueryBlockParser[A] private {

  def parse(term: Term, refs: RefMap, prevFunctName: String)(using ParseContext, Quotes): ParseResult[(A, String, RefMap, Term)]

  def >>>[B](that: QueryBlockParser[B])(using zip: Zip[A, B]): QueryBlockParser[zip.Out] =
    QueryBlockParser.Zipped(this, that, zip)

  def withContext(ctx: String): QueryBlockParser[A] =
    QueryBlockParser.WithContext(this, ctx)

}
object QueryBlockParser {

  trait Required[A] extends QueryBlockParser[A] { self =>

    def parse(term: Term, refs: RefMap)(using ParseContext, Quotes): ParseResult[(A, String, RefMap, Term)]

    override final def parse(term: Term, refs: RefMap, prevFunctName: String)(using ParseContext, Quotes): ParseResult[(A, String, RefMap, Term)] =
      parse(term, refs)

    final def maybe: QueryBlockParser[Option[A]] = Maybe(this)
    final def many: QueryBlockParser[List[A]] = Many(this)

    override def >>>[B](that: QueryBlockParser[B])(using zip: Zip[A, B]): QueryBlockParser.Required[zip.Out] =
      QueryBlockParser.ZippedReq(this, that, zip)

    override def withContext(ctx: String): QueryBlockParser.Required[A] =
      QueryBlockParser.WithContextReq(this, ctx)

    def >>>[B](that: Parser[(Term, RefMap), B])(using zip: Zip[A, B]): Parser[(Term, RefMap), zip.Out] =
      new Parser[(Term, RefMap), zip.Out] {
        override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[zip.Out] =
          for {
            (aRes, aFunct, aRefs, aTerm) <- self.parse(input._1, input._2)
            _ <- ParseResult.validate(aFunct == "map")(aTerm, "expected to be at final `map`")
            bRes <- that.parse((aTerm, aRefs))
          } yield zip.zip(aRes, bRes)
      }

  }

  final case class Maybe[A](inner: QueryBlockParser.Required[A]) extends QueryBlockParser[Option[A]] {

    override def parse(term: Term, refs: RefMap, prevFunctName: String)(using ParseContext, Quotes): ParseResult[(Option[A], String, RefMap, Term)] =
      inner.parse(term, refs) match {
        case ParseResult.Success((a, funct, refs, term)) => ParseResult.Success((a.some, funct, refs, term))
        case error: ParseResult.Error                    => error
        case _: ParseResult.Unknown                      => ParseResult.Success((None, prevFunctName, refs, term))
      }

  }

  final case class Many[A](inner: QueryBlockParser.Required[A]) extends QueryBlockParser[List[A]] {

    @tailrec
    private def loop(
        acc: Growable[A],
        funct: String,
        refs: RefMap,
        term: Term,
    )(using ParseContext, Quotes): ParseResult.Known[(List[A], String, RefMap, Term)] =
      inner.parse(term, refs) match
        case ParseResult.Success((a, funct, refs, term)) => loop(acc :+ a, funct, refs, term)
        case error: ParseResult.Error                    => error
        case _: ParseResult.Unknown                      => ParseResult.Success((acc.to[List], funct, refs, term))

    override def parse(term: Term, refs: RefMap, prevFunctName: String)(using ParseContext, Quotes): ParseResult[(List[A], String, RefMap, Term)] =
      loop(Growable.empty, prevFunctName, refs, term)

  }

  final case class Zipped[A, B, C](a: QueryBlockParser[A], b: QueryBlockParser[B], zip: Zip.Out[A, B, C]) extends QueryBlockParser[C] {

    override def parse(term: Term, refs: RefMap, prevFunctName: String)(using ParseContext, Quotes): ParseResult[(C, String, RefMap, Term)] =
      for {
        (aRes, aFunct, aRefs, aTerm) <- a.parse(term, refs, prevFunctName)
        (bRes, bFunct, bRefs, bTerm) <- b.parse(aTerm, aRefs, aFunct)
      } yield (zip.zip(aRes, bRes), bFunct, bRefs, bTerm)

  }

  final case class ZippedReq[A, B, C](a: QueryBlockParser.Required[A], b: QueryBlockParser[B], zip: Zip.Out[A, B, C]) extends QueryBlockParser.Required[C] {

    override def parse(term: Term, refs: RefMap)(using ParseContext, Quotes): ParseResult[(C, String, RefMap, Term)] =
      for {
        (aRes, aFunct, aRefs, aTerm) <- a.parse(term, refs)
        (bRes, bFunct, bRefs, bTerm) <- b.parse(aTerm, aRefs, aFunct)
      } yield (zip.zip(aRes, bRes), bFunct, bRefs, bTerm)

  }

  final case class WithContext[A](inner: QueryBlockParser[A], ctx: String) extends QueryBlockParser[A] {

    override def parse(term: Term, refs: RefMap, prevFunctName: String)(using ParseContext, Quotes): ParseResult[(A, String, RefMap, Term)] =
      ParseContext.add(ctx) { inner.parse(term, refs, prevFunctName) }

  }

  final case class WithContextReq[A](inner: QueryBlockParser.Required[A], ctx: String) extends QueryBlockParser.Required[A] {

    override def parse(term: Term, refs: RefMap)(using ParseContext, Quotes): ParseResult[(A, String, RefMap, Term)] =
      ParseContext.add(ctx) { inner.parse(term, refs) }

  }

}
