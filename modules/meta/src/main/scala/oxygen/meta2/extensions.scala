package oxygen.meta2

import oxygen.core.syntax.functor.*
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.annotation.{tailrec, unused}
import scala.collection.mutable
import scala.quoted.*

extension [S[_], A](self: S[Expr[A]]) {

  /**
    * Expects [[S2]] to have an `S2.apply[A](values*)`
    */
  def unsafeSeqToExprOf[S2[_]](using s: SeqOps[S], sTpe: Type[S], s2Tpe: Type[S2], aTpe: Type[A], quotes: Quotes): Expr[S2[A]] =
    TypeRepr
      .of[S2[A]]
      .typeSymbol
      .companionModule
      .toTerm
      .select("apply")
      .appliedToType(TypeRepr.of[A])
      .appliedTo(Repeated.spread(self.map(_.toTerm).into[Seq], TypeTree.of[A]))
      .asExprOf[S2[A]]

  /**
    * Expects [[S2]] to have an `S2.apply[A](values*)`
    */
  def seqToExprOf[S2[_]](using s: SeqOps[S], @unused s2: SeqOps[S2], sTpe: Type[S], s2Tpe: Type[S2], aTpe: Type[A], q: Quotes): Expr[S2[A]] =
    self.unsafeSeqToExprOf[S2]

  /**
    * Expects [[S2]] to have an `S2.apply[A](values*)`
    */
  def seqToExpr(using s: SeqOps[S], sTpe: Type[S], aTpe: Type[A], q: Quotes): Expr[S[A]] =
    self.seqToExprOf[S]

}

extension (using quotes: Quotes)(self: quotes.reflect.Tree)
  def toIndentedString: IndentedString = {
    val opt: Matchable => Option[quotes.reflect.TypeTree] =
      m =>
        try { quotes.reflect.TypeTreeTypeTest.unapply(m.asInstanceOf[quotes.reflect.Tree]) }
        catch { case _ => None }
    val part: PartialFunction[Matchable, quotes.reflect.Tree] = opt.unlift
    IndentedString.fromAny(self) { case part(tt) => tt.toString }
  }

extension (self: Tree)
  def toIndentedString: IndentedString = {
    given Quotes = self.quotes
    self.unwrapWithin.toIndentedString
  }

extension [A](self: Expr[A]) {

  /**
    * Attempt to convert this Expr[A] into an A.
    * If it does not succeed, return None.
    */
  def evalOption(using from: FromExpr[A], quotes: Quotes): Option[A] =
    from.unapply(self)

  /**
    * Attempt to convert this Expr[A] into an A.
    * If it does not succeed, die.
    */
  def evalRequired(msg: String)(using from: FromExpr[A], quotes: Quotes): A =
    self.evalOption.getOrElse(report.errorAndAbort(s"Unable to extra Expr, msg = $msg\nexpr:\n${self.show}"))

  /**
    * Attempt to convert this Expr[A] into an A.
    * If it does not succeed, die.
    */
  def evalRequired(using from: FromExpr[A], quotes: Quotes): A =
    self.evalOption.getOrElse(report.errorAndAbort(s"Unable to extra Expr.\nexpr:\n${self.show}"))

  /**
    * Attempt to convert this Expr[A] into an A.
    * If it does not succeed, return a Left of the original expr.
    */
  def evalEither(using from: FromExpr[A], quotes: Quotes): Either[Expr[A], A] =
    self.evalOption.toRight(self)

}

@tailrec
private def combineStrings(
    queue: List[Expr[String]],
    combine: Option[String],
    acc: Growable[Expr[String]],
)(using Quotes): Growable[Expr[String]] =
  queue match {
    case head :: tail =>
      (combine, head.evalEither) match {
        case (Some(combine), Right(const)) => combineStrings(tail, (combine + const).some, acc)
        case (None, Left(expr))            => combineStrings(tail, None, acc :+ expr)
        case (None, Right(const))          => combineStrings(tail, const.some, acc)
        case (Some(combine), Left(expr))   => combineStrings(tail, None, acc :+ Expr(combine) :+ expr)
      }
    case Nil =>
      combine match {
        case Some(combine) => acc :+ Expr(combine)
        case None          => acc
      }
  }

extension [S[_]](self: S[Expr[String]]) {

  def exprMkString(builder: Expr[mutable.StringBuilder])(using Quotes, SeqOps[S]): Expr[Unit] =
    Expr.block(
      combineStrings(self.into[List], None, Growable.empty).to[List].map { s => '{ $builder.append($s) } },
      '{ () },
    )

  def exprMkString(using Quotes, SeqOps[S]): Expr[String] =
    combineStrings(self.into[List], None, Growable.empty).to[List] match {
      case single :: Nil => single
      case Nil           => Expr("")
      case many =>
        '{
          val builder = mutable.StringBuilder()
          ${
            Expr.block(
              many.map { s => '{ builder.append($s) } },
              '{ builder.result() },
            )
          }
        }
    }

  def exprMkString(sep: String)(using Quotes, SeqOps[S]): Expr[String] =
    self.intersperse(Expr(sep)).exprMkString

  def exprMkString(start: String, sep: String, end: String)(using Quotes, SeqOps[S]): Expr[String] =
    self.surround(Expr(start), Expr(sep), Expr(end)).exprMkString

}
