package oxygen.meta

import oxygen.core.syntax.functor.*
import oxygen.meta
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.annotation.{tailrec, targetName, unused}
import scala.collection.mutable
import scala.quoted.*
import scala.reflect.ClassTag

extension [S[_], A](self: S[Expr[A]]) {

  /**
    * Will try to create a static ArraySeq creation, if the compiler can find a [[ClassTag]] for [[A]],
    * otherwise, it will create a norma [[Seq]].
    */
  def seqAttemptArraySeqExpr(using s: SeqRead[S], sTpe: Type[S], aTpe: Type[A], q: Quotes): Expr[Seq[A]] =
    Implicits.searchOption[ClassTag[A]] match {
      case Some(ct) => '{ ArraySeq.apply[A](${ Expr.ofSeq(self.into[Seq]) }*)(using $ct) }
      case None     => '{ Seq.apply[A](${ Expr.ofSeq(self.into[Seq]) }*) }
    }

  /**
    * Will try to create a static ArraySeq creation, if the compiler can find a [[ClassTag]] for [[A]],
    * otherwise, the compiler will blow up saying that it can't find a [[ClassTag]] for [[A]].
    */
  def seqToArraySeqExpr(using s: SeqRead[S], sTpe: Type[S], aTpe: Type[A], q: Quotes): Expr[ArraySeq[A]] =
    '{ ArraySeq.apply[A](${ Expr.ofSeq(self.into[Seq]) }*)(using ${ Implicits.searchRequiredIgnoreExplanation[ClassTag[A]] }) }

}

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

extension (using quotes: Quotes)(self: quotes.reflect.TypeRepr) {
  @targetName("typeReprToIndentedString")
  def toIndentedString: IndentedString =
    IndentedString.fromAny(self)
}

extension (using quotes: Quotes)(self: quotes.reflect.Tree) {
  @targetName("treeToIndentedString")
  def toIndentedString: IndentedString = {
    val opt: Matchable => Option[quotes.reflect.TypeTree] =
      m =>
        try { quotes.reflect.TypeTreeTypeTest.unapply(m.asInstanceOf[quotes.reflect.Tree]) }
        catch { case _ => None }
    val part: PartialFunction[Matchable, quotes.reflect.TypeTree] = opt.unlift
    IndentedString.fromAny(self) { case part(tt) => tt.tpe.toIndentedString }
  }
}

extension (self: TypeRepr)
  def toIndentedString: IndentedString = {
    given Quotes = self.quotes
    self.unwrapWithin.toIndentedString
  }

extension (self: Tree) {

  def toIndentedString: IndentedString = {
    given Quotes = self.quotes
    self.unwrapWithin.toIndentedString
  }

  def showDetailed(label: String): String =
    s"\n=====| Expr : $label |=====\n\n${self.showAnsiCode}\n\n=====| Tree : $label |=====\n\n${self.toIndentedString.toStringColorized}\n"

}

@tailrec
private def combineStrings(
    queue: List[StringExpr],
    combine: Option[String],
    acc: Growable[StringExpr],
)(using Quotes): Growable[StringExpr] =
  queue match {
    case StringExpr.Str(head) :: tail =>
      (combine, head.evalEither) match {
        case (Some(combine), Right(const)) => combineStrings(tail, (combine + const).some, acc)
        case (None, Left(expr))            => combineStrings(tail, None, acc :+ StringExpr.Str(expr))
        case (None, Right(const))          => combineStrings(tail, const.some, acc)
        case (Some(combine), Left(expr))   => combineStrings(tail, None, acc :+ StringExpr.const(combine) :+ StringExpr.Str(expr))
      }
    case (head: StringExpr.StrBuilder) :: tail =>
      combine match {
        case Some(combine) => combineStrings(tail, None, acc :+ StringExpr.const(combine) :+ head)
        case None          => combineStrings(tail, None, acc :+ head)
      }
    case Nil =>
      combine match {
        case Some(combine) => acc :+ StringExpr.const(combine)
        case None          => acc
      }
  }

extension [S[_]](self: S[StringExpr]) {

  def mergeConstStrings(using Quotes, SeqOps[S]): Growable[StringExpr] =
    combineStrings(self.into[List], None, Growable.empty)

  @targetName("stringExpr_exprMkStringTo")
  def exprMkStringTo(builder: Expr[mutable.StringBuilder])(using Quotes, SeqOps[S]): Expr[Unit] =
    Expr.block(
      self.mergeConstStrings
        .map {
          case StringExpr.Str(expr)        => '{ $builder.append($expr) }
          case StringExpr.StrBuilder(expr) => expr(builder)
        }
        .to[List],
      '{ () },
    )

  @targetName("stringExpr_exprMkStringTo")
  def exprMkStringTo(builder: Expr[mutable.StringBuilder], sep: StringExpr)(using Quotes, SeqOps[S]): Expr[Unit] =
    self.intersperse(sep).exprMkStringTo(builder)

  @targetName("stringExpr_exprMkStringTo")
  def exprMkStringTo(builder: Expr[mutable.StringBuilder], start: StringExpr, sep: StringExpr, end: StringExpr)(using Quotes, SeqOps[S]): Expr[Unit] =
    self.surround(start, sep, end).exprMkStringTo(builder)

  @targetName("stringExpr_exprMkString")
  def exprMkString(using Quotes, SeqOps[S]): Expr[String] =
    self.mergeConstStrings.to[List] match {
      case StringExpr.Str(single) :: Nil                 => single
      case StringExpr.Str(a) :: StringExpr.Str(b) :: Nil => '{ $a + $b }
      case Nil                                           => Expr("")
      case many                                          =>
        '{
          val builder = mutable.StringBuilder()
          ${
            Expr.block(
              many.map {
                case StringExpr.Str(expr)        => '{ builder.append($expr) }
                case StringExpr.StrBuilder(expr) => expr('builder)
              },
              '{ builder.result() },
            )
          }
        }
    }

  @targetName("stringExpr_exprMkString")
  def exprMkString(sep: StringExpr)(using Quotes, SeqOps[S]): Expr[String] =
    self.intersperse(sep).exprMkString

  @targetName("stringExpr_exprMkString")
  def exprMkString(start: StringExpr, sep: StringExpr, end: StringExpr)(using Quotes, SeqOps[S]): Expr[String] =
    self.surround(start, sep, end).exprMkString

}

extension [S[_]](self: S[Expr[String]]) {

  @targetName("exprString_exprMkStringTo")
  def exprMkStringTo(builder: Expr[mutable.StringBuilder])(using Quotes, SeqOps[S]): Expr[Unit] =
    self.map(StringExpr.Str(_)).exprMkStringTo(builder)

  @targetName("exprString_exprMkStringTo")
  def exprMkStringTo(builder: Expr[mutable.StringBuilder], sep: Expr[String])(using Quotes, SeqOps[S]): Expr[Unit] =
    self.intersperse(sep).exprMkStringTo(builder)

  @targetName("exprString_exprMkStringTo")
  def exprMkStringTo(builder: Expr[mutable.StringBuilder], start: Expr[String], sep: Expr[String], end: Expr[String])(using Quotes, SeqOps[S]): Expr[Unit] =
    self.surround(start, sep, end).exprMkStringTo(builder)

  @targetName("exprString_exprMkString")
  def exprMkString(using Quotes, SeqOps[S]): Expr[String] =
    self.map(StringExpr.Str(_)).exprMkString

  @targetName("exprString_exprMkString")
  def exprMkString(sep: Expr[String])(using Quotes, SeqOps[S]): Expr[String] =
    self.intersperse(sep).exprMkString

  @targetName("exprString_exprMkString")
  def exprMkString(start: Expr[String], sep: Expr[String], end: Expr[String])(using Quotes, SeqOps[S]): Expr[String] =
    self.surround(start, sep, end).exprMkString

}
