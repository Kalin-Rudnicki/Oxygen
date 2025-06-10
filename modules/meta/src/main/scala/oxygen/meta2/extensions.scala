package oxygen.meta2

import oxygen.core.syntax.functor.*
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.annotation.unused
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
