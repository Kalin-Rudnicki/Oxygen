package oxygen.meta.k0

import oxygen.core.*
import oxygen.quoted.*
import scala.quoted.*

/**
  * Allows you to interact with a sequence of types that might be [0, 1, or 2+] [Unit, A, Tuple].
  */
sealed trait UnderlyingConverter[A] {

  val aTypeRepr: TypeRepr
  val aType: Type[A]

  final def castTermOrFail(term: Term)(using Quotes): Expr[A] =
    castExprOrFail(term.asExpr)
  final def castExprOrFail(expr: Expr[Any])(using Quotes): Expr[A] =
    if expr.isExprOf[A](using aType) then expr.asExprOf[A](using aType)
    else {
      val term = expr.toTerm
      report.errorAndAbort(
        s"""Term is not of the correct type to be cast.
            |Expected type: ${aTypeRepr.showAnsiCode}
            |  Actual type: ${term.tpe.widen.showAnsiCode}
            |         Term:
            |
            |${term.showAnsiCode}
            |""".stripMargin,
      )
    }

  def splitExpr(expr: Expr[A])(using Quotes): List[Term]
  final def splitTerm(term: Term)(using Quotes): List[Term] = splitExpr(castTermOrFail(term))
  final def splitAnyExpr(expr: Expr[Any])(using Quotes): List[Term] = splitExpr(castExprOrFail(expr))

  def joinExpr(terms: List[Term])(using Quotes): Expr[A]
  final def joinTerm(terms: List[Term])(using Quotes): Term = joinExpr(terms).toTerm

}
object UnderlyingConverter {

  final class Empty[A](unitTypeRepr: TypeRepr) extends UnderlyingConverter[A] {

    override val aTypeRepr: TypeRepr = unitTypeRepr
    override val aType: Type[A] = aTypeRepr.asTypeOf

    override def splitExpr(expr: Expr[A])(using Quotes): List[Term] = Nil

    override def joinExpr(terms: List[Term])(using Quotes): Expr[A] = terms match
      case Nil => castExprOrFail { '{ () } }
      case _   => report.errorAndAbort(s"Empty expected empty term input, but got (${terms.size}):${terms.map { t => s"\n  - ${t.showAnsiCode}" }.mkString}")

  }

  final case class Single[A](aTypeRepr: TypeRepr) extends UnderlyingConverter[A] {

    override val aType: Type[A] = aTypeRepr.asTypeOf

    override def splitExpr(expr: Expr[A])(using Quotes): List[Term] = expr.toTerm :: Nil

    override def joinExpr(terms: List[Term])(using Quotes): Expr[A] = terms match
      case term :: Nil => castTermOrFail { term }
      case _           => report.errorAndAbort(s"Single expected single term input, but got (${terms.size}):${terms.map { t => s"\n  - ${t.showAnsiCode}" }.mkString}")

  }

  final case class Tupled[A](generic: ProductGeneric[A]) extends UnderlyingConverter[A] {

    override val aTypeRepr: TypeRepr = generic.typeRepr
    override val aType: Type[A] = generic.tpe

    override def splitExpr(expr: Expr[A])(using Quotes): List[Term] = generic.fields.toList.map { f => f.fromParent(expr).toTerm }

    override def joinExpr(terms: List[Term])(using Quotes): Expr[A] = generic.fieldsToInstance(terms.map(_.asExpr))

  }

  def of[A: Type as tpe](using Quotes): UnderlyingConverter[A] =
    tpe match {
      case '[Unit] => new Empty[A](TypeRepr.of[Unit])
      case _       =>
        val tr = tpe.toTypeRepr
        TypeRepr.extractTuple(tpe.toTypeRepr) match {
          case Some(tupleArgs) => new Tupled[A](ProductGeneric.ofTuple(tupleArgs))
          case None            => new Single[A](tr)
        }
    }

  def of[A](types: List[TypeRepr])(using Quotes): UnderlyingConverter[A] = types match
    case Nil        => new Empty[A](TypeRepr.of[Unit])
    case tpe :: Nil => new Single[A](tpe)
    case tupleArgs  => new Tupled[A](ProductGeneric.ofTuple(tupleArgs))

}
