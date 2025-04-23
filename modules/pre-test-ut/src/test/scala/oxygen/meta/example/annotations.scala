package oxygen.meta.example

import oxygen.predef.core.*
import scala.quoted.*

final case class fieldName(name: String) extends scala.annotation.Annotation
object fieldName {

  given FromExpr[fieldName] =
    new FromExpr[fieldName] {
      override def unapply(x: Expr[fieldName])(using Quotes): Option[fieldName] =
        x match {
          case '{ fieldName(${ Expr(name) }) } => fieldName(name).some
          case _                               => None
        }
    }

}

final case class caseName(name: String) extends scala.annotation.Annotation
object caseName {

  given FromExpr[caseName] =
    new FromExpr[caseName] {
      override def unapply(x: Expr[caseName])(using Quotes): Option[caseName] =
        x match {
          case '{ caseName(${ Expr(name) }) } => caseName(name).some
          // case '{ new caseName(${ Expr(name) }) } => caseName(name).some
          case _ => None
        }
    }

}
