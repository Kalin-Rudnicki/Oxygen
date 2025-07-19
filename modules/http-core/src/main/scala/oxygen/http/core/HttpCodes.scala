package oxygen.http.core

import oxygen.http.model.*
import oxygen.meta.{*, given}
import oxygen.meta.K0.*
import oxygen.predef.core.*
import scala.quoted.*

trait HttpCodes[A] {
  lazy val possibleCodes: Set[HttpCode]
  def code(value: A): HttpCode
}
object HttpCodes extends Derivable[HttpCodes] {

  final case class Const[A](code: HttpCode) extends HttpCodes[A] {
    override lazy val possibleCodes: Set[HttpCode] = Set(code)
    override def code(value: A): HttpCode = code
  }

  override protected def productDeriver[A](using Quotes, Type[HttpCodes], Type[A], ProductGeneric[A], Derivable[HttpCodes]): Derivable.ProductDeriver[HttpCodes, A] =
    new Derivable.ProductDeriver[HttpCodes, A] {

      private def codeImpl: HttpCode =
        generic.annotations.requiredOfValue[httpCode].code

      override def derive: Expr[HttpCodes[A]] =
        '{ HttpCodes.Const(${ Expr { codeImpl } }) }

    }

  override protected def sumDeriver[A](using Quotes, Type[HttpCodes], Type[A], SumGeneric[A], Derivable[HttpCodes]): Derivable.SumDeriver[HttpCodes, A] =
    Derivable.SumDeriver.withInstances[HttpCodes, A] { instances =>
      new Derivable.SumDeriver[HttpCodes, A] {

        private def possibleCodesImpl: Expr[Set[HttpCode]] = {
          val tmp1: Growable[Expr[Growable[HttpCode]]] =
            generic.mapChildren.mapExpr[Growable[HttpCode]] { [a <: A] => (_, _) ?=> (kase: generic.Case[a]) =>
              '{ Growable.many(${ kase.getExpr(instances) }.possibleCodes) }
            }
          val tmp2: Expr[Growable[Growable[HttpCode]]] =
            tmp1.seqToExpr

          '{ $tmp2.flatten.to[Set] }
        }

        private def codeImpl(value: Expr[A]): Expr[HttpCode] =
          generic.matcher.instance(value) { [a <: A] => (_, _) ?=> (kase: generic.Case[a]) =>
            kase.caseExtractor.withRHS { aValue =>
              '{ ${ kase.getExpr(instances) }.code($aValue) }
            }
          }

        override def derive: Expr[HttpCodes[A]] =
          '{
            new HttpCodes[A] {
              override lazy val possibleCodes: Set[HttpCode] = ${ possibleCodesImpl }
              override def code(value: A): HttpCode = ${ codeImpl('value) }
            }
          }

      }
    }

  override inline def derived[A]: HttpCodes[A] = ${ derivedImpl[A] }

}
