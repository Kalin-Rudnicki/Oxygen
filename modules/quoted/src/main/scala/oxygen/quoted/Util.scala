package oxygen.quoted

import scala.quoted.*

object Util {

  private def showExprImpl[A: Type](a: Expr[A])(using Quotes): Expr[A] = {
    report.info(a.showAnsiCode)
    a
  }

  inline def showExpr[A](inline a: A): A = ${ showExprImpl[A]('a) }

}
