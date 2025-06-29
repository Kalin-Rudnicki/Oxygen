package oxygen.quoted

import scala.quoted.*

object Util {

  private def showExprImpl[A: Type](expr: Expr[A])(using Quotes): Expr[A] = {
    val tpeFromType: TypeRepr = TypeRepr.of[A]
    val tpeFromTerm: TypeRepr = expr.toTerm.tpe
    report.info(
      s"""=====| TypeRepr.of[A] |=====
         |                   type: ${tpeFromType.showAnsiCode}
         |             type.widen: ${tpeFromType.widen.showAnsiCode}
         |           type.dealias: ${tpeFromType.dealias.showAnsiCode}
         |type.dealiasKeepOpaques: ${tpeFromType.dealiasKeepOpaques.showAnsiCode}
         |
         |=====| expr.toTerm.tpe |=====
         |                   type: ${tpeFromTerm.showAnsiCode}
         |             type.widen: ${tpeFromTerm.widen.showAnsiCode}
         |           type.dealias: ${tpeFromTerm.dealias.showAnsiCode}
         |type.dealiasKeepOpaques: ${tpeFromTerm.dealiasKeepOpaques.showAnsiCode}
         |
         |=====| Expr |=====
         |${expr.showAnsiCode}""".stripMargin,
      expr,
    )
    expr
  }
  inline def showExpr[A](inline a: A): A = ${ showExprImpl[A]('a) }

}
