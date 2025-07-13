package oxygen.quoted

import scala.annotation.targetName
import scala.quoted.*

object MacroUtil {

  def macroShowExpr[A: Type](expr: Expr[A], pos: Position)(using Quotes): Expr[A] = {
    val tpeFromType: TypeRepr = TypeRepr.of[A]
    val tpeFromTerm: TypeRepr = expr.toTerm.tpe
    report.info(
      s"""
         |=====| TypeRepr.of[A] |=====
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
         |${expr.showAnsiCode}
         |""".stripMargin,
      pos,
    )
    expr
  }
  def macroShowExpr[A: Type](expr: Expr[A], pos: Term)(using Quotes): Expr[A] =
    macroShowExpr(expr, pos.pos)
  def macroShowExpr[A: Type](expr: Expr[A], pos: Expr[?])(using Quotes): Expr[A] =
    macroShowExpr(expr, pos.toTerm.pos)
  def macroShowExpr[A: Type](expr: Expr[A])(using Quotes): Expr[A] =
    macroShowExpr(expr, expr)

  @targetName("macroShowExprWhen_pos")
  def macroShowExprWhen[A: Type](expr: Expr[A], pos: Option[Position])(using Quotes): Expr[A] = pos match
    case Some(pos) => macroShowExpr(expr, pos)
    case None      => expr
  @targetName("macroShowExprWhen_term")
  def macroShowExprWhen[A: Type](expr: Expr[A], pos: Option[Term])(using Quotes): Expr[A] =
    macroShowExprWhen(expr, pos.map(_.pos))
  @targetName("macroShowExprWhen_expr")
  def macroShowExprWhen[A: Type](expr: Expr[A], pos: Option[Expr[?]])(using Quotes): Expr[A] =
    macroShowExprWhen(expr, pos.map(_.toTerm.pos))
  @targetName("macroShowExprWhen_cond")
  def macroShowExprWhen[A: Type](expr: Expr[A], cond: Boolean)(using Quotes): Expr[A] =
    macroShowExprWhen(expr, Option.when(cond)(expr))

  inline def showExpr[A](inline a: A): A = ${ macroShowExpr[A]('a) }

}
