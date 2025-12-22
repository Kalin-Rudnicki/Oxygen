package oxygen.meta

import scala.collection.mutable
import scala.quoted.*

enum StringExpr {
  case Str(expr: Expr[String])
  case StrBuilder(expr: Expr[mutable.StringBuilder] => Expr[Unit])

  def show(using Quotes): String = this match
    case StringExpr.Str(expr)        => expr.show
    case StringExpr.StrBuilder(expr) => expr('{ ??? : mutable.StringBuilder }).show

}
object StringExpr {
  def const(str: String)(using Quotes): StringExpr = Str(Expr(str))
}
