package oxygen.meta

import oxygen.predef.core.*
import scala.quoted.*

object MacroUtils {

  def verboseTreeToString(expr: Expr[Any])(using quotes: Quotes): String = {
    val meta: Meta[quotes.type] = Meta(quotes)
    import meta.*
    import quotes.reflect.Printer

    s"""--- expr ---
     |${expr.toTerm.show(using Printer.TreeAnsiCode)}
     |
     |
     |--- term : tree structure ---
     |${expr.toTerm.show(using Printer.TreeStructure)}
     |
     |
     |--- term : pretty tree structure ---
     |${IndentedString.fromAny(expr.toTerm.underlying.raw)}
     |""".stripMargin
  }

  inline def showTree[A](inline a: A): A = ${ showTreeImpl[A]('a) }
  def showTreeImpl[A: Type](expr: Expr[A])(using quotes: Quotes): Expr[A] = {
    val meta: Meta[quotes.type] = Meta(quotes)
    import meta.*

    report.info(verboseTreeToString(expr))

    expr
  }

}
