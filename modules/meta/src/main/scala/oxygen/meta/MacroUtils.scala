package oxygen.meta

import oxygen.predef.core.*
import scala.quoted.*

object MacroUtils {

  inline def showTree[A](inline a: A): A = ${ showTreeImpl[A]('a) }
  private def showTreeImpl[A: Type](expr: Expr[A])(using quotes: Quotes): Expr[A] = {
    val meta: Meta[quotes.type] = Meta(quotes)
    import meta.*

    report.info(
      s"""--- expr ---
         |${expr.show}
         |
         |
         |--- term ---
         |${expr.toTerm.underlying.raw}
         |
         |
         |--- term : indented ---
         |${IndentedString.fromAny(expr.toTerm.underlying.raw)}
         |""".stripMargin,
    )

    expr
  }

}
