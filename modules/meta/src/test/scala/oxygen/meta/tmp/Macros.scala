package oxygen.meta.tmp

import scala.quoted.*

object Macros {

  inline def myMacro[A]: Unit = ${ myMacroImpl[A] }

  private def myMacroImpl[A](using quotes: Quotes, tpe: Type[A]): Expr[Unit] = {
    import quotes.reflect.*

    val typeRepr = TypeRepr.of[A]
    val typeSymbol = typeRepr.typeSymbol

    val caseFieldSymbols: List[Symbol] = typeSymbol.caseFields
    val caseFieldValDefs: List[ValDef] =
      caseFieldSymbols.map {
        _.tree match {
          case valDef: ValDef => valDef
          case _              => report.errorAndAbort("???")
        }
      }

    println(caseFieldValDefs)

    '{ () }
  }

}
