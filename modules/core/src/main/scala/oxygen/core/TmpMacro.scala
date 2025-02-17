package oxygen.core

import scala.quoted.*

object TmpMacro {

  inline def showType[A]: Unit = ${ showTypeImpl[A] }
  private def showTypeImpl[A: Type](using quotes: Quotes): Expr[Unit] = {
    import quotes.reflect.*

    val typeRepr = TypeRepr.of[A]
    val typeSym = typeRepr.typeSymbol
    val classSym = typeRepr.classSymbol.get

    report.info {
      typeSym.tree.show + "\n\n" +
        classSym.tree.show + "\n\n" +
        classSym.primaryConstructor.paramSymss.flatten
          .filter(_.isTerm)
          .map { f =>
            s"${f.name} (${f.annotations.size}):${f.annotations.map { a => s"\n  - ${a.show}" }.mkString}"
          }
          .mkString("\n")
    }

    '{ () }
  }

}
