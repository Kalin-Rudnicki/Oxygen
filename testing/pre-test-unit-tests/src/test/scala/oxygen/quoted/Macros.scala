package oxygen.quoted

import scala.quoted.*

object Macros {

  private def getConstructorParamsImpl[A: Type](using Quotes): Expr[Set[String]] = {
    val repr = TypeRepr.of[A]
    val pc = repr.typeSymbol.primaryConstructor
    val res = pc.paramSymss.flatten.map(_.name)
    report.info(s"${repr.show} : ${res.mkString(", ")}")
    Expr(res.toSet)
  }

  inline def getConstructorParams[A]: Set[String] = ${ getConstructorParamsImpl[A] }

}
