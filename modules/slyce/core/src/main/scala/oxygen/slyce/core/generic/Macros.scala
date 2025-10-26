package oxygen.slyce.core.generic

import oxygen.quoted.*
import oxygen.slyce.core.*
import scala.quoted.*

// FIX-PRE-MERGE (KR) : rename,split,move
object Macros {

  private def showStuffImpl[A: Type](using Quotes): Expr[Unit] = {
    val aRepr: TypeRepr = TypeRepr.of[A]
    val cache: ElementReprCache = new ElementReprCache
    cache.getAny(aRepr)

    report.info(cache.allReprs.toSeq.sortBy(_.value.typeRepr.showCode).mkString("\n\n"))

    '{ () }
  }

  // FIX-PRE-MERGE (KR) :
  inline def showStuff[A]: Unit = ${ showStuffImpl[A] }

}
