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

    val all1: Seq[ElementRepr] = cache.allReprs.toSeq.map(_.value)
    val all2: Seq[ElementRepr] =
      Seq(
        all1.collect { case elem: ElementRepr.TokenRepr => elem }.sortBy(_.typeRepr.showCode),
        all1.collect { case elem: ElementRepr.NodeRepr => elem }.sortBy(_.typeRepr.showCode),
        all1.collect { case elem: ElementRepr.SingleTypeParam => elem }.sortBy(_.inner.value.typeRepr.showCode),
        all1.collect { case elem: ElementRepr.OtherSpecial => elem }.sortBy(_.typeRepr.showCode),
      ).flatten

    report.info(all2.mkString("\n\n"))

    '{ () }
  }

  // FIX-PRE-MERGE (KR) :
  inline def showStuff[A]: Unit = ${ showStuffImpl[A] }

}
