package oxygen.slyce.core.generic

// import oxygen.meta.*
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*
import scala.reflect.ClassTag

// FIX-PRE-MERGE (KR) : rename,split,move
object Macros {

  private final class ElementReprRef(typeRepr: TypeRepr) {
    private var isInitialized: Boolean = false
    private var valueRef: ElementRepr = null

    def value: ElementRepr = {
      if (!isInitialized) throw new RuntimeException(s"attempted to get uninitialized value for: ${typeRepr.showAnsiCode}")
      valueRef
    }

    def init(value: ElementRepr): Unit = {
      if (isInitialized) throw new RuntimeException(s"attempted to init already initialized value for: ${typeRepr.showAnsiCode}")
      isInitialized = true
      valueRef = value
    }

    override def toString: String =
      if (isInitialized) valueRef.toString
      else s"< uninitialized : ${typeRepr.showAnsiCode} >"

  }

  private final class ElementReprCache(using Quotes) {

    private var cacheRef: Map[TypeRepr, ElementReprRef] = Map.empty

    def allReprs: Iterable[ElementReprRef] = cacheRef.values

    private val optionTycon: TypeRepr = TypeRepr.of[Option[?]].narrow[AppliedType].tycon
    private val listTycon: TypeRepr = TypeRepr.of[List[?]].narrow[AppliedType].tycon
    private val nonEmptyListTycon: TypeRepr = TypeRepr.of[NonEmptyList[?]].narrow[AppliedType].tycon

    private def calculate(typeRepr: TypeRepr): ElementRepr =
      typeRepr match {
        case AppliedType(`optionTycon`, arg0 :: Nil) =>
          ??? // FIX-PRE-MERGE (KR) :
        case AppliedType(`listTycon`, arg0 :: Nil) =>
          ??? // FIX-PRE-MERGE (KR) :
        case AppliedType(`nonEmptyListTycon`, arg0 :: Nil) =>
          ??? // FIX-PRE-MERGE (KR) :
        case _ =>
          ??? // FIX-PRE-MERGE (KR) :
      }

    def get(typeRepr: TypeRepr): ElementReprRef =
      cacheRef.get(typeRepr) match {
        case Some(value) => value
        case None        =>
          val ref: ElementReprRef = ElementReprRef(typeRepr)
          cacheRef = cacheRef.updated(typeRepr, ref)
          ref.init(calculate(typeRepr))
          ref
      }

  }

  private sealed trait ElementRepr {

    val typeRepr: TypeRepr

    final def subtypeOrThrow[A <: ElementRepr: ClassTag as ct] =
      this match {
        case ct(self) => self
        case _        =>
          import typeRepr.givenQuotes
          report.errorAndAbort(s"Expected ${ct.runtimeClass.getName}, but got ${this.getClass.getName}\n$this")
      }

  }
  private object ElementRepr {

    sealed trait TokenOrNodeRepr extends ElementRepr

    final case class TokenRepr(
        typeRepr: TypeRepr,
    ) extends TokenOrNodeRepr

    sealed trait NodeRepr extends TokenOrNodeRepr

    final case class ProductNodeRepr(
        typeRepr: TypeRepr,
    ) extends NodeRepr

    final case class SumNodeRepr(
        typeRepr: TypeRepr,
    ) extends NodeRepr

    sealed trait SpecialRepr extends ElementRepr

    final case class OptionRepr(typeRepr: TypeRepr, inner: ElementRepr.TokenOrNodeRepr) extends SpecialRepr

    // TODO (KR) : support other tpes of sequences
    final case class ListRepr(typeRepr: TypeRepr, inner: ElementRepr.TokenOrNodeRepr) extends SpecialRepr

  }

  private def showStuffImpl[A: Type](using Quotes): Expr[Unit] = {
    val aRepr: TypeRepr = TypeRepr.of[A]
    val cache: ElementReprCache = new ElementReprCache
    cache.get(aRepr)

    report.info(cache.allReprs.toSeq.mkString("\n\n"))

    '{ () }
  }

  // FIX-PRE-MERGE (KR) :
  inline def showStuff[A]: Unit = ${ showStuffImpl[A] }

}
