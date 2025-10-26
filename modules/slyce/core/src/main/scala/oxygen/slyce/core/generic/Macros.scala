package oxygen.slyce.core.generic

import oxygen.meta.K0.*
import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.slyce.core.*
import scala.quoted.*
import scala.reflect.ClassTag

// FIX-PRE-MERGE (KR) : rename,split,move
object Macros {

  private final class ElementReprRef[A <: ElementRepr](typeRepr: TypeRepr) {
    private var isInitialized: Boolean = false
    private var valueRef: A = null.asInstanceOf[A]

    def value: A = {
      if (!isInitialized) throw new RuntimeException(s"attempted to get uninitialized value for: ${typeRepr.showAnsiCode}")
      valueRef
    }

    def init(value: A): Unit = {
      if (isInitialized) throw new RuntimeException(s"attempted to init already initialized value for: ${typeRepr.showAnsiCode}")
      isInitialized = true
      valueRef = value
    }

    override def toString: String =
      if (isInitialized) valueRef.toString
      else s"< uninitialized : ${typeRepr.showAnsiCode} >"

  }

  private final class ElementReprCache(using Quotes) {

    private var cacheRef: Map[TypeRepr, ElementReprRef[ElementRepr]] = Map.empty

    def allReprs: Iterable[ElementReprRef[ElementRepr]] = cacheRef.values

    private val optionTycon: TypeRepr = TypeRepr.of[Option[?]].narrow[AppliedType].tycon
    private val listTycon: TypeRepr = TypeRepr.of[List[?]].narrow[AppliedType].tycon
    private val nonEmptyListTycon: TypeRepr = TypeRepr.of[NonEmptyList[?]].narrow[AppliedType].tycon

    private val tokenTypeRepr: TypeRepr = TypeRepr.of[Token]
    private val nodeTypeRepr: TypeRepr = TypeRepr.of[Node]

    private val deriveConfig: Derivable.Config = Derivable.Config(defaultUnrollStrategy = SumGeneric.UnrollStrategy.Nested)

    private object unionType {

      def unapply(typeRepr: TypeRepr): Option[NonEmptyList[TypeRepr]] = typeRepr match
        case typeRepr: OrType => NonEmptyList.unsafeFromList(typeRepr.orChildren.toList).some
        case _                => None

    }

    private def calculateTokenRepr(gen: ProductOrSumGeneric[?]): ElementRepr.TokenRepr =
      gen match {
        case gen: ProductGeneric.CaseObjectGeneric[?] =>
          report.errorAndAbort("not allowed: case object _ extends Token", gen.pos)
        case _gen: ProductGeneric[?] =>
          type A <: Token
          val gen: ProductGeneric[A] = _gen.typedAs[A]
          import gen.given

          val regexAnnot: Expr[regex] = gen.annotations.requiredOf[regex]
          val regexStr: String = regexAnnot match
            case '{ new `regex`((${ Expr(regexStr) }: String).r) }   => regexStr
            case '{ `regex`.apply((${ Expr(regexStr) }: String).r) } => regexStr
            case _                                                   => report.errorAndAbort("does not look like \"...\".r", regexAnnot)

          val builder: Expr[Token.Builder[?]] =
            Implicits.searchOption[Token.Builder[A]].getOrElse {
              if (gen.fields.isEmpty) '{ Token.Builder.noParams[A] { ${ gen.instantiate.fieldsToInstance(Nil) } } }
              else report.errorAndAbort(s"Token ${gen.typeRepr.showAnsiCode} has non-empty constructor. Because of this, a Token.Builder instance is required.", gen.pos)
            }

          ElementRepr.ProductTokenRepr(gen, regexStr, builder)
        case gen: SumGeneric[?] =>
          val children: List[ElementRepr.SumTokenRepr.Child] =
            gen.children.toList.map { kase =>
              val ref = getSpecific[ElementRepr.TokenRepr](kase.typeRepr) { calculateTokenRepr(kase.generic) }
              ElementRepr.SumTokenRepr.Child(gen)(kase, ref)
            }

          ElementRepr.SumTokenRepr(gen, children)
      }

    private def calculateNodeRepr(gen: ProductOrSumGeneric[?]): ElementRepr.NodeRepr =
      gen match {
        case gen: ProductGeneric.CaseObjectGeneric[?] =>
          report.errorAndAbort("not allowed: case object _ extends Node", gen.pos)
        case gen: ProductGeneric[?] =>
          val children: List[ElementRepr.ProductNodeRepr.Child] =
            gen.children.toList.map { field =>
              val ref = getAny(field.typeRepr)
              ElementRepr.ProductNodeRepr.Child(gen)(field, ref)
            }

          ElementRepr.ProductNodeRepr(gen, children)
        case gen: SumGeneric[?] =>
          val children: List[ElementRepr.SumNodeRepr.Child] =
            gen.children.toList.map { kase =>
              val ref = getSpecific[ElementRepr.NodeRepr](kase.typeRepr) { calculateNodeRepr(kase.generic) }
              ElementRepr.SumNodeRepr.Child(gen)(kase, ref)
            }

          ElementRepr.SumNodeRepr(gen, children)
      }

    private def calculate(typeRepr: TypeRepr): ElementRepr =
      typeRepr.dealias.widen match {
        case AppliedType(tc, arg0 :: Nil) if tc =:= optionTycon =>
          val inner: ElementReprRef[ElementRepr.TokenOrNodeRepr] = getSpecific[ElementRepr.TokenOrNodeRepr](arg0)
          ElementRepr.OptionRepr(typeRepr, inner)
        case AppliedType(tc, arg0 :: Nil) if tc =:= listTycon =>
          val inner: ElementReprRef[ElementRepr.TokenOrNodeRepr] = getSpecific[ElementRepr.TokenOrNodeRepr](arg0)
          ElementRepr.ListRepr(typeRepr, inner)
        case AppliedType(tc, arg0 :: Nil) if tc =:= nonEmptyListTycon =>
          val inner: ElementReprRef[ElementRepr.TokenOrNodeRepr] = getSpecific[ElementRepr.TokenOrNodeRepr](arg0)
          ElementRepr.NonEmptyListRepr(typeRepr, inner)
        case typeRepr @ unionType(types) =>
          val children: NonEmptyList[ElementReprRef[ElementRepr.TokenOrNodeRepr]] = types.map(getSpecific[ElementRepr.TokenOrNodeRepr](_))
          ElementRepr.UnionRepr(typeRepr, children)
        case typeRepr if typeRepr <:< tokenTypeRepr =>
          type A
          given Type[A] = typeRepr.asTypeOf
          calculateTokenRepr(Generic.of[A](deriveConfig))
        case typeRepr if typeRepr <:< nodeTypeRepr =>
          type A
          given Type[A] = typeRepr.asTypeOf
          calculateNodeRepr(Generic.of[A](deriveConfig))
        case typeRepr =>
          report.errorAndAbort(s"Type ${typeRepr.showAnsiCode} is not a Token, Node, or special case")
      }

    private def getSpecific[A <: ElementRepr](typeRepr: TypeRepr)(doCalc: => A): ElementReprRef[A] =
      cacheRef.get(typeRepr) match {
        case Some(value) =>
          value.asInstanceOf[ElementReprRef[A]]
        case None =>
          val ref: ElementReprRef[A] = ElementReprRef(typeRepr)
          cacheRef = cacheRef.updated(typeRepr, ref.asInstanceOf[ElementReprRef[ElementRepr]])
          ref.init(doCalc)
          ref
      }

    private def getSpecific[A <: ElementRepr: ClassTag](typeRepr: TypeRepr): ElementReprRef[A] = getSpecific[A](typeRepr) { calculate(typeRepr).subtypeOrThrow[A] }

    def getAny(typeRepr: TypeRepr): ElementReprRef[ElementRepr] = getSpecific[ElementRepr](typeRepr)

  }

  private sealed trait ElementRepr {

    def typeRepr: TypeRepr

    final def subtypeOrThrow[A <: ElementRepr: ClassTag as ct] =
      this match {
        case ct(self) => self
        case _        =>
          given Quotes = typeRepr.quotes
          report.errorAndAbort(s"Expected ${ct.runtimeClass.getName}, but got ${this.getClass.getName}\n$this")
      }

    def showHeader: String
    def showBody: IndentedString

    final def toIndentedString: IndentedString = IndentedString.section(showHeader)(showBody)
    override final def toString: String = toIndentedString.toString

  }
  private object ElementRepr {

    sealed trait TokenOrNodeRepr extends ElementRepr {

      val gen: ProductOrSumGeneric[?]

      override final lazy val typeRepr: TypeRepr = gen.typeRepr

    }

    sealed trait TokenRepr extends TokenOrNodeRepr

    final case class ProductTokenRepr(
        gen: ProductGeneric[?],
        regString: String, // TODO (KR) : parse regex
        builder: Expr[Token.Builder[?]],
    ) extends TokenRepr {

      override def showHeader: String = s"Token.Product(${typeRepr.showAnsiCode})"

      override def showBody: IndentedString =
        IndentedString.inline(
          s"regex: ${regString.unesc("`")}",
          s"builder: ${builder.showAnsiCode(using gen.typeRepr.quotes)}",
        )

    }

    final case class SumTokenRepr(
        gen: SumGeneric[?],
        children: List[SumTokenRepr.Child],
    ) extends TokenRepr {

      override def showHeader: String = s"Token.Sum(${typeRepr.showAnsiCode})"

      override def showBody: IndentedString =
        children.map(_.toIndentedString)

    }
    object SumTokenRepr {

      final class Child(val parentGen: SumGeneric[?])(
          val kase: parentGen.Case[?],
          val child: ElementReprRef[TokenRepr],
      ) {

        def toIndentedString: IndentedString =
          IndentedString.section(s"${kase.name}:")(child.value.toIndentedString)

      }

    }

    sealed trait NodeRepr extends TokenOrNodeRepr

    final case class ProductNodeRepr(
        gen: ProductGeneric[?],
        children: List[ProductNodeRepr.Child],
    ) extends NodeRepr {

      override def showHeader: String = s"Node.Product(${typeRepr.showAnsiCode})"

      override def showBody: IndentedString =
        children.map(_.toIndentedString)

    }
    object ProductNodeRepr {

      final class Child(val parentGen: ProductGeneric[?])(
          val field: parentGen.Field[?],
          val child: ElementReprRef[ElementRepr],
      ) {

        def toIndentedString: IndentedString =
          IndentedString.section(s"${field.name}:")(child.value.showHeader)

      }

    }

    final case class SumNodeRepr(
        gen: SumGeneric[?],
        children: List[SumNodeRepr.Child],
    ) extends NodeRepr {

      override def showHeader: String = s"Node.Sum(${typeRepr.showAnsiCode})"

      override def showBody: IndentedString =
        children.map(_.toIndentedString)

    }
    object SumNodeRepr {

      final class Child(val parentGen: SumGeneric[?])(
          val kase: parentGen.Case[?],
          val child: ElementReprRef[NodeRepr],
      ) {

        def toIndentedString: IndentedString =
          IndentedString.section(s"${kase.name}:")(child.value.toIndentedString)

      }

    }

    sealed trait SpecialRepr extends ElementRepr

    final case class OptionRepr(typeRepr: TypeRepr, inner: ElementReprRef[ElementRepr.TokenOrNodeRepr]) extends SpecialRepr {

      override def showHeader: String = s"Option(${inner.value.typeRepr.showAnsiCode})"

      override def showBody: IndentedString =
        inner.value.toIndentedString

    }

    final case class ListRepr(typeRepr: TypeRepr, inner: ElementReprRef[ElementRepr.TokenOrNodeRepr]) extends SpecialRepr {

      override def showHeader: String = s"List(${inner.value.typeRepr.showAnsiCode})"

      override def showBody: IndentedString =
        inner.value.toIndentedString

    }

    final case class NonEmptyListRepr(typeRepr: TypeRepr, inner: ElementReprRef[ElementRepr.TokenOrNodeRepr]) extends SpecialRepr {

      override def showHeader: String = s"NonEmptyList(${inner.value.typeRepr.showAnsiCode})"

      override def showBody: IndentedString =
        inner.value.toIndentedString

    }

    final case class UnionRepr(typeRepr: TypeRepr, inner: NonEmptyList[ElementReprRef[ElementRepr.TokenOrNodeRepr]]) extends SpecialRepr {

      override def showHeader: String = s"Union( ${inner.map(_.value.typeRepr.showAnsiCode).mkString(" | ")} )"

      override def showBody: IndentedString =
        inner.toList.map(_.value.toIndentedString)

    }

  }

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
