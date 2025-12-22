package oxygen.meta.k0

import oxygen.core.*
import oxygen.core.collection.*
import oxygen.core.instances.listOrd
import oxygen.core.syntax.common.*
import oxygen.core.typeclass.*
import oxygen.meta.{given, *}
import oxygen.meta.k0 as PKG
import oxygen.quoted.*
import scala.collection.immutable.ArraySeq
import scala.quoted.*

sealed trait SumGeneric[A] extends Generic.SumGeneric[A] { sumGeneric =>

  override final type ChildBound = A
  type Gen[b] <: ProductOrSumGeneric[b]
  override type SelfType[A2] <: SumGeneric[A2]
  override final type Child[B <: A] = Case[B]

  override val typeType: TypeType.Sealed
  protected val subTypeName: String

  def cases: ArraySeq[Case[? <: A]]

  override final def children: ArraySeq[AnyChild] = cases.asInstanceOf[ArraySeq[AnyChild]]

  override final def toIndentedString: IndentedString =
    IndentedString.section(s"SumGeneric.$subTypeName[${typeRepr.showCode}]")(cases.map(_.toIndentedString))

  val matcher: Matcher = new Matcher

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Inner Classes
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /////// Case ///////////////////////////////////////////////////////////////

  final case class Case[B <: A](
      idx: Int,
      generic: Gen[B],
  ) extends Entity.Child.Deferred[A, B, A](generic) {

    override type SelfType[A2 <: A] = Case[A2]

    override val childType: String = "case"

    override def parentGeneric: SumGeneric[A] = sumGeneric

    def caseExtractor: CaseExtractor[B, Expr[B]] =
      caseExtractor(s => s"case_$s")

    def caseExtractor(bindName: String => String): CaseExtractor[B, Expr[B]] =
      caseExtractor(bindName(name))

    def caseExtractor(bindName: String): CaseExtractor[B, Expr[B]] =
      generic match {
        case caseObject: ProductGeneric.CaseObjectGeneric[B] =>
          CaseExtractor.const[B](caseObject.instantiate.instance).map { _ => caseObject.instantiate.instance }
        case generic: Generic[B] =>
          CaseExtractor.extract[B](bindName)
      }

  }

  /////// Matcher ///////////////////////////////////////////////////////////////

  class Matcher {

    // TODO (KR) : support a generic combinator that would look something like:
    //           : (matcher.instance ++ matcher.instance ++ matcher.value[String]).make[Out] { _ => ??? }

    def make[In[_ <: A], Out: Type](
        f: ChildFunction0[[b <: A] =>> MatchBuilder[In[b], Out]],
    )(using Quotes): MatchBuilder[In[A], Out] = {
      val widened: ChildFunction0[PKG.Const[MatchBuilder[In[A], Out]]] = f.asInstanceOf[ChildFunction0[PKG.Const[MatchBuilder[In[A], Out]]]]
      MatchBuilder.merge(mapChildren.map(widened))
    }

    def value[In: Type, Out: Type](expr: Expr[In])(
        f: ChildFunction0[PKG.Const[MatchBuilder[In, Out]]],
    )(
        elseCase: Quotes ?=> Expr[Out],
    )(using Quotes): Expr[Out] =
      make[PKG.Const[In], Out](f).withWildcard(elseCase).matchOn(expr)

    def instance[Out: Type](expr: Expr[A])(
        f: ChildFunction0[[b <: A] =>> MatchBuilder[b, Out]],
    )(using Quotes): Expr[Out] =
      make[PKG.Id, Out](f).matchOn(expr)

    def instance2[Out: Type](expr: Expr[(A, A)])(
        f: ChildFunction0[[b <: A] =>> MatchBuilder[(b, b), Out]],
    )(
        elseCase: Quotes ?=> Expr[Out],
    )(using Quotes): Expr[Out] =
      make[[b] =>> (b, b), Out](f).withWildcard(elseCase).matchOn(expr)

    def instance2[Out: Type](expr1: Expr[A], expr2: Expr[A])(
        f: ChildFunction0[[b <: A] =>> MatchBuilder[(b, b), Out]],
    )(
        elseCase: Quotes ?=> Expr[Out],
    )(using Quotes): Expr[Out] =
      instance2('{ ($expr1, $expr2) })(f)(elseCase)

    def instance3[Out: Type](expr: Expr[(A, A, A)])(
        f: ChildFunction0[[b <: A] =>> MatchBuilder[(b, b, b), Out]],
    )(
        elseCase: Quotes ?=> Expr[Out],
    )(using Quotes): Expr[Out] =
      make[[b] =>> (b, b, b), Out](f).withWildcard(elseCase).matchOn(expr)

    def instance3[Out: Type](expr1: Expr[A], expr2: Expr[A], expr3: Expr[A])(
        f: ChildFunction0[[b <: A] =>> MatchBuilder[(b, b, b), Out]],
    )(
        elseCase: Quotes ?=> Expr[Out],
    )(using Quotes): Expr[Out] =
      instance3('{ ($expr1, $expr2, $expr3) })(f)(elseCase)

  }

}
object SumGeneric {

  def apply[A: SumGeneric as g]: SumGeneric[A] = g

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Summon Instance
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private[k0] def unsafeOf[A](
      _typeRepr: TypeRepr,
      _typeSym: Symbol,
      config: Derivable.Config,
  )(using Quotes): SumGeneric[A] = {

    def childGenericsRec(
        isRoot: Boolean,
        sym: Symbol,
        inheritedUnroll: SumGeneric.UnrollStrategy,
    ): Growable[ProductOrSumGeneric[? <: A]] =
      sym.typeType.option match {
        case Some(_: TypeType.Case.Class) =>
          val classDef: ClassDef = sym.tree.narrow[ClassDef]
          if classDef.constructor.paramss.exists { case _: TypeParamClause => true; case _ => false } then
            report.errorAndAbort("Type params on sum types is not yet supported")

          type B <: A
          Growable.single(ProductGeneric.unsafeOf[B](sym.typeRef, sym, config))

        case Some(TypeType.CaseObject) =>
          type B <: A
          Growable.single(ProductGeneric.unsafeOf[B](sym.termRef, sym, config))

        case Some(TypeType.EnumCaseObject) =>
          type B <: A
          Growable.single(ProductGeneric.unsafeOf[B](Singleton.companion.apply(sym.termRef.toTerm).tpe, sym, config))

        case Some(TypeType.Scala2CaseObject) =>
          // TODO (KR) :
          report.errorAndAbort("TODO : support scala-2 case object")

        case Some(_: TypeType.Sealed) =>
          val classDef: ClassDef = sym.tree.narrow[ClassDef]
          if classDef.constructor.paramss.exists { case _: TypeParamClause => true; case _ => false } then
            report.errorAndAbort("Type params on sum types is not yet supported")

          type B <: A
          val typeRepr: TypeRepr = sym.typeRef
          given Type[B] = typeRepr.asTypeOf

          val myUnrollStrategy: SumGeneric.UnrollStrategy = SumGeneric.UnrollStrategy.calculate[B](inheritedUnroll, config.overrideUnrollStrategyBehavior)

          val shouldUnroll: Boolean =
            if isRoot then true
            else
              myUnrollStrategy match
                case SumGeneric.UnrollStrategy.Unroll => true
                case SumGeneric.UnrollStrategy.Nested => false

          if shouldUnroll then {
            val children = sym.children
            if children.isEmpty then report.errorAndAbort(s"Sum type ${sym.name} has no children", sym.pos)

            Growable.many(children).flatMap(childGenericsRec(false, _, myUnrollStrategy))
          } else
            Growable.single(Generic.of[B](config))

        case None => report.errorAndAbort(s"Type ${sym.name} is not a product or sum type", sym.pos)
      }

    val childGenerics: ArraySeq[ProductOrSumGeneric[? <: A]] =
      childGenericsRec(true, _typeSym, config.defaultUnrollStrategy).toArraySeq.sorted(using config.defaultOrdinalStrategy.ord)

    val filteredGenerics: Either[ArraySeq[ProductOrSumGeneric[? <: A]], Either[ArraySeq[ProductGeneric[? <: A]], ArraySeq[ProductGeneric.CaseObjectGeneric[? <: A]]]] =
      childGenerics
        .traverse {
          case g: ProductGeneric[? <: A] => g.some
          case _                         => None
        }
        .toRight(childGenerics)
        .map { childGenerics =>
          childGenerics
            .traverse {
              case g: ProductGeneric.CaseObjectGeneric[? <: A] => g.some
              case _                                           => None
            }
            .toRight(childGenerics)
        }

    filteredGenerics match {
      case Right(Right(enums)) =>
        new SumGeneric.EnumGeneric[A] {
          override val label: String = _typeSym.name
          override val sym: Symbol = _typeSym
          override val typeRepr: TypeRepr = _typeRepr
          override val typeType: TypeType.Sealed = _typeSym.typeType.sum.required(using typeRepr.quotes)
          override val derivedFromConfig: Derivable.Config = config
          override val cases: ArraySeq[Case[? <: A]] = enums.toArraySeq.zipWithIndex.map { case (g, i) => Case(i, g) }
        }
      case Right(Left(products)) =>
        new SumGeneric.FlatNonEnumGeneric[A] {
          override val label: String = _typeSym.name
          override val sym: Symbol = _typeSym
          override val typeRepr: TypeRepr = _typeRepr
          override val typeType: TypeType.Sealed = _typeSym.typeType.sum.required(using typeRepr.quotes)
          override val derivedFromConfig: Derivable.Config = config
          override val cases: ArraySeq[Case[? <: A]] = products.toArraySeq.zipWithIndex.map { case (g, i) => Case(i, g) }
        }
      case Left(generics) =>
        new SumGeneric.NestedGeneric[A] {
          override val label: String = _typeSym.name
          override val sym: Symbol = _typeSym
          override val typeRepr: TypeRepr = _typeRepr
          override val typeType: TypeType.Sealed = _typeSym.typeType.sum.required(using typeRepr.quotes)
          override val derivedFromConfig: Derivable.Config = config
          override val cases: ArraySeq[Case[? <: A]] = generics.toArraySeq.zipWithIndex.map { case (g, i) => Case(i, g) }
        }
    }
  }

  def of[A](using Type[A], Quotes): SumGeneric[A] = SumGeneric.of[A](Derivable.Config())
  def of[A](config: Derivable.Config)(using Type[A], Quotes): SumGeneric[A] =
    Generic.of[A](config) match
      case g: SumGeneric[A] => g
      case _                => report.errorAndAbort(s"Not a sum type: ${TypeRepr.of[A].show}", TypeRepr.of[A].typeOrTermSymbol.pos)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Config
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /**
    * How should ordinals be assigned?
    *
    * ```scala
    * enum MyEnum {
    *   case A
    *   case C
    *   case B
    * }
    * ```
    *
    * SourcePosition: A, C, B
    * Lexicographical: A, B, C
    */
  enum OrdinalStrategy {
    case SourcePosition, Lexicographical

    final def ord: Ordering[Generic[?]] = this match
      case OrdinalStrategy.SourcePosition  => Ordering.by { _.pos.start }
      case OrdinalStrategy.Lexicographical => Ordering.by { _.sym.fullName.split('.').reverse.toList }

  }
  object OrdinalStrategy {

    given FromExprT[OrdinalStrategy] =
      new FromExprT[OrdinalStrategy] {
        override def unapply(x: Expr[OrdinalStrategy])(using Type[OrdinalStrategy], Quotes): Option[OrdinalStrategy] = x match
          case '{ OrdinalStrategy.SourcePosition }  => OrdinalStrategy.SourcePosition.some
          case '{ OrdinalStrategy.Lexicographical } => OrdinalStrategy.Lexicographical.some
          case _                                    => None
      }

  }

  /**
    * How to handle sealed trait hierarchies.
    *
    * ```scala
    * sealed trait Root
    *
    * sealed trait Child1 extends Root
    * case object A extends Child1
    * case object B extends Child1
    *
    * sealed trait Child2 extends Root
    * case object D extends Child2
    * case object C extends Child2
    * ```
    *
    * Unroll: SumGeneric(Root)(ProductGeneric(A), ProductGeneric(B), ProductGeneric(C), ProductGeneric(D))
    * Nested: SumGeneric(Root)(SumGeneric(Child1)(ProductGeneric(A), ProductGeneric(B)), SumGeneric(Child2)(ProductGeneric(C), ProductGeneric(D)))
    */
  enum UnrollStrategy { case Unroll, Nested }
  object UnrollStrategy {

    given FromExprT[UnrollStrategy] =
      new FromExprT[UnrollStrategy] {
        override def unapply(x: Expr[UnrollStrategy])(using Type[UnrollStrategy], Quotes): Option[UnrollStrategy] = x match
          case '{ UnrollStrategy.Unroll } => UnrollStrategy.Unroll.some
          case '{ UnrollStrategy.Nested } => UnrollStrategy.Nested.some
          case _                          => None
      }

    def calculate[T: Type](default: UnrollStrategy, behavior: OverrideUnrollStrategyBehavior)(using Quotes): UnrollStrategy = {
      val typeRepr: TypeRepr = TypeRepr.of[T]
      behavior match {
        case OverrideUnrollStrategyBehavior.IgnoreSilently => default
        case OverrideUnrollStrategyBehavior.Allow          =>
          typeRepr.annotations.optionalOfValue[overrideUnrollStrategy[T]] match {
            case Some(over) => over.strategy
            case None       =>
              typeRepr.annotations.optionalOfValue[overrideAllUnrollStrategy] match {
                case Some(over) => over.strategy
                case None       => default
              }
          }
        case OverrideUnrollStrategyBehavior.IgnoreAndWarn =>
          if typeRepr.annotations.optionalOfValue[overrideUnrollStrategy[T]].nonEmpty || typeRepr.annotations.optionalOfValue[overrideAllUnrollStrategy].nonEmpty then
            report.warning(s"${typeRepr.showAnsiCode} attempted to override generic unroll strategy, but it is being ignored")

          default
      }
    }

  }

  enum OverrideUnrollStrategyBehavior { case Allow, IgnoreSilently, IgnoreAndWarn }
  object OverrideUnrollStrategyBehavior {

    given FromExprT[OverrideUnrollStrategyBehavior] =
      new FromExprT[OverrideUnrollStrategyBehavior] {
        override def unapply(x: Expr[OverrideUnrollStrategyBehavior])(using Type[OverrideUnrollStrategyBehavior], Quotes): Option[OverrideUnrollStrategyBehavior] = x match
          case '{ OverrideUnrollStrategyBehavior.Allow }          => OverrideUnrollStrategyBehavior.Allow.some
          case '{ OverrideUnrollStrategyBehavior.IgnoreSilently } => OverrideUnrollStrategyBehavior.IgnoreSilently.some
          case '{ OverrideUnrollStrategyBehavior.IgnoreAndWarn }  => OverrideUnrollStrategyBehavior.IgnoreAndWarn.some
          case _                                                  => None
      }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      FlatGeneric
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait FlatGeneric[A] private[SumGeneric] () extends SumGeneric[A] {
    override type Gen[b] <: ProductGeneric[b]
    override type SelfType[A2] <: FlatGeneric[A2]
  }
  object FlatGeneric {

    def of[A](using Type[A], Quotes): SumGeneric.FlatGeneric[A] =
      SumGeneric.of[A](Derivable.Config(defaultUnrollStrategy = SumGeneric.UnrollStrategy.Unroll)) match
        case gen: SumGeneric.FlatGeneric[A] => gen
        case gen                            => report.errorAndAbort(s"internal defect : FlatGeneric.of did not return a FlagGeneric:\n$gen")

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      FlatNonEnumGeneric
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private[SumGeneric] trait FlatNonEnumGeneric[A] private[SumGeneric] () extends FlatGeneric[A] {
    override final type Gen[b] = ProductGeneric[b]
    override final type SelfType[A2] = FlatNonEnumGeneric[A2]
    override protected val subTypeName: String = "FlatNonEnumGeneric"
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      EnumGeneric
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait EnumGeneric[A] private[SumGeneric] () extends FlatGeneric[A] {
    override final type Gen[b] = ProductGeneric.CaseObjectGeneric[b]
    override final type SelfType[A2] = EnumGeneric[A2]
    override protected val subTypeName: String = "EnumGeneric"
  }
  object EnumGeneric {

    def of[A](using Type[A], Quotes): SumGeneric.EnumGeneric[A] =
      SumGeneric.of[A](Derivable.Config(defaultUnrollStrategy = SumGeneric.UnrollStrategy.Unroll)) match
        case gen: SumGeneric.EnumGeneric[A] => gen
        case gen                            => report.errorAndAbort(s"internal defect : EnumGeneric.of did not return an EnumGeneric:\n$gen")

    private def extractSum[A: Type](using Quotes): ArraySeq[ProductGeneric[? <: A]] = {
      def unroll(g: ProductOrSumGeneric[? <: A]): ArraySeq[ProductGeneric[? <: A]] = g.productOrSumGenericSelf match
        case generic: ProductGeneric[? <: A] => ArraySeq(generic)
        case generic: FlatGeneric[? <: A]    => generic.children.map(_.generic).toArraySeq
        case generic: NestedGeneric[? <: A]  => generic.children.toArraySeq.flatMap(g => unroll(g.generic))

      unroll(Generic.of[A])
    }

    private def extractReturnCaseClasses[A: Type](using Quotes): (TypeRepr, ArraySeq[ProductGeneric.CaseClassGeneric[? <: A]], Expr[ArraySeq[A]]) = {
      val typeRepr: TypeRepr = TypeRepr.of[A].dealias
      val children: ArraySeq[ProductGeneric[? <: A]] =
        typeRepr match {
          case or: OrType =>
            ArraySeq.from(or.orChildren).flatMap { t =>
              type B <: A
              given Type[B] = t.asTypeOf
              extractSum[B]
            }
          case _ => extractSum[A]
        }

      val (caseObjects, caseClasses) = children.partitionMap {
        case caseObject: ProductGeneric.CaseObjectGeneric[? <: A] => caseObject.asLeft
        case caseClass: ProductGeneric.CaseClassGeneric[? <: A]   => caseClass.asRight
      }

      if caseObjects.isEmpty then report.errorAndAbort(s"No case objects returned for parent ${typeRepr.showAnsiCode}")

      val values: ArraySeq[Expr[A]] = caseObjects.map { _.instantiate.instance }

      (typeRepr, caseClasses, values.seqToArraySeqExpr)
    }

    private def extract[A: Type](validateNumCaseClasses: Int => Boolean, expectedSize: String)(using Quotes): Expr[ArraySeq[A]] = {
      val (typeRepr, caseClasses, valuesExpr) = extractReturnCaseClasses[A]

      if !validateNumCaseClasses(caseClasses.length) then {
        val showCaseClasses = caseClasses.map { ccg => s"\n  - ${ccg.typeRepr.showAnsiCode}" }.mkString
        report.errorAndAbort(s"Invalid number of case classes detected for parent ${typeRepr.showAnsiCode}, expected $expectedSize, but found ${caseClasses.length}:$showCaseClasses")
      }

      valuesExpr
    }

    // TODO (KR) : move to top level?
    object deriveEnum {

      /**
        * Expects a strict enum, all case object children.
        */
      object strictEnum {
        private def valuesImpl[A: Type](using Quotes): Expr[ArraySeq[A]] = extract[A](_ == 0, "exactly 0")
        inline def values[A]: ArraySeq[A] = ${ valuesImpl[A] }
        inline def map[A, B](f: A => B): Map[B, A] = values[A].map { a => (f(a), a) }.toMap
      }

      /**
        * Expects all case object children, except exactly 1 case class.
        * Useful for an enum with a representation for Other(_).
        */
      object ignoreSingleCaseClass {

        private def valuesImpl[A: Type](using Quotes): Expr[ArraySeq[A]] = extract[A](_ == 1, "exactly 1")
        inline def values[A]: ArraySeq[A] = ${ valuesImpl[A] }
        inline def map[A, B](f: A => B): Map[B, A] = values[A].map { a => (f(a), a) }.toMap

        private def valuesAndWrapImpl[A: Type, B: Type](using Quotes): Expr[(ArraySeq[A], B => A)] = {
          val (typeRepr, caseClasses, valuesExpr) = extractReturnCaseClasses[A]
          val caseClass: ProductGeneric.CaseClassGeneric[? <: A] =
            caseClasses.toList match {
              case caseClass :: Nil => caseClass
              case _                =>
                val showCaseClasses = caseClasses.map { ccg => s"\n  - ${ccg.typeRepr.showAnsiCode}" }.mkString
                report.errorAndAbort(s"Invalid number of case classes detected for parent ${typeRepr.showAnsiCode}, expected 1, but found ${caseClasses.length}:$showCaseClasses")
            }

          val bTypeRepr: TypeRepr = TypeRepr.of[B].dealias
          val caseClassField: caseClass.Field[?] = caseClass.fields.toList match
            case field :: Nil => field
            case _            => report.errorAndAbort(s"Found single case-class child for ${typeRepr.showAnsiCode}, but it doesn't have a single field:\n$caseClass")

          if !(caseClassField.typeRepr =:= bTypeRepr) then
            report.errorAndAbort(s"Expected single case-class field to have type ${bTypeRepr.showAnsiCode}, but got ${caseClassField.typeRepr.showAnsiCode}", caseClassField.pos)

          def wrap(value: Expr[B]): Expr[A] = caseClass.instantiate.fieldsToInstance(value :: Nil)

          val wrapExpr: Expr[B => A] =
            '{ (value: B) => ${ wrap('value) } }

          '{ ($valuesExpr, $wrapExpr) }
        }

        /**
          * Expects a type with case-object children, and a single case-class child, which has a single field of type [[B]].
          * The returned function `[[B]] => [[A]]` will create an [[A]] by wrapping it in that case-class.
          */
        inline def valuesAndWrap[A, B]: (ArraySeq[A], B => A) = ${ valuesAndWrapImpl[A, B] }

      }

      /**
        * Expects case object children, with more than 1 case class.
        */
      object ignoreManyCaseClasses {
        private def valuesImpl[A: Type](using Quotes): Expr[ArraySeq[A]] = extract[A](_ > 1, "more than 1")
        inline def values[A]: ArraySeq[A] = ${ valuesImpl[A] }
        inline def map[A, B](f: A => B): Map[B, A] = values[A].map { a => (f(a), a) }.toMap
      }

      /**
        * Expects case object children, and does not care about the number of case class children.
        */
      object lax {
        private def valuesImpl[A: Type](using Quotes): Expr[ArraySeq[A]] = extract[A](_ => true, "???")
        inline def values[A]: ArraySeq[A] = ${ valuesImpl[A] }
        inline def map[A, B](f: A => B): Map[B, A] = values[A].map { a => (f(a), a) }.toMap
      }

    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      NestedGeneric
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait NestedGeneric[A] private[SumGeneric] () extends SumGeneric[A] {
    override final type Gen[b] = ProductOrSumGeneric[b]
    override final type SelfType[A2] = NestedGeneric[A2]
    override protected val subTypeName: String = "NestedGeneric"
  }
  object NestedGeneric {

    def of[A](using Type[A], Quotes): SumGeneric.NestedGeneric[A] =
      SumGeneric.of[A](Derivable.Config(defaultUnrollStrategy = SumGeneric.UnrollStrategy.Nested)) match
        case gen: SumGeneric.NestedGeneric[A] => gen
        case gen                              => report.errorAndAbort(s"internal defect : NestedGeneric.of did not return a NestedGeneric:\n$gen")

  }

}
