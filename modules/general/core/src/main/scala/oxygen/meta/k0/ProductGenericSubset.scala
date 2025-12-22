package oxygen.meta.k0

import oxygen.meta.k0 as PKG
import oxygen.quoted.*
import scala.collection.immutable.ArraySeq
import scala.quoted.*

sealed trait ProductGenericSubset[A, B] { self =>

  final def typedAs[B2]: ProductGenericSubset[A, B2] = this.asInstanceOf[ProductGenericSubset[A, B2]]

  val subsetType: String
  def aGeneric: ProductGeneric[A]
  def bGeneric: Generic[B]

  final given aTpe: Type[A] = aGeneric.tpe
  final given bTpe: Type[B] = bGeneric.tpe

  def convert(a: Expr[A])(using Quotes): Expr[B]
  final def convertExpr(using Quotes): Expr[A => B] = '{ (a: A) => ${ convert('a) } }

  def convertExpressions[F[_]](expressions: Expressions[F, A])(using Quotes): Expressions[F, B]

  private def toSpecific[S <: ProductGenericSubset[A, ?]](filterType: String)(f: PartialFunction[ProductGenericSubset[A, B], S])(using Quotes): S =
    f.applyOrElse(
      this,
      _ => report.errorAndAbort(s"Unable to filter $this to SubsetGeneric.$filterType"),
    )

  final def toEmpty(using Quotes): ProductGenericSubset.Empty[A] = toSpecific("Empty") { case s: ProductGenericSubset.Empty[A @unchecked] => s }
  final def toNonEmpty(using Quotes): ProductGenericSubset.NonEmpty[A, B] = toSpecific("NonEmpty") { case s: ProductGenericSubset.NonEmpty[A, B] => s }
  final def toSingle(using Quotes): ProductGenericSubset.Single[A, B] = toSpecific("Single") { case s: ProductGenericSubset.Single[A, B] => s }
  final def toMany(using Quotes): ProductGenericSubset.Many[A, B] = toSpecific("Many") { case s: ProductGenericSubset.Many[A, B] => s }

  object subInstance {

    /**
      * This only works if the [[Derivable.ProductDeriver]] in your [[Derivable]] uses [[Derivable.ProductDeriver.withInstances]].
      */
    final def fromDerivable[F[_]: Type as fTpe](
        derivable: Derivable[F],
        aInstances: Expressions[F, A],
    )(using quotes: Quotes): Expr[F[B]] = {
      def deriveWithInstances(generic: ProductGeneric[B]): Expr[F[B]] =
        derivable.deriveInternal
          .productDeriver[B](using quotes, fTpe, bTpe, generic, derivable)
          .deriveWithInstances(convertExpressions(aInstances))

      self match
        case subset: ProductGenericSubset.Empty[A @unchecked] => deriveWithInstances(subset.bGenericTyped[B])
        case subset: ProductGenericSubset.Single[A, B]        => subset.aField.getExpr(aInstances)
        case subset: ProductGenericSubset.Many[A, B]          => deriveWithInstances(subset.bGeneric)
    }

    /**
      * This only works if the [[Derivable.ProductDeriver]] in your [[Derivable]] uses [[Derivable.ProductDeriver.withInstances]].
      */
    final def fromDerivable[F[_]: Type as fTpe](
        derivable: Derivable[F],
        aInstances: Expressions[F, A],
        emptyInstance: => Expr[F[Unit]],
    )(using quotes: Quotes): Expr[F[B]] = {
      def deriveWithInstances(generic: ProductGeneric[B]): Expr[F[B]] =
        derivable.deriveInternal
          .productDeriver[B](using quotes, fTpe, bTpe, generic, derivable)
          .deriveWithInstances(convertExpressions(aInstances))

      self match
        case _: ProductGenericSubset.Empty[A @unchecked] => emptyInstance.asExprOf[F[B]]
        case subset: ProductGenericSubset.Single[A, B]   => subset.aField.getExpr(aInstances)
        case subset: ProductGenericSubset.Many[A, B]     => deriveWithInstances(subset.bGeneric)
    }

    final def fromDeriver[F[_]: Type as fTpe](
        productDeriver: (Quotes, Type[F], Type[B], ProductGeneric[B]) ?=> Expressions[F, B] => Derivable.ProductDeriver[F, B],
        aInstances: Expressions[F, A],
    )(using quotes: Quotes): Expr[F[B]] = {
      def deriveWithInstances(generic: ProductGeneric[B]): Expr[F[B]] =
        productDeriver(using quotes, fTpe, bTpe, generic)(convertExpressions(aInstances)).derive

      self match
        case subset: ProductGenericSubset.Empty[A @unchecked] => deriveWithInstances(subset.bGenericTyped[B])
        case subset: ProductGenericSubset.Single[A, B]        => subset.aField.getExpr(aInstances)
        case subset: ProductGenericSubset.Many[A, B]          => deriveWithInstances(subset.bGeneric)
    }

    final def fromDeriver[F[_]: Type as fTpe](
        productDeriver: (Quotes, Type[F], Type[B], ProductGeneric[B]) ?=> Expressions[F, B] => Derivable.ProductDeriver[F, B],
        aInstances: Expressions[F, A],
        emptyInstance: => Expr[F[Unit]],
    )(using quotes: Quotes): Expr[F[B]] = {
      def deriveWithInstances(generic: ProductGeneric[B]): Expr[F[B]] =
        productDeriver(using quotes, fTpe, bTpe, generic)(convertExpressions(aInstances)).derive

      self match
        case _: ProductGenericSubset.Empty[A @unchecked] => emptyInstance.asExprOf[F[B]]
        case subset: ProductGenericSubset.Single[A, B]   => subset.aField.getExpr(aInstances)
        case subset: ProductGenericSubset.Many[A, B]     => deriveWithInstances(subset.bGeneric)
    }

  }

  override final def toString: String =
    s"ProductGenericSubset.$subsetType[${aGeneric.typeRepr.showCode}, ${bGeneric.typeRepr.showCode}]"

}
object ProductGenericSubset {

  trait Empty[A] extends ProductGenericSubset[A, Unit] {

    // do not do `override val unitTypeRepr: TypeRepr = TypeRepr.of[Unit]`. `given bTpe: Type[B = Unit]` will mess this up.
    val unitTypeRepr: TypeRepr
    override final val subsetType: String = "Empty"

    override final lazy val bGeneric: ProductGeneric.CaseObjectGeneric[Unit] = ProductGeneric.CaseObjectGeneric.unit(unitTypeRepr, aGeneric.derivedFromConfig)

    final def bGenericTyped[B]: ProductGeneric.CaseObjectGeneric[B] = bGeneric.typedAs[B]

    override final def convert(a: Expr[A])(using Quotes): Expr[Unit] = bGeneric.instantiate.instance

    override final def convertExpressions[F[_]](expressions: Expressions[F, A])(using Quotes): Expressions[F, Unit] =
      new Expressions[F, Unit](expressions.fTpe, bGeneric.tpe, ArraySeq.empty)

  }

  sealed trait NonEmpty[A, T] extends ProductGenericSubset[A, T]

  trait Single[A, B] extends ProductGenericSubset.NonEmpty[A, B] {

    override final val subsetType: String = "Single"
    override val aGeneric: ProductGeneric[A]
    val aField: aGeneric.Field[B]
    override final lazy val bGeneric: IdentityGeneric[B] =
      IdentityGeneric.Instance[B](aField.label, aField.typeRepr.typeSymbol, aField.typeRepr)

    override final def convert(a: Expr[A])(using Quotes): Expr[B] = aField.fromParent(a)

    override final def convertExpressions[F[_]](expressions: Expressions[F, A])(using Quotes): Expressions[F, B] =
      new Expressions[F, B](expressions.fTpe, bGeneric.tpe, ArraySeq(expressions.expressions(aField.idx)))

  }

  trait Many[A, B] extends ProductGenericSubset.NonEmpty[A, B] {
    override final val subsetType: String = "Many"
    override val aGeneric: ProductGeneric[A]
    val aFields: ArraySeq[aGeneric.Field[?]]
    override val bGeneric: ProductGeneric.CaseClassGeneric[B]

    override final def convert(a: Expr[A])(using Quotes): Expr[B] =
      bGeneric.instantiate.id { [b] => (_, _) ?=> (bField: bGeneric.Field[b]) =>
        aFields(bField.idx)
          .typedAs[b]
          .fromParent(a)
      }

    override final def convertExpressions[F[_]](expressions: Expressions[F, A])(using Quotes): Expressions[F, B] =
      new Expressions[F, B](expressions.fTpe, bGeneric.tpe, aFields.map { aField => expressions.expressions(aField.idx) })

  }

}
