package oxygen.meta.k0

import oxygen.meta.*
import oxygen.quoted.*
import scala.quoted.*

trait Derivable[F[_]] { der =>

  // TODO (KR) : support this:
  //           : type ProductF[A] <: F[A]
  //           : type SumF[A] <: F[A]

  protected val deriveConfig: Derivable.Config = Derivable.Config()
  protected def productDeriver[A](using Quotes, Type[F], Type[A], ProductGeneric[A], Derivable[F]): Derivable.ProductDeriver[F, A]
  protected def sumDeriver[A](using Quotes, Type[F], Type[A], SumGeneric[A], Derivable[F]): Derivable.SumDeriver[F, A]

  private[meta] final def deriveFromGenericImpl[A](g: ProductOrSumGeneric[A])(using Quotes, Type[F], Type[A]): Expr[F[A]] = {
    given Derivable[F] = this
    val res: Expr[F[A]] = g.productOrSumGenericSelf match {
      case g: ProductGeneric[A] =>
        given ProductGeneric[A] = g
        productDeriver[A].derive
      case g: SumGeneric[A] =>
        given SumGeneric[A] = g
        sumDeriver[A].derive
    }

    k0.annotationUtil.showSpecific[F, F[A]](g.annotations, res)

    res
  }

  protected final def derivedImpl[A](using Quotes, Type[F], Type[A]): Expr[F[A]] =
    deriveFromGenericImpl(Generic.of[A](deriveConfig))

  final given Derivable[F] = this

  object deriveInternal {
    def productDeriver[A](using Quotes, Type[F], Type[A], ProductGeneric[A], Derivable[F]): Derivable.ProductDeriver[F, A] = der.productDeriver[A]
    def sumDeriver[A](using Quotes, Type[F], Type[A], SumGeneric[A], Derivable[F]): Derivable.SumDeriver[F, A] = der.sumDeriver[A]
  }

  /**
    * Unfortunately, scala macros do not allow this to be implemented in [[Derivable]].
    * Therefore, every companion object that extends [[Derivable]] must implement this function with the following body:
    *      ${ derivedImpl[A] }
    */
  inline def derived[A]: F[A]

}
object Derivable {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Config
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Config(
      defaultOrdinalStrategy: SumGeneric.OrdinalStrategy = SumGeneric.OrdinalStrategy.SourcePosition,
      defaultUnrollStrategy: SumGeneric.UnrollStrategy = SumGeneric.UnrollStrategy.Unroll,
      overrideUnrollStrategyBehavior: SumGeneric.OverrideUnrollStrategyBehavior = SumGeneric.OverrideUnrollStrategyBehavior.Allow,
  )

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      ProductDeriver
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  abstract class ProductDeriver[F[_], A](using
      final val quotes: Quotes,
      final val fTpe: Type[F],
      final val aTpe: Type[A],
      final val generic: ProductGeneric[A],
  ) {

    def derive: Expr[F[A]]

    /**
      * This only works if the [[Derivable.ProductDeriver]] in your [[Derivable]] uses [[Derivable.ProductDeriver.withInstances]].
      */
    def deriveWithInstances(exprs: Expressions[F, A])(using quotes: Quotes): Expr[F[A]] =
      report.errorAndAbort("`deriveWithInstances` is only supported when you use `ProductDeriver.withInstances`")

  }
  object ProductDeriver {

    final class NotSupported[F[_], A](using Quotes, Type[F], Type[A], ProductGeneric[A]) extends ProductDeriver[F, A] {
      override def derive: Expr[F[A]] = report.errorAndAbort(s"Auto derivation of product-types is not supported for ${Type.show[F]}")
    }

    def notSupported[F[_], A](using Quotes, Type[F], Type[A], ProductGeneric[A]): ProductDeriver[F, A] = new NotSupported[F, A]

    final class WithInstances[F[_], A](f: Quotes ?=> Expressions[F, A] => ProductDeriver[F, A])(using Quotes, Type[F], Type[A], ProductGeneric[A]) extends ProductDeriver[F, A] {

      override def derive: Expr[F[A]] = generic.cacheVals.summonTypeClasses[F]().defineAndUse { f(_).derive }

      override def deriveWithInstances(exprs: Expressions[F, A])(using quotes: Quotes): Expr[F[A]] =
        f(using quotes)(exprs).derive

    }

    def withInstances[F[_], A](f: Quotes ?=> Expressions[F, A] => ProductDeriver[F, A])(using Quotes, Type[F], Type[A], ProductGeneric[A]) =
      new WithInstances[F, A](f)

    final class WithDisjointInstances[ParentF[_], ChildF[_], A](f: Quotes ?=> Expressions[ChildF, A] => ProductDeriver[ParentF, A])(using
        Quotes,
        Type[ParentF],
        Type[ChildF],
        Type[A],
        ProductGeneric[A],
    ) extends ProductDeriver[ParentF, A] {

      override def derive: Expr[ParentF[A]] = generic.cacheVals.summonTypeClasses[ChildF]().defineAndUse { f(_).derive }

    }

    def withDisjointInstances[ParentF[_], ChildF[_], A](f: Quotes ?=> Expressions[ChildF, A] => ProductDeriver[ParentF, A])(using Quotes, Type[ParentF], Type[ChildF], Type[A], ProductGeneric[A]) =
      new WithDisjointInstances[ParentF, ChildF, A](f)

    final class Impl[F[_], A](value: () => Expr[F[A]])(using Quotes, Type[F], Type[A], ProductGeneric[A]) extends ProductDeriver[F, A] {
      override def derive: Expr[F[A]] = value()
    }

    def impl[F[_], A](value: => Expr[F[A]])(using Quotes, Type[F], Type[A], ProductGeneric[A]): ProductDeriver[F, A] =
      new Impl[F, A](() => value)

    abstract class Split[F[_], A](using Quotes, Type[F], Type[A], ProductGeneric[A]) extends ProductDeriver[F, A] {

      def deriveCaseClass(generic: ProductGeneric.CaseClassGeneric[A]): Expr[F[A]]

      def deriveAnyVal[B: Type](generic: ProductGeneric.AnyValGeneric[A, B]): Expr[F[A]] = deriveCaseClass(generic)

      def deriveCaseObject(generic: ProductGeneric.CaseObjectGeneric[A]): Expr[F[A]]

      override final def derive: Expr[F[A]] = generic match {
        case generic0: ProductGeneric.AnyValGeneric[A, _] =>
          type B
          val generic: ProductGeneric.AnyValGeneric[A, B] = generic0.singleFieldTypedAs[B]
          given Type[B] = generic.bTpe
          deriveAnyVal(generic)
        case generic: ProductGeneric.CaseClassGeneric[A]  => deriveCaseClass(generic)
        case generic: ProductGeneric.CaseObjectGeneric[A] => deriveCaseObject(generic)
      }

    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      SumDeriver
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  abstract class SumDeriver[F[_], A](using
      final val quotes: Quotes,
      final val fTpe: Type[F],
      final val aTpe: Type[A],
      final val generic: SumGeneric[A],
  ) {

    def derive: Expr[F[A]]

  }
  object SumDeriver {

    final class NotSupported[F[_], A](using Quotes, Type[F], Type[A], SumGeneric[A]) extends SumDeriver[F, A] {
      override def derive: Expr[F[A]] = report.errorAndAbort(s"Auto derivation of sum-types is not supported for ${Type.show[F]}")
    }

    def notSupported[F[_], A](using Quotes, Type[F], Type[A], SumGeneric[A]): SumDeriver[F, A] = new NotSupported[F, A]

    final class WithInstances[F[_]: Derivable as derivable, A](f: Quotes ?=> Expressions[F, A] => SumDeriver[F, A])(using Quotes, Type[F], Type[A], SumGeneric[A]) extends SumDeriver[F, A] {
      override def derive: Expr[F[A]] =
        generic.cacheVals.summonTypeClassesOrDerive[F]() { [b <: A] => (_, _) ?=> (kase: generic.Case[b]) => derivable.deriveFromGenericImpl(kase.generic) }.defineAndUse { f(_).derive }
    }

    def withInstances[F[_], A](f: Quotes ?=> Expressions[F, A] => SumDeriver[F, A])(using Quotes, Type[F], Type[A], SumGeneric[A], Derivable[F]): SumDeriver[F, A] = new WithInstances[F, A](f)

    final class Impl[F[_], A](value: () => Expr[F[A]])(using Quotes, Type[F], Type[A], SumGeneric[A]) extends SumDeriver[F, A] {
      override def derive: Expr[F[A]] = value()
    }

    def impl[F[_], A](value: => Expr[F[A]])(using Quotes, Type[F], Type[A], SumGeneric[A]): SumDeriver[F, A] =
      new Impl[F, A](() => value)

    abstract class Split[F[_], A](using Quotes, Type[F], Type[A], SumGeneric[A]) extends SumDeriver[F, A] {

      def deriveFlat(generic: SumGeneric.FlatGeneric[A]): Expr[F[A]]

      def deriveEnum(generic: SumGeneric.EnumGeneric[A]): Expr[F[A]] = deriveFlat(generic)

      def deriveNested(generic: SumGeneric.NestedGeneric[A]): Expr[F[A]]

      override final def derive: Expr[F[A]] = generic match
        case generic: SumGeneric.EnumGeneric[A]   => deriveEnum(generic)
        case generic: SumGeneric.FlatGeneric[A]   => deriveFlat(generic)
        case generic: SumGeneric.NestedGeneric[A] => deriveNested(generic)

    }

  }

}
