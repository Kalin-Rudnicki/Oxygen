package oxygen.meta

import oxygen.core.collection.*
import oxygen.quoted.{Wildcard as WILD, *}
import scala.Tuple.{++, :*}
import scala.quoted.*

trait CaseExtractor[I, R] private {

  final def ++[I2, R2](that: CaseExtractor[I2, R2])(using zip: CaseExtractor.Zip[I, I2, R, R2]): CaseExtractor[zip.I3, zip.R3] =
    zip.zip(this, that)

  final def map[R2](f: Quotes ?=> R => R2): CaseExtractor[I, R2] =
    CaseExtractor.Mapped(this, quotes => f(using quotes)(_))

  final def withRHS[O](rhs: Quotes ?=> R => Expr[O]): MatchBuilder[I, O] =
    MatchBuilder.Case.make(this, rhs)

  private[meta] def build(using Quotes): CaseExtractor.Pattern[R]

}
object CaseExtractor {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      API
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def extract[A: Type as aTpe](bindName: String): CaseExtractor[A, Expr[A]] =
    Extract(aTpe, bindName)

  def const[A: Type as aTpe](lhs: Quotes ?=> Expr[A]): CaseExtractor[A, Unit] =
    Const(aTpe, quotes => lhs(using quotes))

  def anything(bindName: String): CaseExtractor[Nothing, Expr[Any]] =
    Anything(bindName)

  def wildcard: CaseExtractor[Nothing, Unit] =
    Wildcard

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Root Elems
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private final case class Merge[I1, I2, _I3, R1, R2, _R3](
      _1: CaseExtractor[I1, R1],
      _2: CaseExtractor[I2, R2],
      mergePattern: (Pattern[R1], Pattern[R2]) => Pattern[_R3],
  ) extends CaseExtractor[_I3, _R3] {

    override private[meta] def build(using Quotes): Pattern[_R3] =
      mergePattern(_1.build, _2.build)

  }

  private final case class Mapped[I, R, R2](
      inner: CaseExtractor[I, R],
      map: Quotes => R => R2,
  ) extends CaseExtractor[I, R2] {

    override private[meta] def build(using quotes: Quotes): Pattern[R2] =
      inner.build.map(map(quotes)(_))

  }

  private final case class Extract[A](
      tpe: Type[A],
      bindName: String,
  ) extends CaseExtractor[A, Expr[A]] {

    override private[meta] def build(using Quotes): Pattern[Expr[A]] = {
      val repr: TypeRepr = TypeRepr.of[A](using tpe)
      val bindSymbol: Symbol = Symbol.newBind(Symbol.spliceOwner, bindName, Flags.EmptyFlags, repr)
      val bind: Bind = Bind.companion.apply(bindSymbol, Typed.companion.apply(WILD.companion.apply(), repr.typeTree))
      val term: Term = Ref.companion.apply(bindSymbol)

      Pattern.single(
        repr,
        bind,
        term.asExprOf[A](using tpe),
      )
    }

  }

  private final case class Const[A](
      tpe: Type[A],
      lhs: Quotes => Expr[A],
  ) extends CaseExtractor[A, Unit] {

    override private[meta] def build(using quotes: Quotes): Pattern[Unit] =
      Pattern.single(
        TypeRepr.of[A](using tpe),
        lhs(quotes).toTerm,
        (),
      )

  }

  private final case class Anything(
      bindName: String,
  ) extends CaseExtractor[Nothing, Expr[Any]] {

    override private[meta] def build(using Quotes): Pattern[Expr[Any]] = {
      val repr: TypeRepr = TypeRepr.of[Any]
      val bindSymbol: Symbol = Symbol.newBind(Symbol.spliceOwner, bindName, Flags.EmptyFlags, repr)
      val bind: Bind = Bind.companion.apply(bindSymbol, Typed.companion.apply(WILD.companion.apply(), repr.typeTree))
      val term: Term = Ref.companion.apply(bindSymbol)

      Pattern.single(
        repr,
        bind,
        term.asExprOf[Any],
      )
    }

  }

  private case object Wildcard extends CaseExtractor[Nothing, Unit] {

    override private[meta] def build(using Quotes): Pattern[Unit] =
      Pattern.single(
        TypeRepr.of[Any],
        WILD.companion.apply(),
        (),
      )

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Pattern
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  // TODO (KR) : This might need special handling if a root CaseExtractor is build using a tuple type.
  //           : For now, this case is ignored.
  //           : This is because `CaseExtractor[(A, B), ?] ++ CaseExtractor[C, ?]` will yield `CaseExtractor[(A, B, C), ?]`,
  //           : but under the hood, `(A, B)` is a single match elem.
  private[meta] final case class Pattern[R](
      elems: Growable[Pattern.Elem],
      r: R,
  ) {

    def mergeWith[R2, R3](that: Pattern[R2])(f: (R, R2) => R3): Pattern[R3] =
      Pattern(this.elems ++ that.elems, f(this.r, that.r))

    def map[R2](f: R => R2): Pattern[R2] =
      Pattern(this.elems, f(this.r))

  }
  private[meta] object Pattern {

    def single[R](
        typeRepr: TypeRepr,
        pat: Tree,
        r: R,
    ): Pattern[R] =
      Pattern(
        Growable.single(Elem(typeRepr, pat)),
        r,
      )

    final case class Elem(typeRepr: TypeRepr, pat: Tree)

    extension [R1 <: Tuple](r1: Pattern[R1])
      def ++[R2 <: Tuple](r2: Pattern[R2]): Pattern[R1 ++ R2] =
        r1.mergeWith(r2)(_ ++ _)

    extension [R1 <: Tuple](r1: Pattern[R1])
      def :*[R2](r2: Pattern[R2]): Pattern[R1 :* R2] =
        r1.mergeWith(r2)(_ :* _)

    extension [R1](r1: Pattern[R1])
      def *:[R2 <: Tuple](r2: Pattern[R2]): Pattern[R1 *: R2] =
        r1.mergeWith(r2)(_ *: _)

    extension [R1](r1: Pattern[R1])
      def <*>[R2](r2: Pattern[R2]): Pattern[(R1, R2)] =
        r1.mergeWith(r2)((_, _))

    extension [R1](r1: Pattern[R1])
      def <*[R2](r2: Pattern[R2]): Pattern[R1] =
        r1.mergeWith(r2)((r, _) => r)

    extension [R1](r1: Pattern[R1])
      def *>[R2](r2: Pattern[R2]): Pattern[R2] =
        r1.mergeWith(r2)((_, r) => r)

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Zip
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait Zip[I1, I2, R1, R2] {
    type I3
    type R3

    def zip(_1: CaseExtractor[I1, R1], _2: CaseExtractor[I2, R2]): CaseExtractor[I3, R3]

  }
  object Zip extends ZipLowPriority.LowPriority1 {
    type Aux[I1, I2, _I3, R1, R2, _R3] = Zip[I1, I2, R1, R2] { type I3 = _I3; type R3 = _R3 }

    final case class Merge[I1, I2, _I3, R1, R2, _R3](
        mergePattern: (Pattern[R1], Pattern[R2]) => Pattern[_R3],
    ) extends Zip[I1, I2, R1, R2] {
      override type I3 = _I3
      override type R3 = _R3

      override def zip(_1: CaseExtractor[I1, R1], _2: CaseExtractor[I2, R2]): CaseExtractor[_I3, _R3] =
        CaseExtractor.Merge(_1, _2, mergePattern)
    }

    given zip_TupTup_TupTup: [I1 <: Tuple, I2 <: Tuple, R1 <: Tuple, R2 <: Tuple] => Zip.Aux[I1, I2, I1 ++ I2, R1, R2, R1 ++ R2] =
      Zip.Merge { _ ++ _ }

    given zip_TupTup_TupUnit: [I1 <: Tuple, I2 <: Tuple, R1 <: Tuple] => Zip.Aux[I1, I2, I1 ++ I2, R1, Unit, R1] =
      Zip.Merge { _ <* _ }

    given zip_TupUnit_TupTup: [I1 <: Tuple, I2 <: Tuple, R2 <: Tuple] => Zip.Aux[I1, I2, I1 ++ I2, Unit, R2, R2] =
      Zip.Merge { _ *> _ }

    given zip_TupUnit_TupUnit: [I1 <: Tuple, I2 <: Tuple] => Zip.Aux[I1, I2, I1 ++ I2, Unit, Unit, Unit] =
      Zip.Merge { _ *> _ }

  }

  object ZipLowPriority {

    trait LowPriority1 extends LowPriority2 {

      given zip_TupTup_TupId: [I1 <: Tuple, I2 <: Tuple, R1 <: Tuple, R2] => Zip.Aux[I1, I2, I1 ++ I2, R1, R2, R1 :* R2] =
        Zip.Merge { _ :* _ }

      given zip_TupId_TupTup: [I1 <: Tuple, I2 <: Tuple, R1, R2 <: Tuple] => Zip.Aux[I1, I2, I1 ++ I2, R1, R2, R1 *: R2] =
        Zip.Merge { _ *: _ }

      given zip_TupId_TupUnit: [I1 <: Tuple, I2 <: Tuple, R1] => Zip.Aux[I1, I2, I1 ++ I2, R1, Unit, R1] =
        Zip.Merge { _ <* _ }

      given zip_TupUnit_TupId: [I1 <: Tuple, I2 <: Tuple, R2] => Zip.Aux[I1, I2, I1 ++ I2, Unit, R2, R2] =
        Zip.Merge { _ *> _ }

      given zip_TupTup_IdUnit: [I1 <: Tuple, I2, R1 <: Tuple] => Zip.Aux[I1, I2, I1 :* I2, R1, Unit, R1] =
        Zip.Merge { _ <* _ }

      given zip_TupUnit_IdUnit: [I1 <: Tuple, I2] => Zip.Aux[I1, I2, I1 :* I2, Unit, Unit, Unit] =
        Zip.Merge { _ *> _ }

      given zip_IdUnit_TupTup: [I1, I2 <: Tuple, R2 <: Tuple] => Zip.Aux[I1, I2, I1 *: I2, Unit, R2, R2] =
        Zip.Merge { _ *> _ }

      given zip_IdUnit_TupUnit: [I1, I2 <: Tuple] => Zip.Aux[I1, I2, I1 *: I2, Unit, Unit, Unit] =
        Zip.Merge { _ *> _ }

    }

    trait LowPriority2 extends LowPriority3 {

      given zip_TupId_TupId: [I1 <: Tuple, I2 <: Tuple, R1, R2] => Zip.Aux[I1, I2, I1 ++ I2, R1, R2, (R1, R2)] =
        Zip.Merge { _ <*> _ }

      given zip_TupTup_IdId: [I1 <: Tuple, I2, R1 <: Tuple, R2] => Zip.Aux[I1, I2, I1 :* I2, R1, R2, R1 :* R2] =
        Zip.Merge { _ :* _ }

      given zip_TupId_IdUnit: [I1 <: Tuple, I2, R1] => Zip.Aux[I1, I2, I1 :* I2, R1, Unit, R1] =
        Zip.Merge { _ <* _ }

      given zip_TupUnit_IdId: [I1 <: Tuple, I2, R2] => Zip.Aux[I1, I2, I1 :* I2, Unit, R2, R2] =
        Zip.Merge { _ *> _ }

      given zip_IdId_TupTup: [I1, I2 <: Tuple, R1, R2 <: Tuple] => Zip.Aux[I1, I2, I1 *: I2, R1, R2, R1 *: R2] =
        Zip.Merge { _ *: _ }

      given zip_IdId_TupUnit: [I1, I2 <: Tuple, R1] => Zip.Aux[I1, I2, I1 *: I2, R1, Unit, R1] =
        Zip.Merge { _ <* _ }

      given zip_IdUnit_TupId: [I1, I2 <: Tuple, R2] => Zip.Aux[I1, I2, I1 *: I2, Unit, R2, R2] =
        Zip.Merge { _ *> _ }

      given zip_IdUnit_IdUnit: [I1, I2] => Zip.Aux[I1, I2, (I1, I2), Unit, Unit, Unit] =
        Zip.Merge { _ *> _ }

    }

    trait LowPriority3 extends LowPriority4 {

      given zip_TupId_IdId: [I1 <: Tuple, I2, R1, R2] => Zip.Aux[I1, I2, I1 :* I2, R1, R2, (R1, R2)] =
        Zip.Merge { _ <*> _ }

      given zip_IdId_TupId: [I1, I2 <: Tuple, R1, R2] => Zip.Aux[I1, I2, I1 *: I2, R1, R2, (R1, R2)] =
        Zip.Merge { _ <*> _ }

      given zip_IdId_IdUnit: [I1, I2, R1] => Zip.Aux[I1, I2, (I1, I2), R1, Unit, R1] =
        Zip.Merge { _ <* _ }

      given zip_IdUnit_IdId: [I1, I2, R2] => Zip.Aux[I1, I2, (I1, I2), Unit, R2, R2] =
        Zip.Merge { _ *> _ }

    }

    trait LowPriority4 {

      given zip_IdId_IdId: [I1, I2, R1, R2] => Zip.Aux[I1, I2, (I1, I2), R1, R2, (R1, R2)] =
        Zip.Merge { _ <*> _ }

    }

  }

}
