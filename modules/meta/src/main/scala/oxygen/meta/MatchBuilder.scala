package oxygen.meta

import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*

final case class MatchBuilder[+I, +O](
    cases: Growable[MatchBuilder.Case[I, O]],
) {

  def ||[I2 >: I, O2 >: O](that: MatchBuilder[I2, O2]): MatchBuilder[I2, O2] =
    MatchBuilder(this.cases ++ that.cases)

  /**
    * Add an else case to your match statement.
    *
    * ```scala
    * case _ => $rhs
    * ```
    */
  def withWildcard[O2 >: O](rhs: Quotes ?=> Expr[O2]): MatchBuilder[I, O2] =
    this || MatchBuilder.wildcard[O2](rhs)

  /**
    * It seems the compiler might not figure out when a match is exhaustive.
    * Adding this case fixes that.
    *
    * ```scala
    * case unknown => throw new MatchError(unknown)
    * ```
    */
  def withNonExhaustive: MatchBuilder[I, O] =
    this || MatchBuilder.nonExhaustive

}
object MatchBuilder {

  val empty: MatchBuilder[Any, Nothing] = MatchBuilder(Growable.empty)

  def merge[I, O](all: Seq[MatchBuilder[I, O]]): MatchBuilder[I, O] =
    MatchBuilder.merge(Growable.many(all))

  def merge[I, O](all: Growable[MatchBuilder[I, O]]): MatchBuilder[I, O] =
    MatchBuilder(all.flatMap(_.cases))

  def wildcard[O](rhs: Quotes ?=> Expr[O]): MatchBuilder[Nothing, O] =
    Case.make[Nothing, Unit, O](
      CaseExtractor.wildcard,
      _ => rhs,
    )

  def nonExhaustive: MatchBuilder[Nothing, Nothing] =
    CaseExtractor.anything("unknown").withRHS { any => '{ throw new MatchError($any) } }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Build
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  extension [I, O](self: MatchBuilder[I, O]) {

    def matchOn(i: Expr[I])(using Quotes, Type[I], Type[O]): Expr[O] =
      Match.companion
        .apply(
          i.toTerm,
          self.cases.map(_.toCaseDef).to[List],
        )
        .asExprOf[O]

    def matchOn[I1: Type, I2: Type](
        i1: Expr[I1],
        i2: Expr[I2],
    )(using Quotes, Type[I], Type[O], (I1, I2) <:< I): Expr[O] =
      self.matchOn { '{ ($i1, $i2) }.asExprOf[I] }

    def matchOn[I1: Type, I2: Type, I3: Type](
        i1: Expr[I1],
        i2: Expr[I2],
        i3: Expr[I3],
    )(using Quotes, Type[I], Type[O], (I1, I2, I3) <:< I): Expr[O] =
      self.matchOn { '{ ($i1, $i2, $i3) }.asExprOf[I] }

    def matchOn[I1: Type, I2: Type, I3: Type, I4: Type](
        i1: Expr[I1],
        i2: Expr[I2],
        i3: Expr[I3],
        i4: Expr[I4],
    )(using Quotes, Type[I], Type[O], (I1, I2, I3, I4) <:< I): Expr[O] =
      self.matchOn { '{ ($i1, $i2, $i3, $i4) }.asExprOf[I] }

    def matchOn[I1: Type, I2: Type, I3: Type, I4: Type, I5: Type](
        i1: Expr[I1],
        i2: Expr[I2],
        i3: Expr[I3],
        i4: Expr[I4],
        i5: Expr[I5],
    )(using Quotes, Type[I], Type[O], (I1, I2, I3, I4, I5) <:< I): Expr[O] =
      self.matchOn { '{ ($i1, $i2, $i3, $i4, $i5) }.asExprOf[I] }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      ...
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait Case[+I, +O] {

    def toCaseDef(using Quotes): CaseDef

  }
  object Case {

    def make[I, R, O](
        extractor: CaseExtractor[I, R],
        guard: Quotes ?=> R => Expr[Boolean],
        rhs: Quotes ?=> R => Expr[O],
    ): MatchBuilder[I, O] =
      MatchBuilder(
        Growable.single(
          CaseImpl[I, R, O](
            extractor = extractor,
            guard = Some(quotes => r => guard(using quotes)(r)),
            rhs = quotes => r => rhs(using quotes)(r),
          ),
        ),
      )

    def make[I, R, O](
        extractor: CaseExtractor[I, R],
        rhs: Quotes ?=> R => Expr[O],
    ): MatchBuilder[I, O] =
      MatchBuilder(
        Growable.single(
          CaseImpl[I, R, O](
            extractor = extractor,
            guard = None,
            rhs = quotes => r => rhs(using quotes)(r),
          ),
        ),
      )

  }

  final case class CaseImpl[I, R, O](
      extractor: CaseExtractor[I, R],
      guard: Option[Quotes => R => Expr[Boolean]],
      rhs: Quotes => R => Expr[O],
  ) extends Case[I, O] {

    override def toCaseDef(using quotes: Quotes): CaseDef = {
      val pat: CaseExtractor.Pattern[R] = extractor.build

      CaseDef.companion.apply(
        pat.elems.to[List] match {
          case Nil           => report.errorAndAbort("empty pattern?")
          case single :: Nil => single.pat
          case many          =>
            val tupleSym = Symbol.tupleClass(many.size).companionModule
            Unapply.companion.apply(
              tupleSym.toTerm
                .select("unapply")
                .appliedToTypes(many.map(_.typeRepr)),
              Nil,
              many.map(_.pat),
            )
        },
        guard.map { f => f(quotes)(pat.r).toTerm },
        rhs(quotes)(pat.r).toTerm,
      )
    }

  }

}
