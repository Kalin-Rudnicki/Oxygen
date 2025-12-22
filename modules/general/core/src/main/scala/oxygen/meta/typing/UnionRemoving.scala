package oxygen.meta.typing

import oxygen.core.syntax.common.*
import oxygen.quoted.*
import scala.quoted.*
import scala.reflect.TypeTest

/**
  * Given a type [[Total]] and type [[Removing]], gives you a [[Remaining]] such that:
  * [[Total]] <\:< ([[Removing]] | [[Remaining]])
  *
  * extension[A](self: A)
  *   def filterPartial[B](using ev: UnionRemoving[B]): Either[ev.Remaining, B] =
  *     ev(self).flip
  */
trait UnionRemoving[_Total, _Removing] {

  final type Total = _Total
  final type Removing = _Removing
  type Remaining

  def apply(total: Total): Either[Removing, Remaining]

}
object UnionRemoving {

  type Aux[_Total, _Removing, _Remaining] = UnionRemoving[_Total, _Removing] { type Remaining = _Remaining }
  type NothingRemaining[_Total, _Removing] = UnionRemoving.Aux[_Total, _Removing, Nothing]

  trait Instance[_Total, _Removing, _Remaining] extends UnionRemoving[_Total, _Removing] {
    override final type Remaining = _Remaining
  }
  trait NothingRemainingInstance[_Total, _Removing] extends Instance[_Total, _Removing, Nothing] {

    protected def toLeft(total: Total): Removing
    override final def apply(total: Total): Either[Removing, Remaining] = toLeft(total).asLeft

  }

  private def derivedImpl[_Total: Type, _Removing: Type](using Quotes): Expr[UnionRemoving.Instance[_Total, _Removing, ?]] = {
    type _Remaining
    val totalTypeRepr: TypeRepr = TypeRepr.of[_Total]
    val removingTypeRepr: TypeRepr = TypeRepr.of[_Removing]
    val totalUnionParts: Set[TypeRepr] = totalTypeRepr.dealiasKeepOpaques.orChildren
    val removingUnionParts: Set[TypeRepr] = removingTypeRepr.dealiasKeepOpaques.orChildren

    val remainingParts: Set[TypeRepr] =
      totalUnionParts.filterNot { totalPart =>
        removingUnionParts.exists { removingPart =>
          removingPart >:> totalPart
        }
      }

    // TODO (KR) : emit error if nothing is removed?

    val remainingTypeRepr: TypeRepr = remainingParts.toList match
      case Nil      => TypeRepr.of[Nothing]
      case h :: Nil => h
      case h :: t   => t.foldLeft(h)(OrType.companion.apply(_, _))
    given Type[_Remaining] = remainingTypeRepr.asTypeOf

    val removingOrRemaining: TypeRepr = OrType.companion.apply(removingTypeRepr, remainingTypeRepr)

    if totalTypeRepr !<:< removingOrRemaining then
      report.errorAndAbort(s"This should always be true, but for some reason is not: ${totalTypeRepr.showAnsiCode} <:< ${removingOrRemaining.showAnsiCode}")

    def summonTypeTest[T: Type](tName: String) = {
      type TTT = TypeTest[_Total, T]
      Implicits.searchOption[TTT].getOrElse { report.errorAndAbort(s"Unable to find TypeTest[Total, $tName]: ${TypeRepr.of[TTT].showAnsiCode}") }
    }

    val wtfMessageImpl: String =
      s"""${TypeRepr.of[UnionRemoving.Instance[_Total, _Removing, _Remaining]]} failed to filter (this should never happen)
         |      Total: ${TypeRepr.of[_Total].showAnsiCode}
         |   Removing: ${TypeRepr.of[_Removing].showAnsiCode}
         |  Remaining: ${TypeRepr.of[_Remaining].showAnsiCode}
         |      value: """.stripMargin

    val res: (Expr[UnionRemoving.Instance[_Total, _Removing, ?]], TypeRepr) =
      if remainingParts.isEmpty then
        (
          '{
            new UnionRemoving.NothingRemainingInstance[_Total, _Removing] {
              override protected def toLeft(total: Total): Removing = ${ ('total).asExprOf[_Removing] }
            }
          },
          TypeRepr.of[UnionRemoving.NothingRemainingInstance[_Total, _Removing]],
        )
      else
        (
          '{
            new UnionRemoving.Instance[_Total, _Removing, _Remaining] {

              private val removingTypeTest: TypeTest[_Total, _Removing] = ${ summonTypeTest[_Removing]("Removing") }
              private val remainingTypeTest: TypeTest[_Total, _Remaining] = ${ summonTypeTest[_Remaining]("Remaining") }

              private def wtfMessage(total: Total): String =
                ${ Expr(wtfMessageImpl) } + total.toString

              override def apply(total: Total): Either[Removing, _Remaining] = total match
                case removingTypeTest(value)  => value.asLeft
                case remainingTypeTest(value) => value.asRight
                case _                        => throw new RuntimeException(wtfMessage(total))

            }
          },
          TypeRepr.of[UnionRemoving.Instance[_Total, _Removing, _Remaining]],
        )

    type ResT <: UnionRemoving.Instance[_Total, _Removing, _Remaining]
    given Type[ResT] = res._2.asTypeOf

    val castRes: Expr[ResT] =
      res._1.asExprOf[ResT]

    castRes
  }

  transparent inline def derived[Total, Removing]: UnionRemoving.Instance[Total, Removing, ?] = ${ derivedImpl[Total, Removing] }

  transparent inline given autoInstance: [Total, Removing] => UnionRemoving.Instance[Total, Removing, ?] = derived[Total, Removing]

}
