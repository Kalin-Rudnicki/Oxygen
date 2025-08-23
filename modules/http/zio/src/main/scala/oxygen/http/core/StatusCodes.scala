package oxygen.http.core

import oxygen.http.core.generic.given
import oxygen.http.schema.ExpectedStatuses
import oxygen.meta.K0.*
import oxygen.meta.given
import oxygen.quoted.*
import scala.quoted.*
import zio.http.Status

trait StatusCodes[A] {
  // TODO (KR) : this should contain a schema of which types return which codes
  val expectedStatuses: ExpectedStatuses
  def status(value: A): Status
}
object StatusCodes {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Impls
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final class DNE[A] extends StatusCodes[A] {
    override val expectedStatuses: ExpectedStatuses = ExpectedStatuses.None
    override def status(value: A): Status = throw new RuntimeException("this is supposed to be DNE!")
  }

  final case class Exact[A](exact: Status) extends StatusCodes[A] {
    override val expectedStatuses: ExpectedStatuses = ExpectedStatuses.Exact(exact)
    override def status(value: A): Status = exact
  }

  final case class Range[A](range: ExpectedStatuses.StatusRange) extends StatusCodes[A] {
    override val expectedStatuses: ExpectedStatuses = range
    override def status(value: A): Status = Status.fromInt(range.min)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Generic
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def derivedImpl[A: Type](using Quotes): Expr[StatusCodes[A]] =
    Generic.of[A](Derivable.Config(defaultUnrollStrategy = SumGeneric.UnrollStrategy.Unroll)) match {
      case generic: ProductGeneric[A] =>
        val code: statusCode = generic.annotations.requiredOfValue[statusCode]
        '{ StatusCodes.Exact(${ Expr(code.status) }) }
      case generic: SumGeneric.FlatGeneric[A] =>
        val statuses: List[Expr[Status]] =
          generic.mapChildren
            .map { [b <: A] => (_, _) ?=> (kase: generic.Case[b]) =>
              kase.annotations.requiredOfValue[statusCode].status
            }
            .to[List]
            .distinct
            .map(Expr(_))

        val statusesExpr: Expr[ExpectedStatuses] =
          statuses match
            case status :: Nil => '{ ExpectedStatuses.Exact($status) }
            case _             => '{ ExpectedStatuses.OneOf(Set(${ Expr.ofSeq(statuses) }*)) }

        def statusImpl(value: Expr[A]): Expr[Status] =
          generic.matcher.instance(value) { [b <: A] => (_, _) ?=> (kase: generic.Case[b]) =>
            val code: statusCode = kase.annotations.requiredOfValue[statusCode]
            kase.caseExtractor.withRHS { _ => Expr(code.status) }
          }

        '{
          new StatusCodes[A] {
            override val expectedStatuses: ExpectedStatuses = $statusesExpr
            override def status(value: A): Status = ${ statusImpl('value) }
          }
        }
      case _: SumGeneric[A] => report.errorAndAbort("not possible... not flattened?")
    }

  inline def derived[A]: StatusCodes[A] = ${ derivedImpl[A] }

}
