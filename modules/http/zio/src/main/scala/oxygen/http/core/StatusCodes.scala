package oxygen.http.core

import oxygen.http.core.generic.given
import oxygen.http.schema.ExpectedStatuses
import oxygen.meta.given
import oxygen.meta.k0.*
import oxygen.quoted.*
import scala.quoted.*
import zio.http.Status

trait StatusCodes[A] {
  val expectedStatuses: ExpectedStatuses

  /** For sum-typed responses, the status code each case maps to (keyed by case name). Empty otherwise. */
  val caseStatuses: List[StatusCodes.CaseStatus]
  def status(value: A): Status
}
object StatusCodes {

  inline def apply[A](using ev: StatusCodes[A]): StatusCodes[A] = ev

  /** Associates a sum-case (by name) with the HTTP status it is encoded as. */
  final case class CaseStatus(caseName: String, status: Status)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Impls
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final class DNE[A] extends StatusCodes[A] {
    override val expectedStatuses: ExpectedStatuses = ExpectedStatuses.None
    override val caseStatuses: List[CaseStatus] = Nil
    override def status(value: A): Status = throw new RuntimeException("this is supposed to be DNE!")
  }

  final case class Exact[A](exact: Status) extends StatusCodes[A] {
    override val expectedStatuses: ExpectedStatuses = ExpectedStatuses.Exact(exact)
    override val caseStatuses: List[CaseStatus] = Nil
    override def status(value: A): Status = exact
  }

  final case class Range[A](range: ExpectedStatuses.StatusRange) extends StatusCodes[A] {
    override val expectedStatuses: ExpectedStatuses = range
    override val caseStatuses: List[CaseStatus] = Nil
    override def status(value: A): Status = Status.fromInt(range.min)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Generic
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def derivedImpl[A: Type](using Quotes): Expr[StatusCodes[A]] =
    Generic.of[A](Derivable.Config(defaultUnrollStrategy = SumGeneric.UnrollStrategy.Unroll)).productOrSumGenericSelf match {
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

        val caseStatuses: List[Expr[StatusCodes.CaseStatus]] =
          generic.mapChildren
            .map { [b <: A] => (_, _) ?=> (kase: generic.Case[b]) =>
              val code: statusCode = kase.annotations.requiredOfValue[statusCode]
              '{ StatusCodes.CaseStatus(${ Expr(kase.name) }, ${ Expr(code.status) }) }
            }
            .to[List]

        val caseStatusesExpr: Expr[List[StatusCodes.CaseStatus]] = Expr.ofList(caseStatuses)

        def statusImpl(value: Expr[A]): Expr[Status] =
          generic.matcher.instance(value) { [b <: A] => (_, _) ?=> (kase: generic.Case[b]) =>
            val code: statusCode = kase.annotations.requiredOfValue[statusCode]
            kase.caseExtractor.withRHS { _ => Expr(code.status) }
          }

        '{
          new StatusCodes[A] {
            override val expectedStatuses: ExpectedStatuses = $statusesExpr
            override val caseStatuses: List[StatusCodes.CaseStatus] = $caseStatusesExpr
            override def status(value: A): Status = ${ statusImpl('value) }
          }
        }
      case _: SumGeneric[A] => report.errorAndAbort("not possible... not flattened?")
    }

  inline def derived[A]: StatusCodes[A] = ${ derivedImpl[A] }

}
