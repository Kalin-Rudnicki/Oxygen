package oxygen.http.schema

import zio.http.Status

sealed trait ExpectedStatuses {

  final def contains(s: Status): Boolean = this match
    case ExpectedStatuses.None               => false
    case ExpectedStatuses.Exact(status)      => status == s
    case ExpectedStatuses.OneOf(statuses)    => statuses.contains(s)
    case range: ExpectedStatuses.StatusRange => s.code >= range.min && s.code <= range.max
    case ExpectedStatuses.Or(a, b)           => a.contains(s) || b.contains(s)

  final def ||(that: ExpectedStatuses): ExpectedStatuses = (this, that) match
    case (ExpectedStatuses.None, that) => that
    case (_, ExpectedStatuses.None)    => this
    case _                             => ExpectedStatuses.Or(this, that)

}
object ExpectedStatuses {

  def oneOf(statuses: ExpectedStatuses*): ExpectedStatuses =
    statuses.foldLeft(ExpectedStatuses.None: ExpectedStatuses)(_ || _)

  case object None extends ExpectedStatuses
  final case class Exact(status: Status) extends ExpectedStatuses
  final case class OneOf(statuses: Set[Status]) extends ExpectedStatuses
  final case class Or(a: ExpectedStatuses, b: ExpectedStatuses) extends ExpectedStatuses

  sealed abstract class StatusRange(
      final val name: String,
      final val display: String,
      final val min: Int,
      final val max: Int,
  ) extends ExpectedStatuses
  object StatusRange {

    case object Informational extends StatusRange("Informational", "1xx", 100, 199)
    case object Success extends StatusRange("Informational", "2xx", 200, 299)
    case object Redirection extends StatusRange("Informational", "3xx", 300, 399)

    sealed abstract class Error(name: String, display: String, min: Int, max: Int) extends StatusRange(name, display, min, max)
    case object Error extends Error("Error", "4xx/5xx", 400, 599)
    case object ClientError extends Error("ClientError", "4xx", 400, 499)
    case object ServerError extends Error("ServerError", "5xx", 500, 599)

  }

}
