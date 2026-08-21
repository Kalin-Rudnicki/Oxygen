package oxygen.http.schema.compat

import oxygen.predef.core.*
import oxygen.schema.compat.ComparisonResult
import zio.http.Method

/**
  * A single backwards-'''incompatible''' (breaking) change found while comparing two
  * [[oxygen.http.schema.compiled.RawCompiledApiSpec]]s — the HTTP-layer analogue of a
  * [[oxygen.schema.compat.ComparisonResult.Different]] node, but already reduced to a verdict.
  *
  * Every case carries the [[HttpLocation]] where it was found so the flat break list stays navigable
  * without re-walking a tree.
  */
sealed trait HttpBreak {
  def location: HttpLocation
  def message: String

  final def toIndentedString: IndentedString = IndentedString.Str(s"${location.render}: $message")
}
object HttpBreak {

  private def showMethod(method: Option[Method]): String = method.fold("<any>")(_.name)

  /** An API present in `from` is gone in `to` — every endpoint under it was removed. */
  final case class ApiRemoved(location: HttpLocation, name: String) extends HttpBreak {
    override def message: String = s"api '$name' was removed"
  }

  /** An endpoint present in `from` is gone in `to`. */
  final case class EndpointRemoved(location: HttpLocation, name: String) extends HttpBreak {
    override def message: String = s"endpoint '$name' was removed"
  }

  final case class MethodChanged(location: HttpLocation, from: Option[Method], to: Option[Method]) extends HttpBreak {
    override def message: String = s"method changed ${showMethod(from)} -> ${showMethod(to)}"
  }

  /** A path shape present in `from` no longer exists in `to` (rename / re-shape). */
  final case class PathRemoved(location: HttpLocation, shape: String) extends HttpBreak {
    override def message: String = s"path shape '$shape' was removed"
  }

  /** A new required request param (query / header / path) that old clients don't send. */
  final case class RequiredParamAdded(location: HttpLocation, name: String) extends HttpBreak {
    override def message: String = s"required param '$name' was added"
  }

  /** An existing param tightened (input optional -> required, or output required -> optional). */
  final case class ParamRequirednessTightened(location: HttpLocation, name: String) extends HttpBreak {
    override def message: String = s"param '$name' requiredness tightened"
  }

  /** A response header that was always present is gone (client expected it). */
  final case class RequiredResponseHeaderRemoved(location: HttpLocation, name: String) extends HttpBreak {
    override def message: String = s"required response header '$name' was removed"
  }

  /** Request body went from empty to required — old clients send no body. */
  final case class RequestBodyAdded(location: HttpLocation) extends HttpBreak {
    override def message: String = "request body was added (empty -> required)"
  }

  /** Response body went from present to empty — clients expected a payload. */
  final case class ResponseBodyRemoved(location: HttpLocation) extends HttpBreak {
    override def message: String = "response body was removed (present -> empty)"
  }

  /** Body kind changed between incompatible shapes (e.g. single-value <-> SSE <-> line-stream). */
  final case class BodyKindChanged(location: HttpLocation, from: String, to: String) extends HttpBreak {
    override def message: String = s"body kind changed $from -> $to"
  }

  /** A type-reference transition the type-level diff judged incompatible in this variance position. */
  final case class IncompatibleType(location: HttpLocation, variance: HttpVariance, comparison: ComparisonResult) extends HttpBreak {
    override def message: String = s"incompatible type change (${variance})"
  }

}
