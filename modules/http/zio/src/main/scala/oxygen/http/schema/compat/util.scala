package oxygen.http.schema.compat

import oxygen.predef.core.*

/**
  * Variance of the position a type reference sits in, which decides how a type-level
  * [[oxygen.schema.compat.ComparisonResult]] is read as an HTTP compatibility verdict:
  *
  *   - '''Contravariant''' — a request '''input''' (path / query / header / request-body). Evolving is
  *     compatible when the server accepts a ''superset'' of what it used to (widening).
  *   - '''Covariant''' — a response '''output''' (response-body / response-header). Evolving is
  *     compatible when the server produces a ''subset'' of what it used to (narrowing).
  */
enum HttpVariance {
  case Contravariant // request input
  case Covariant // response output
}

/** Where in the compiled spec a break / change was found — a dotted breadcrumb. */
final case class HttpLocation(segments: List[String]) {
  def /(segment: String): HttpLocation = HttpLocation(segments :+ segment)
  def render: String = if segments.isEmpty then "<spec>" else segments.mkString(".")
}
object HttpLocation {
  val root: HttpLocation = HttpLocation(Nil)
}

/** A non-breaking (backwards-compatible) difference — an addition or a relaxation. */
final case class HttpChange(location: HttpLocation, message: String) {
  def toIndentedString: IndentedString = IndentedString.Str(s"${location.render}: $message")
}

/**
  * The three-way summary of an HTTP spec comparison — the analogue of the ticket's
  * "exact-equal / backwards-compatible-addition / breaking".
  */
enum HttpCompatibility {
  case ExactEqual // no differences at all
  case BackwardsCompatible // only additions / relaxations — old clients keep working
  case Breaking // at least one incompatible change
}
