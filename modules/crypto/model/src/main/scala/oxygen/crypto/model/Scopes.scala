package oxygen.crypto.model

import oxygen.json.*

/**
  * An OAuth scope set — the space-delimited, case-sensitive list of scope tokens (RFC 6749 §3.3).
  * Modeled as a `Set` with a [[containsAll]] subset check for authorization decisions.
  */
final case class Scopes(values: Set[String]) {
  def containsAll(required: Set[String]): Boolean = required.subsetOf(values)
  def containsAll(required: Scopes): Boolean = required.values.subsetOf(values)
  def render: String = values.toList.sorted.mkString(" ")
}
object Scopes {

  val empty: Scopes = Scopes(Set.empty)

  def of(values: String*): Scopes = Scopes(values.toSet)

  /** Parse the wire form: space-delimited scope tokens. */
  def parse(raw: String): Scopes = Scopes(raw.split(' ').iterator.map(_.trim).filter(_.nonEmpty).toSet)

  given JsonCodec[Scopes] = JsonCodec.string.transform(parse, _.render)

}
