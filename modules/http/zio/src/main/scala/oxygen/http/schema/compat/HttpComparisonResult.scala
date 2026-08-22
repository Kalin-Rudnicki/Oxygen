package oxygen.http.schema.compat

import oxygen.predef.core.*

/**
  * The result of comparing two [[oxygen.http.schema.compiled.RawCompiledApiSpec]]s — the HTTP-layer
  * analogue of [[oxygen.schema.compat.ComparisonResult]].
  *
  * Where the type-level result is a recursive tree (built to '''render''' a type diff), the HTTP
  * result is an accumulating monoid of already-classified verdicts: the breaking changes
  * ([[HttpBreak]]) and the backwards-compatible ones ([[HttpChange]]), each tagged with its
  * [[HttpLocation]]. That keeps `isCompatible` / the three-way [[HttpCompatibility]] summary trivial
  * while preserving where each finding came from. Like `ComparisonResult`, it carries no `JsonCodec`
  * — it is an in-memory analysis result, not a serialized artifact (the serialized artifact is the
  * `RawCompiledApiSpec` itself).
  */
final case class HttpComparisonResult(
    breaks: List[HttpBreak],
    changes: List[HttpChange],
) {

  def ++(that: HttpComparisonResult): HttpComparisonResult =
    HttpComparisonResult(this.breaks ++ that.breaks, this.changes ++ that.changes)

  /** Backwards-compatible iff there are no breaks (additions / relaxations are fine). */
  def isCompatible: Boolean = breaks.isEmpty

  def compatibility: HttpCompatibility =
    if breaks.nonEmpty then HttpCompatibility.Breaking
    else if changes.nonEmpty then HttpCompatibility.BackwardsCompatible
    else HttpCompatibility.ExactEqual

  def toIndentedString: IndentedString =
    IndentedString.keyValueSection(s"HttpComparisonResult [${compatibility}]:")(
      s"breaks (${breaks.size}): " -> breaks.map(_.toIndentedString),
      s"changes (${changes.size}): " -> changes.map(_.toIndentedString),
    )

  override def toString: String = toIndentedString.toString

}
object HttpComparisonResult {

  val empty: HttpComparisonResult = HttpComparisonResult(Nil, Nil)

  def break(b: HttpBreak): HttpComparisonResult = HttpComparisonResult(List(b), Nil)
  def change(c: HttpChange): HttpComparisonResult = HttpComparisonResult(Nil, List(c))

  def combineAll(results: Seq[HttpComparisonResult]): HttpComparisonResult =
    results.foldLeft(HttpComparisonResult.empty)(_ ++ _)

}
