package oxygen.http

import oxygen.http.core.*
import oxygen.http.server.DeriveEndpoints
import oxygen.json.*
import oxygen.schema.JsonSchema
import scala.annotation.experimental
import zio.*

/** A small product. */
final case class Small(a: String) derives JsonSchema

/** `Small` plus a second required field — as a request body it is strictly '''narrower''' than `Small`. */
final case class Big(a: String, b: Int) derives JsonSchema

/** A sum with two cases. */
enum Shape derives JsonSchema {
  case Circle(radius: Int)
  case Square(side: Int)
}

/** `Shape` plus a third case — as a response body it is a '''widened''' sum. */
enum ShapeWide derives JsonSchema {
  case Circle(radius: Int)
  case Square(side: Int)
  case Triangle(base: Int, height: Int)
}

/**
  * Fixture API for `HttpCompatSpec`. Compiled once; the resulting endpoints + shared schema bundle are
  * then structurally mutated to build the `to` side of each comparison (distinct type names — `Small` /
  * `Big`, `Shape` / `ShapeWide` — so both versions coexist in the one bundle the diff resolves against).
  */
@experimental
trait CompatFixtureApi {

  @route.post("/small")
  def takesSmall(
      @param.body.json body: Small,
  ): IO[String, Shape]

  @route.post("/big")
  def takesBig(
      @param.body.json body: Big,
      @param.query tag: Option[String],
  ): IO[String, ShapeWide]

}
object CompatFixtureApi {
  given DeriveEndpoints[CompatFixtureApi] = DeriveEndpoints.derived
}
