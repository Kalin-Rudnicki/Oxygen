package oxygen.meta.exprEvaluator

import oxygen.predef.core.*

object SqlModels extends scala.App {

  ///////  ///////////////////////////////////////////////////////////////

  final case class TwoInts(int1: Int, int2: Int) derives SqlSchema, Show

  final case class ThreeStrings(string1: String, string2: String, string3: String) derives SqlSchema, Show

  final case class ThreeStringsJoined(string1: String, string2: String, string3: String) derives Show
  object ThreeStringsJoined {

    private val reg = "^([^:]*):([^:]*):([^:]*)$".r

    given SqlSchema[ThreeStringsJoined] =
      SqlSchema.string.transformOrFail(
        {
          case reg(a, b, c) => ThreeStringsJoined(a, b, c).asRight
          case _            => "Expected format: _:_:_".asLeft
        },
        joined => s"${joined.string1}:${joined.string2}:${joined.string3}",
      )

  }

  final case class Full(
      ints: TwoInts,
      strings1: ThreeStrings,
      strings2: ThreeStringsJoined,
      optStrings1: Option[ThreeStrings],
      optStrings2: Option[ThreeStringsJoined],
  ) derives SqlTable, Show

  ///////  ///////////////////////////////////////////////////////////////

  private def decode(values: Any*): Unit =
    println(SqlTable[Full].schema.decode(values*).fold(identity, _.show))

  println()
  println("=====| Schema |=====")
  println()
  println(SqlTable[Full].show)
  println()

  println()
  println("=====| Decode |=====")
  println()
  decode(1, 2, "A", "B", "C", "D:E:F", "G", "H", "I", "J:K:L")
  decode(1, 2, "A", "B", "C", "D:E:F", null, null, null, null)
  decode(1, 2, "A", "B", "C", "D:E:F", null, "H", null, null)
  println()

}
