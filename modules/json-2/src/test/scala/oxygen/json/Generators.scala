package oxygen.json

import zio.test.Gen

object Generators {

  object strings {

    type StringGen[T] = Gen[Any, (T, String)]

    private val stringElem: StringGen[Char] =
      Gen.weighted(
        Gen.alphaNumericChar.map(c => (c, c.toString)) -> 9.0,
        Gen.elements(
          '\n' -> "\\n",
          '\t' -> "\\t",
          '\\' -> "\\\\",
          '"' -> "\\\"",
        ) -> 1.0,
      )

    private val whiteSpaceGen: Gen[Any, String] =
      Gen.chunkOfBounded(0, 10)(Gen.elements(' ', '\t', '\n')).map(_.mkString)

    val stringGen: StringGen[Json.Str] =
      Gen.chunkOfBounded(0, 10)(stringElem).map { elems =>
        (
          Json.Str(elems.map(_._1).mkString),
          elems.map(_._2).mkString("\"", "", "\""),
        )
      }

    val numberWithoutDecimalGen: StringGen[Json.NumberWithoutDecimal] =
      Gen.int.map { int => (Json.NumberWithoutDecimal(BigInt(int)), int.toString) }

    val numberWithDecimalGen: StringGen[Json.NumberWithDecimal] =
      Gen.double.map { double => (Json.NumberWithDecimal(BigDecimal(double)), double.toString) }

    val booleanGen: StringGen[Json.Bool] =
      Gen.boolean.map { boolean => (Json.boolean(boolean), boolean.toString) }

    val nullGen: StringGen[Json.Null.type] =
      Gen.const((Json.Null, "null"))

    def arrayGen(maxDepth: Int): StringGen[Json.Arr] =
      if (maxDepth > 0)
        for {
          afterOpen <- whiteSpaceGen
          beforeClose <- whiteSpaceGen
          elems <- Gen.chunkOfBounded(0, 5) { Gen.int(0, maxDepth - 1).flatMap(anyJsonGen) }
        } yield (
          Json.arr(elems.map(_._1)*),
          s"[$afterOpen${elems.map(_._2).mkString(", ")}$beforeClose]",
        )
      else
        whiteSpaceGen.map { whiteSpace => (Json.arr(), s"[$whiteSpace]") }

    private def objectPairGen(maxDepth: Int): StringGen[(String, Json)] =
      for {
        a <- whiteSpaceGen
        b <- whiteSpaceGen
        c <- whiteSpaceGen
        d <- whiteSpaceGen
        key <- stringGen
        value <- Gen.int(0, maxDepth - 1).flatMap(anyJsonGen)
      } yield (
        (key._1.value, value._1),
        s"$a${key._2}$b:$c${value._2}$d",
      )

    def objectGen(maxDepth: Int): StringGen[Json.Obj] =
      if (maxDepth > 0)
        for {
          afterOpen <- whiteSpaceGen
          beforeClose <- whiteSpaceGen
          elems <- Gen.chunkOfBounded(0, 5) { objectPairGen(maxDepth) }
        } yield (
          Json.obj(elems.map(_._1)*),
          s"{$afterOpen${elems.map(_._2).mkString(", ")}$beforeClose}",
        )
      else
        whiteSpaceGen.map { whiteSpace => (Json.obj(), s"{$whiteSpace}") }

    private def anyJsonNoWhitespaceGen(maxDepth: Int): StringGen[Json] =
      Gen.oneOf(
        stringGen,
        numberWithoutDecimalGen,
        numberWithDecimalGen,
        booleanGen,
        nullGen,
        arrayGen(maxDepth),
        objectGen(maxDepth),
      )

    def anyJsonGen(maxDepth: Int): StringGen[Json] =
      Gen.boolean.flatMap {
        case true =>
          for {
            before <- whiteSpaceGen
            after <- whiteSpaceGen
            json <- anyJsonNoWhitespaceGen(maxDepth)
          } yield (json._1, s"$before${json._2}$after")
        case false =>
          anyJsonNoWhitespaceGen(maxDepth)
      }

  }

}
