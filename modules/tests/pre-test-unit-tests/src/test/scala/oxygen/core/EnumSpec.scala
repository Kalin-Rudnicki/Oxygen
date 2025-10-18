package oxygen.core

import oxygen.predef.test.*
import zio.test.TestResult

object EnumSpec extends OxygenSpecDefault {

  enum Enum1 { case A, B, C }
  object Enum1 {
    given strictEnum: StrictEnum[Enum1] = StrictEnum.derive[Enum1](a => NonEmptyList.of(a.toString, a.toString * 2))
  }

  sealed trait Enum2 derives StrictEnum
  object Enum2 {

    given EnumEncoding[Enum2] = EnumEncoding.single(_.toString.camelToSnake.snakeToDash)

    case object CaseA extends Enum2
    case object CaseB extends Enum2
    case object CaseC extends Enum2

  }

  private def strictEnumDecodeSpec[E: StrictEnum as e](string: String, value: E)(using Trace, SourceLocation): TestSpec =
    test(s"${e.typeTag.prefixObject} / ${string.unesc} / $value") {
      assertTrue(
        e.encodeMany(value).exists(_.toLowerCase == string.toLowerCase),
        e.decodeEither(string) == value.asRight,
      )
    }

  private def strictEnumRoundTripSpec[E: StrictEnum as e](value: E)(using Trace, SourceLocation): TestSpec =
    test(s"round trip: $value") {
      val allRes: NonEmptyList[TestResult] =
        for {
          (s1, i) <- e.encodeMany(value).zipWithIndex
          (s2, t) <- NonEmptyList.of(s1 -> "raw", s1.toLowerCase -> "lower", s1.toUpperCase -> "upper")
        } yield assert(e.decodeEither(s2))(isRight(equalTo(value))).label(s"i = $i, t = $t")

      allRes.reduceLeft(_ && _)
    }

  private def enumWithOtherDecodeSpec[E: EnumWithOther as e](string: String, value: E)(using Trace, SourceLocation): TestSpec =
    test(s"${e.typeTag.prefixObject} / ${string.unesc} / $value") {
      assertTrue(
        e.encodeMany(value).exists(_.toLowerCase == string.toLowerCase),
        e.decode(string) == value,
      )
    }

  enum Enum3 derives EnumWithOther {
    case A
    case B
    case C(value: String)
  }
  object Enum3 {
    given EnumEncoding[Enum3] = EnumEncoding.single[Enum3] {
      case C(value) => value
      case a        => a.toString
    }
  }

  override def testSpec: TestSpec =
    suite("EnumSpec")(
      suite("StrictEnum")(
        strictEnumDecodeSpec[Enum1]("A", Enum1.A),
        strictEnumDecodeSpec[Enum1]("a", Enum1.A),
        strictEnumDecodeSpec[Enum1]("Aa", Enum1.A),
        strictEnumDecodeSpec[Enum2]("case-a", Enum2.CaseA),
        strictEnumDecodeSpec[Enum2]("CASE-B", Enum2.CaseB),
        strictEnumDecodeSpec[Enum2]("case-C", Enum2.CaseC),
        strictEnumRoundTripSpec[Enum1](Enum1.A),
        strictEnumRoundTripSpec[Enum1](Enum1.B),
        strictEnumRoundTripSpec[Enum1](Enum1.C),
        strictEnumRoundTripSpec[Enum2](Enum2.CaseA),
        strictEnumRoundTripSpec[Enum2](Enum2.CaseB),
        strictEnumRoundTripSpec[Enum2](Enum2.CaseC),
      ),
      suite("EnumWithOther")(
        enumWithOtherDecodeSpec[Enum3]("A", Enum3.A),
        enumWithOtherDecodeSpec[Enum3]("a", Enum3.A),
        enumWithOtherDecodeSpec[Enum3]("B", Enum3.B),
        enumWithOtherDecodeSpec[Enum3]("b", Enum3.B),
        enumWithOtherDecodeSpec[Enum3]("other-1", Enum3.C("other-1")),
        enumWithOtherDecodeSpec[Enum3]("OTHER-2", Enum3.C("OTHER-2")),
      ),
    )

}
