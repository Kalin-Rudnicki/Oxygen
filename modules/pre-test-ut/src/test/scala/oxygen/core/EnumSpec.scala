package oxygen.core

import oxygen.predef.test.*

object EnumSpec extends OxygenSpecDefault {

  enum Enum1 extends Enum[Enum1] { case A, B, C }
  object Enum1 extends Enum.Companion[Enum1] {
    override protected val defaultToString: Enum1 => NonEmptyList[String] =
      e => NonEmptyList.of(e.toString, e.toString * 2)
  }

  sealed trait Enum2 extends Enum[Enum2]
  object Enum2 extends Enum.Companion[Enum2] {

    case object CaseA extends Enum2
    case object CaseB extends Enum2
    case object CaseC extends Enum2

    override protected val defaultToString: Enum2 => NonEmptyList[String] = e => NonEmptyList.one(e.toString.camelToSnake.snakeToDash)

    override def values: Array[Enum2] =
      Array(CaseA, CaseB, CaseC)

  }

  private def decodeSpec[E <: Enum[E]](string: String, value: E)(implicit hec: Enum.HasCompanion[E], tt: TypeTag[E]): TestSpec =
    test(s"${tt.prefixObject} / ${string.unesc} / $value") {
      assert(hec.companion.stringCodec.decoder.decodeError(string))(isRight(equalTo(value)))
    }

  override def testSpec: TestSpec =
    suite("EnumSpec")(
      decodeSpec("A", Enum1.A),
      decodeSpec("a", Enum1.A),
      decodeSpec("Aa", Enum1.A),
      decodeSpec("case-a", Enum2.CaseA),
      decodeSpec("CASE-B", Enum2.CaseB),
    )

}
