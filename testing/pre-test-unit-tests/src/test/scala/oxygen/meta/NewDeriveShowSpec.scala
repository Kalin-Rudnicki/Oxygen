package oxygen.meta

import oxygen.core.typeclass.Show
import oxygen.core.typeclass.Show.annotation.*
// import oxygen.meta2.*
import oxygen.predef.test.*

object NewDeriveShowSpec extends OxygenSpecDefault {

  final case class CaseClass1(
      a: Int,
      b: Option[String],
  ) derives Show

  // @K0.annotation.showDerivation[Show]
  @typeName("MyCaseClass")
  final case class CaseClass2(
      @obfuscate.map('*')
      @fieldName("masked")
      a: Int,
      @Show.annotation.hide
      b: String,
  ) derives Show

  // @K0.annotation.showDerivation[Show]
  final case class CaseClass3[A](
      a: A,
      b: Option[String],
  ) derives Show

  final case class AnyVal1(value: String) extends AnyVal derives Show

  case object CaseObject1 {
    given Show[CaseObject1.type] = Show.derived
  }

  enum Enum1 {
    case A1
    case B1(value: CaseClass1)
  }

  sealed trait Enum3 derives Show
  object Enum3 {
    case object A3 extends Enum3
    final case class B3(value: CaseClass1) extends Enum3
  }

  private def showSpec[A: Show as showA](name: String)(value: A)(exp: String)(using SourceLocation): TestSpec =
    test(name) {
      assert(showA.show(value))(equalTo(exp))
    }

  override def testSpec: TestSpec =
    suite("NewDeriveShowSpec")(
      suite("CaseClass1")(
        showSpec("some")(CaseClass1(1, "string".some))("CaseClass1(a = 1, b = \"string\")"),
        showSpec("none")(CaseClass1(1, None))("CaseClass1(a = 1, b = <none>)"),
      ),
      suite("CaseClass2")(
        showSpec("simple")(CaseClass2(56, "heyo"))("MyCaseClass(masked = **)"),
      ),
      suite("CaseClass3")(
        showSpec("string")(CaseClass3("str", None))("CaseClass3(a = \"str\", b = <none>)"),
        showSpec("int")(CaseClass3(56, None))("CaseClass3(a = 56, b = <none>)"),
      ),
      suite("AnyVal1")(
        showSpec("simple")(AnyVal1("heyo"))("\"heyo\""),
      ),
      suite("CaseObject1")(
        showSpec("simple")(CaseObject1)("CaseObject1"),
      ),
      suite("Enum3")(
        showSpec[Enum3]("A")(Enum3.A3)("A3"),
        showSpec[Enum3]("B")(Enum3.B3(CaseClass1(56, "heyo".some)))("B3(value = CaseClass1(a = 56, b = \"heyo\"))"),
      ),
    )

}
