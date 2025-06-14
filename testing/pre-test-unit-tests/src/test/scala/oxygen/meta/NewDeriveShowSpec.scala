package oxygen.meta

import oxygen.core.typeclass.Show
import oxygen.predef.test.*

object NewDeriveShowSpec extends OxygenSpecDefault {

  final case class CaseClass0[A](
      a: A,
      b: Option[String],
  )
  object CaseClass0 {
    given show: [A: Show] => Show[CaseClass0[A]] = Show.derived
  }

  summon[Show[CaseClass0[Double]]]

  final case class CaseClass1(
      a: Int,
      b: Option[String],
  ) derives Show

  final case class CaseClass2(
      @Show.annotation.obfuscate.map('*') a: Int,
      @Show.annotation.hide b: String,
  ) derives Show

  final case class AnyVal1(value: String) extends AnyVal derives Show

  case object CaseObject1 {
    given Show[CaseObject1.type] = Show.derived
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
        showSpec("simple")(CaseClass2(56, "heyo"))("CaseClass2(a = **)"),
      ),
      suite("AnyVal1")(
        showSpec("simple")(AnyVal1("heyo"))("\"heyo\""),
      ),
      suite("CaseObject1")(
        showSpec("simple")(CaseObject1)("CaseObject1"),
      ),
    )

}
