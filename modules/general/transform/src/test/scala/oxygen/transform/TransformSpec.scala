package oxygen.transform

import oxygen.predef.test.*
import oxygen.transform.transformers.given

object TransformSpec extends OxygenSpecDefault {

  private def makeTest[From, To](name: String)(from: From, to: To)(using Transform[From, To]): TestSpec =
    test(name) {
      assertTrue(
        from.transformInto[To] == to,
      )
    }

  final case class WrappedString(s: String)

  given Transform[WrappedString, String] = _.s

  override def testSpec: TestSpec =
    suite("TransformSpec")(
      suite("givens")(
        makeTest("id - String")("A", "A"),
        makeTest("seq - String")(List("A", "B", "C"), Vector("A", "B", "C")),
        makeTest("seq - Password")(List(WrappedString("A"), WrappedString("B"), WrappedString("C")), Vector("A", "B", "C")),
        makeTest("some - String")("A", "A".some),
      ),
      suite("derived")(
        makeTest("domain.Person -> api.Person")(
          domain.Person("F", "L", domain.Email("email@e.mail"), domain.Password("pass")),
          api.Person("F", "L", "email@e.mail", "pass"),
        ),
        makeTest[domain.SumExample, api.SumExample]("domain.SumExample.A -> api.SumExample.A")(
          domain.SumExample.A("str", true, 5),
          api.SumExample.A("str", true.some),
        ),
        makeTest[domain.SumExample, api.SumExample]("domain.SumExample.B -> api.SumExample.B")(
          domain.SumExample.B(5),
          api.SumExample.B,
        ),
      ),
    )

}
