package oxygen.core

import oxygen.predef.test.*

object ShowSpec extends OxygenSpecDefault {

  final case class ISB(i: Int, s: String, b: Boolean) derives Show.ToString

  final case class PersonId(id: Int)
  object PersonId {
    given Show[PersonId] = pid => s"[[${pid.id}]]"
  }

  final case class Person(id: PersonId, first: String, last: String, age: Int) derives Show

  private val pid1 = PersonId(1)
  private val pid2 = PersonId(2)
  private val person1 = Person(pid1, "F1", "L1", 1)

  override def testSpec: TestSpec =
    suite("ShowSpec")(
      test(".show extension") {
        assertTrue(
          pid1.show == "[[1]]",
          pid2.show == "[[2]]",
          person1.show == """Person(id = [[1]], first = "F1", last = "L1", age = 1)""",
        )
      },
      test("`sh` interpolator") {
        assertTrue(
          sh"" == "",
          sh"A${"B"}C" == "ABC",
          sh" A${"B".toShown}C " == """ A"B"C """,
          sh"1${2}3" == "123",
          sh"<$pid1>" == "<[[1]]>",
          sh"error: $person1 is not friends with $pid2" == """error: Person(id = [[1]], first = "F1", last = "L1", age = 1) is not friends with [[2]]""",
        )
      },
      test("derives Show.ToString") {
        assertTrue(
          ISB(1, "B", true).show == "ISB(1,B,true)",
        )
      },
    )

}
