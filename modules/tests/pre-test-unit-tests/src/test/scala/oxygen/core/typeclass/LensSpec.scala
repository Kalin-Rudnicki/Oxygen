package oxygen.core.typeclass

import oxygen.core.syntax.extra.*
import oxygen.predef.test.*

object LensSpec extends OxygenSpecDefault {

  final case class Person(
      first: String,
      last: String,
      age: Int,
  )

  final case class ContactInfo(
      person: Person,
      phone: String,
      email: String,
  )

  val surround: String => String = s => s"_${s}_"

  override def testSpec: TestSpec =
    suite("LensSpec")(
      test("seq lens") {
        val seq: Seq[String] = Seq("A", "B", "C")

        val _0: PartialLens.Applied[Seq[String], String] = PartialLens.seq(0)(seq)
        val _1: PartialLens.Applied[Seq[String], String] = PartialLens.seq(1)(seq)
        val _2: PartialLens.Applied[Seq[String], String] = PartialLens.seq(2)(seq)
        val _3: PartialLens.Applied[Seq[String], String] = PartialLens.seq(3)(seq)

        assertTrue(
          _0.getOption.contains("A"),
          _1.getOption.contains("B"),
          _2.getOption.contains("C"),
          _3.getOption.isEmpty,
          _0.replaceOption("_").contains(Seq("_", "B", "C")),
          _1.replaceOption("_").contains(Seq("A", "_", "C")),
          _2.replaceOption("_").contains(Seq("A", "B", "_")),
          _3.replaceOption("_").isEmpty,
          _0.modifyOption(surround).contains(Seq("_A_", "B", "C")),
          _1.modifyOption(surround).contains(Seq("A", "_B_", "C")),
          _2.modifyOption(surround).contains(Seq("A", "B", "_C_")),
          _3.modifyOption(surround).isEmpty,
        )
      },
      /*
      test("person lens") {
        val person: Person = Person("F", "L", 100)

        assertTrue(
          person.focus(_.first).get == "F",
          person.focus(_.last).get == "L",
          person.focus(_.age).get == 100,
          person.focus(_.first).replace("F2") == Person("F2", "L", 100),
          person.focus(_.last).replace("L2") == Person("F", "L2", 100),
          person.focus(_.age).replace(101) == Person("F", "L", 101),
          person.focus(_.first).modify(surround) == Person("_F_", "L", 100),
          person.focus(_.last).modify(surround) == Person("F", "_L_", 100),
          person.focus(_.age).modify(_ / 2) == Person("F", "L", 50),
        )
      },
       */
      test("person lens") {
        val contact: ContactInfo = ContactInfo(Person("F", "L", 100), "123", "a@b.c")

        assertTrue(
          contact.focus(_.person.first).get == "F",
          /*
          contact.focus(_.person.last).get == "L",
          contact.focus(_.person.age).get == 100,
          contact.focus(_.person.first).replace("F2") == Person("F2", "L", 100),
          contact.focus(_.person.last).replace("L2") == Person("F", "L2", 100),
          contact.focus(_.person.age).replace(101) == Person("F", "L", 101),
          contact.focus(_.person.first).modify(surround) == Person("_F_", "L", 100),
          contact.focus(_.person.last).modify(surround) == Person("F", "_L_", 100),
          contact.focus(_.person.age).modify(_ / 2) == Person("F", "L", 50),
           */
        )
      },
    )

}
