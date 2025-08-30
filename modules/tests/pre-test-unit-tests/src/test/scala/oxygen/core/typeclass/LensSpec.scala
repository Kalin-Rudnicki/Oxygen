package oxygen.core.typeclass

import oxygen.predef.test.*

object LensSpec extends OxygenSpecDefault {

  override def testSpec: TestSpec =
    suite("LensSpec")(
      test("seq lens") {
        val seq: Seq[String] = Seq("A", "B", "C")

        val _0: PartialLens.Applied[Seq[String], String] = PartialLens.seq(0)(seq)
        val _1: PartialLens.Applied[Seq[String], String] = PartialLens.seq(1)(seq)
        val _2: PartialLens.Applied[Seq[String], String] = PartialLens.seq(2)(seq)
        val _3: PartialLens.Applied[Seq[String], String] = PartialLens.seq(3)(seq)

        val surround: String => String = s => s"_${s}_"

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
    )

}
