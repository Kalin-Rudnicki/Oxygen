package oxygen.core.syntax

import oxygen.predef.test.*

object SeqOpsSpec extends OxygenSpecDefault {

  override def testSpec: TestSpec =
    suite("SeqOpsSpec")(
      suite("intersperse")(
        test("empty") {
          assertTrue(
            Seq.empty[Int].intersperse(0) == Seq.empty,
            List.empty[Int].intersperse(0) == List.empty,
            Chunk.empty[Int].intersperse(0) == Chunk.empty,
          )
        },
        test("non-empty") {
          assertTrue(
            Seq(1, 2, 3).intersperse(0) == Seq(1, 0, 2, 0, 3),
            List(1, 2, 3).intersperse(0) == List(1, 0, 2, 0, 3),
            Chunk(1, 2, 3).intersperse(0) == Chunk(1, 0, 2, 0, 3),
          )
        },
      ),
      suite("surround")(
        test("empty") {
          assertTrue(
            Seq.empty[Int].surround(0) == Seq(0, 0),
            List.empty[Int].surround(0) == List(0, 0),
            Chunk.empty[Int].surround(0) == Chunk(0, 0),
          )
        },
        test("non-empty") {
          assertTrue(
            Seq(1, 2, 3).surround(0) == Seq(0, 1, 0, 2, 0, 3, 0),
            List(1, 2, 3).surround(0) == List(0, 1, 0, 2, 0, 3, 0),
            Chunk(1, 2, 3).surround(0) == Chunk(0, 1, 0, 2, 0, 3, 0),
          )
        },
      ),
    )

}
