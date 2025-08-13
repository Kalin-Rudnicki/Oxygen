package oxygen.zio

import oxygen.predef.test.*
import oxygen.zio.syntax.stream.*
import zio.stream.*

object StreamAggregatorSpec extends OxygenSpecDefault {

  private def passingAggTest[In, Out](name: String)(agg: SparseStreamAggregator[In, Out])(ins: In*)(outs: Out*)(using SourceLocation, Trace): TestSpec =
    test(name) {
      val expOutChunk: Chunk[Out] = Chunk.from(outs)
      ZStream.fromIterable(ins).agg(agg).runCollect.map { actOutChunk => assertTrue(actOutChunk == expOutChunk) }
    }

  private def singleSpec: TestSpec = {
    val agg = SparseStreamAggregator.of[Int]

    suite("single")(
      passingAggTest("empty")(agg)()(),
      passingAggTest("non-empty")(agg)(
        1.some,
        2.some,
        3.some,
      )(
        1,
        2,
        3,
      ),
    )
  }

  private def optionalSpec: TestSpec = {
    val agg = SparseStreamAggregator.of[Int].optional

    suite("optional")(
      passingAggTest("empty")(agg)()(),
      passingAggTest("non-empty")(agg)(
        1.some,
        2.some,
        3.some,
      )(
        1.some,
        2.some,
        3.some,
      ),
    )
  }

  private def manySpec: TestSpec = {
    val agg = SparseStreamAggregator.of[Int].many[Seq]

    suite("many")(
      passingAggTest("empty")(agg)()(),
      passingAggTest("non-empty")(agg)(
        1.some,
        2.some,
        3.some,
      )(
        Seq(1, 2, 3),
      ),
    )
  }

  private def aOptBSpec: TestSpec = {
    val agg = SparseStreamAggregator.of[Int] *: SparseStreamAggregator.of[String].optional

    suite("aOptB")(
      passingAggTest("empty")(agg)()(),
      passingAggTest("case-1")(agg)(
        (1.some, None),
        (2.some, None),
        (None, "A".some),
        (3.some, None),
        (None, "B".some),
        (4.some, None),
      )(
        (1, None),
        (2, "A".some),
        (3, "B".some),
        (4, None),
      ),
      passingAggTest("case-2")(agg)(
        (1.some, None),
      )(
        (1, None),
      ),
    )
  }

  private def megaSpec: TestSpec = {
    val agg = SparseStreamAggregator.of[Int] *: SparseStreamAggregator.of[Boolean] *: SparseStreamAggregator.of[String].optional *: SparseStreamAggregator.of[String].many[Seq]

    suite("mega")(
      passingAggTest("empty")(agg)()(),
      passingAggTest("case-1")(agg)(
        (1.some, None, None, None),
        (None, true.some, None, None),
        (2.some, None, None, None),
        (None, true.some, None, None),
        (None, None, "L1".some, None),
        (3.some, None, None, None),
        (None, true.some, None, None),
        (None, None, None, "R1".some),
        (None, None, None, "R2".some),
        (None, None, None, "R3".some),
        (4.some, None, None, None),
        (None, false.some, None, None),
        (None, None, "L2".some, None),
        (None, None, None, "R4".some),
        (None, None, None, "R5".some),
        (None, None, None, "R6".some),
      )(
        (1, true, None, Seq()),
        (2, true, "L1".some, Seq()),
        (3, true, None, Seq("R1", "R2", "R3")),
        (4, false, "L2".some, Seq("R4", "R5", "R6")),
      ),
    )
  }

  override def testSpec: TestSpec =
    suite("StreamAggregatorSpec")(
      singleSpec,
      optionalSpec,
      manySpec,
      aOptBSpec,
      megaSpec,
    )

}
