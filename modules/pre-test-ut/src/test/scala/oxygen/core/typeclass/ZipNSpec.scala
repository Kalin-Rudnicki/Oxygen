package oxygen.core.typeclass

import oxygen.predef.test.*

object ZipNSpec extends OxygenSpecDefault {

  // Zip3

  private def roundTripTest[In1, In2, In3, Out1, Out2](
      in1: In1,
      in2: In2,
      in3: In3,
      out: Out1,
  )(implicit
      zip: Zip3.Out[In1, In2, In3, Out2],
      ev: Out1 =:= Out2,
      loc: SourceLocation,
  ): TestSpec =
    test(s"($in1, $in2, $in3) <-> $out") {
      assertTrue(
        zip.zip(in1, in2, in3) == out,
        zip.unzip(ev(out)) == (in1, in2, in3),
      )
    }

  // Zip4

  private def roundTripTest[In1, In2, In3, In4, Out1, Out2](
      in1: In1,
      in2: In2,
      in3: In3,
      in4: In4,
      out: Out1,
  )(implicit
      zip: Zip4.Out[In1, In2, In3, In4, Out2],
      ev: Out1 =:= Out2,
      loc: SourceLocation,
  ): TestSpec =
    test(s"($in1, $in2, $in3, $in4) <-> $out") {
      assertTrue(
        zip.zip(in1, in2, in3, in4) == out,
        zip.unzip(ev(out)) == (in1, in2, in3, in4),
      )
    }

  // Zip5

  private def roundTripTest[In1, In2, In3, In4, In5, Out1, Out2](
      in1: In1,
      in2: In2,
      in3: In3,
      in4: In4,
      in5: In5,
      out: Out1,
  )(implicit
      zip: Zip5.Out[In1, In2, In3, In4, In5, Out2],
      ev: Out1 =:= Out2,
      loc: SourceLocation,
  ): TestSpec =
    test(s"($in1, $in2, $in3, $in4, $in5) <-> $out") {
      assertTrue(
        zip.zip(in1, in2, in3, in4, in5) == out,
        zip.unzip(ev(out)) == (in1, in2, in3, in4, in5),
      )
    }

  // Potentially test all the way up until Zip10, but testing 3-5 is sufficient for now in order to prove the concept works
  override def testSpec: TestSpec =
    suite("ZipNSpec")(
      suite("Zip3")(
        roundTripTest((), (), (), ()),
        roundTripTest(1, (), (), 1),
        roundTripTest((), (), 1, 1),
        roundTripTest(1, 2, (), (1, 2)),
        roundTripTest(1, (), 2, (1, 2)),
        roundTripTest(1, 2, 3, (1, 2, 3)),
        roundTripTest((1, 2), (3, 4), (5, 6), (1, 2, 3, 4, 5, 6)),
      ),
      suite("Zip4")(
        roundTripTest((), (), (), (), ()),
        roundTripTest(1, (), (), (), 1),
        roundTripTest((), (), (), 1, 1),
        roundTripTest(1, 2, (), (), (1, 2)),
        roundTripTest(1, (), (), 2, (1, 2)),
        roundTripTest(1, 2, 3, 4, (1, 2, 3, 4)),
        roundTripTest((1, 2), (3, 4), (5, 6), (7, 8), (1, 2, 3, 4, 5, 6, 7, 8)),
      ),
      suite("Zip5")(
        roundTripTest((), (), (), (), (), ()),
        roundTripTest(1, (), (), (), (), 1),
        roundTripTest((), (), (), (), 1, 1),
        roundTripTest(1, 2, (), (), (), (1, 2)),
        roundTripTest(1, (), (), (), 2, (1, 2)),
        roundTripTest(1, 2, 3, 4, 5, (1, 2, 3, 4, 5)),
        roundTripTest((1, 2), (3, 4), (5, 6), (7, 8), (9, 10), (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)),
      ),
    )

}
