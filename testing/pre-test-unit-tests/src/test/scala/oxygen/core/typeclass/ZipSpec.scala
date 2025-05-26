package oxygen.core.typeclass

import oxygen.predef.test.*

object ZipSpec extends OxygenSpecDefault {

  private def roundTripTest[In1, In2, Out1, Out2](in1: In1, in2: In2, out: Out1)(implicit
      zip: Zip.Out[In1, In2, Out2],
      ev: Out1 =:= Out2,
      loc: SourceLocation,
  ): TestSpec =
    test(s"($in1, $in2) <-> $out") {
      assertTrue(
        zip.zip(in1, in2) == out,
        zip.unzip(ev(out)) == (in1, in2),
      )
    }

  override def testSpec: TestSpec =
    suite("ZipSpec")(
      roundTripTest((), (), ()),
      roundTripTest((), 1, 1),
      roundTripTest(1, (), 1),
      roundTripTest((1, 2), (3, 4), (1, 2, 3, 4)),
      roundTripTest((1, 2), 3, (1, 2, 3)),
      roundTripTest(1, (2, 3), (1, 2, 3)),
      roundTripTest(1, 2, (1, 2)),
      roundTripTest((1, 2), (), (1, 2)),
      roundTripTest((), (1, 2), (1, 2)),
    )

}
