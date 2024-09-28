package oxygen.json

import oxygen.predef.test.*

object EncodedThrowableSpec extends OxygenSpecDefault {

  private def fromThrowableTest(throwable: Throwable, exp: EncodedThrowable)(implicit loc: SourceLocation): TestSpec =
    test(exp.simpleMessageWithCause.unesc) {
      assert(EncodedThrowable.fromThrowable(throwable))(equalTo_unfilteredDiff(exp) { case tt: TypeTag[?] => tt.prefixAll })
    }

  override def testSpec: TestSpec =
    suite("EncodedThrowableSpec")(
      fromThrowableTest(
        new RuntimeException,
        EncodedThrowable(TypeTag[RuntimeException], None, None),
      ),
      fromThrowableTest(
        new RuntimeException("error"),
        EncodedThrowable(TypeTag[RuntimeException], "error".some, None),
      ),
      fromThrowableTest(
        new RuntimeException("error", new RuntimeException("error2")),
        EncodedThrowable(TypeTag[RuntimeException], "error".some, EncodedThrowable(TypeTag[RuntimeException], "error2".some, None).some),
      ),
      fromThrowableTest(
        new RuntimeException("error", new RuntimeException),
        EncodedThrowable(TypeTag[RuntimeException], "error".some, EncodedThrowable(TypeTag[RuntimeException], None, None).some),
      ),
    )

}
