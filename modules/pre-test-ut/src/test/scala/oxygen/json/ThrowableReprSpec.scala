package oxygen.json

import oxygen.predef.test.*

object ThrowableReprSpec extends OxygenSpecDefault {

  private def fromThrowableTest(throwable: Throwable, exp: ThrowableRepr)(implicit loc: SourceLocation): TestSpec =
    test(exp.simpleMessageWithCause.unesc) {
      assert(ThrowableRepr.fromThrowable(throwable))(equalTo_unfilteredDiff(exp) { case tt: TypeTag[?] => tt.prefixAll })
    }

  override def testSpec: TestSpec =
    suite("EncodedThrowableSpec")(
      fromThrowableTest(
        new RuntimeException,
        ThrowableRepr(TypeTag[RuntimeException], None, None),
      ),
      fromThrowableTest(
        new RuntimeException("error"),
        ThrowableRepr(TypeTag[RuntimeException], "error".some, None),
      ),
      fromThrowableTest(
        new RuntimeException("error", new RuntimeException("error2")),
        ThrowableRepr(TypeTag[RuntimeException], "error".some, ThrowableRepr(TypeTag[RuntimeException], "error2".some, None).some),
      ),
      fromThrowableTest(
        new RuntimeException("error", new RuntimeException),
        ThrowableRepr(TypeTag[RuntimeException], "error".some, ThrowableRepr(TypeTag[RuntimeException], None, None).some),
      ),
    )

}
