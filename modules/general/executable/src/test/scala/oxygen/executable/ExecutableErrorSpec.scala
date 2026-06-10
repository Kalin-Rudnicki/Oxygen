package oxygen.executable

import oxygen.predef.test.*
import zio.*

object ExecutableErrorSpec extends OxygenSpecDefault {

  override def testSpec: TestSpec =
    suite("ExecutableErrorSpec")(
      test("messageFromCause uses safeGetMessage for defects") {
        val cause = Cause.die(new RuntimeException("boom"))
        assertTrue(ExecutableError.ExitWith.messageFromCause(cause) == "boom")
      },
      test("messageFromCause uses safeGetMessage for typed failures") {
        val cause = Cause.fail(new IllegalArgumentException("bad arg"))
        assertTrue(ExecutableError.ExitWith.messageFromCause(cause) == "bad arg")
      },
      test("exitCodeFromCause still recognizes ExitWith") {
        val cause = Cause.die(ExecutableError.ExitWith(ExitCode(7)))
        assertTrue(ExecutableError.ExitWith.exitCodeFromCause(cause) == Some(ExitCode(7)))
      },
    )

}
