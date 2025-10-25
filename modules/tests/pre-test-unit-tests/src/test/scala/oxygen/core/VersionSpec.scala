package oxygen.core

import oxygen.predef.test.*
import scala.Ordering.Implicits.infixOrderingOps

object VersionSpec extends OxygenSpecDefault {

  private def parseSpec(in: String)(exp: Version)(using Trace, SourceLocation): TestSpec =
    test(in) {
      val res = Version.parse(in).getOrElse(throw new RuntimeException("invalid version"))
      assertTrue(
        res == exp,
        res.toString == in,
      )
    }

  override def testSpec: TestSpec =
    suite("VersionSpec")(
      suite("parse")(
        parseSpec("1.2.3")(Version(false, NonEmptyList.of(1, 2, 3), None)),
        parseSpec("v1.2.3")(Version(true, NonEmptyList.of(1, 2, 3), None)),
        parseSpec("v1.2.3-SNAPSHOT")(Version(true, NonEmptyList.of(1, 2, 3), Version.Suffix.Snapshot("-SNAPSHOT").some)),
        parseSpec("1.2.3-RC1")(Version(false, NonEmptyList.of(1, 2, 3), Version.Suffix.ReleaseCandidate(1).some)),
        parseSpec("1.2.3-what")(Version(false, NonEmptyList.of(1, 2, 3), Version.Suffix.Other("-what").some)),
      ),
      test("ordering") {
        val v1 = Version("1")
        val v2 = Version("v1.0")
        val v3 = Version("v1.1")
        val v4 = Version("v1.1.0")
        val v5 = Version("v1.1.1")
        val v6 = Version("v1.1.1-RC2")
        val v7 = Version("v1.1.1-SNAPSHOT")
        val v8 = Version("v1.1.1-what")
        val v9 = Version("2.3.4")

        assertTrue(
          // id
          v1 == v1,
          v2 == v2,
          v3 == v3,
          v4 == v4,
          v5 == v5,
          v6 == v6,
          v7 == v7,
          v8 == v8,
          v9 == v9,
          // other
          v1 == v2,
          v1 < v3,
          v3 == v4,
          v3 < v5,
          v5 > v6,
          v5 > v7,
          v5 > v8,
          v6 > v7,
          v6 > v8,
          v7 < v8,
          v5 < v9,
          v8 < v9,
        )
      },
    )

}
