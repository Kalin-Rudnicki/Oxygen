package oxygen.core

import oxygen.predef.test.*
import zio.test.TestResult

object VersionSpec extends OxygenSpecDefault {

  private def parseSpec(in: String)(exp: Version)(using Trace, SourceLocation): TestSpec =
    test(in) {
      val res = Version.unsafeParse(in)
      assertTrue(
        res == exp,
        res.toString == in,
      )
    }

  private def allEqualSpec(versions: Version*)(using Trace, SourceLocation): TestSpec =
    test(versions.head.repr.show(true)) {
      val hashCodes: Set[Int] = versions.map(_.hashCode).toSet
      val notEqual: Seq[(Version, Version)] =
        (for {
          a <- versions
          b <- versions
        } yield (a, b)).filter(_ != _)

      assertTrue(hashCodes.size == 1).label(s"hashCodes = $hashCodes") &&
      assertTrue(notEqual.isEmpty).label(s"notEqual = ${notEqual.mkString(", ")}")
    }

  private def makeOrderingSpec(name: String)(sortedVersions: Version*)(using Trace, SourceLocation): TestSpec =
    test(name) {
      val sortedVersionList: List[Version] = sortedVersions.toList
      val shuffleAndReSort: UIO[TestResult] =
        for {
          shuffled <- Random.RandomLive.shuffle(sortedVersionList)
          reSorted = shuffled.sorted
        } yield assertTrue(reSorted == sortedVersionList)

      shuffleAndReSort.replicateZIO(10).map(_.reduceLeft(_ && _))
    }

  override def testSpec: TestSpec =
    suite("VersionSpec")(
      suite("parse")(
        parseSpec("1.2.3")(Version.make(1, 2, 3)()),
        parseSpec("v1.2.3")(Version.make(1, 2, 3)()),
        parseSpec("v1.2.3-SNAPSHOT")(Version.make(1, 2, 3)(suffix = Version.PreReleaseSuffix.Snapshot)),
        parseSpec("1.2.3-RC1")(Version.make(1, 2, 3)(suffix = Version.PreReleaseSuffix.ReleaseCandidate(1))),
        parseSpec("1.2.3-what")(Version.make(1, 2, 3)(suffix = Version.PreReleaseSuffix.Custom("what"), isSnapshot = false)),
      ),
      suite("all-equal")(
        allEqualSpec(Version("1"), Version("v1"), Version("v1.0"), Version("v1.0.0"), Version("1.0.0.0.0")),
        allEqualSpec(Version("1-alpha"), Version("v1alpha"), Version("v1-alpha-0"), Version("v1.0.0-alpha"), Version("v1.0.0alpha0")),
        allEqualSpec(Version("v1"), Version("v1+0-g0123")),
      ),
      suite("ordering")(
        makeOrderingSpec("ord-test-1")(
          Version("1"),
          Version("1.1"),
          Version("1.2"),
          Version("2.0-alpha"),
          Version("2.0alpha1"),
          Version("2.0alpha2"),
          Version("2.0alpha11"),
          Version("2.0-beta0"),
          Version("2.0-beta-1"),
          Version("2.0-beta2"),
          Version("2.0-beta-11"),
          Version("2.0-m"),
          Version("2.0-m1"),
          Version("2.0-rc"),
          Version("2.0-rc1"),
          Version("2.0-rc-2"),
          Version("2.0rc11"),
          Version("2.0-snapshot"),
          Version("2.0"),
          Version("2.0+1-g123"),
          Version("2.0+2-g456"),
          Version("2.0.1"),
          Version("2.1"),
          Version("2.1.1"),
          Version("2.2"),
          Version("3-rc1"),
          Version("3.0.0"),
          Version("3.0.1"),
        ),
      ),
    )

}
