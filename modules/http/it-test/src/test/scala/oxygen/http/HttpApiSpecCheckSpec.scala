package oxygen.http

import oxygen.http.schema.compat.HttpApiSpecCheck
import oxygen.http.schema.compat.HttpApiSpecCheck.{Config, Outcome}
import oxygen.predef.test.*
import oxygen.zio.system.Path
import scala.annotation.experimental
import zio.*

/**
  * End-to-end tests of the persist-and-diff harness over throwaway temp files, exercising the four
  * outcomes: genesis/stale -> `PendingUpdate`, write -> `Wrote`, unchanged -> `UpToDate`, and a breaking
  * change -> `BlockedIncompatible` (unless `allowIncompatible`).
  */
@experimental
object HttpApiSpecCheckSpec extends OxygenSpecDefault {

  import ApiSpecCompatFixtures.*

  private val ci: Config = Config.ci
  private val update: Config = Config(allowUpdate = true, allowIncompatible = false)
  private val updateIncompatible: Config = Config(allowUpdate = true, allowIncompatible = true)

  private def freshPath: UIO[Path] =
    for {
      uuid <- Random.nextUUID
      path <- Path.of(s"target/test-out/api-spec-check/$uuid.json").orDie
    } yield path

  extension (self: IO[HttpApiSpecCheck.HttpApiSpecCheckError, Outcome])
    private def outcome: UIO[Outcome] = self.orDieWith(e => new RuntimeException(e.toString))

  private def isPending(o: Outcome): Boolean = o match
    case _: Outcome.PendingUpdate => true
    case _                        => false
  private def isWrote(o: Outcome): Boolean = o match
    case _: Outcome.Wrote => true
    case _                => false
  private def isBlocked(o: Outcome): Boolean = o match
    case _: Outcome.BlockedIncompatible => true
    case _                              => false

  override def testSpec: TestSpec =
    suite("HttpApiSpecCheckSpec")(
      test("a missing committed file is a PendingUpdate under CI") {
        for {
          path <- freshPath
          o <- HttpApiSpecCheck.check(path, reqThingV1, ci).outcome
        } yield assertTrue(isPending(o))
      },
      test("allowUpdate writes the genesis file, which then reads back as UpToDate") {
        for {
          path <- freshPath
          wrote <- HttpApiSpecCheck.check(path, reqThingV1, update).outcome
          exists <- path.exists.orDie
          again <- HttpApiSpecCheck.check(path, reqThingV1, ci).outcome
        } yield assertTrue(isWrote(wrote), exists, again == Outcome.UpToDate)
      },
      test("a compatible change is PendingUpdate under CI and Wrote under allowUpdate") {
        for {
          path <- freshPath
          _ <- HttpApiSpecCheck.check(path, reqThingV1, update).outcome
          pending <- HttpApiSpecCheck.check(path, reqThingV2Opt, ci).outcome
          wrote <- HttpApiSpecCheck.check(path, reqThingV2Opt, update).outcome
        } yield assertTrue(isPending(pending), isWrote(wrote))
      },
      test("a breaking change is BlockedIncompatible unless allowIncompatible is set") {
        for {
          path <- freshPath
          _ <- HttpApiSpecCheck.check(path, reqThingV1, update).outcome
          blocked <- HttpApiSpecCheck.check(path, reqThingV2Req, update).outcome
          forced <- HttpApiSpecCheck.check(path, reqThingV2Req, updateIncompatible).outcome
        } yield assertTrue(isBlocked(blocked), isWrote(forced))
      },
    ) @@ TestAspect.withLiveRandom // freshPath needs real UUIDs so temp files don't collide

}
