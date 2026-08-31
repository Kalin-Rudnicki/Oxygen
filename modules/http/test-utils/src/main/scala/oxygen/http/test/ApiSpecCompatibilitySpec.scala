package oxygen.http.test

import oxygen.http.schema.compat.HttpApiSpecCheck
import oxygen.http.schema.compat.HttpApiSpecCheck.{Config, Outcome}
import oxygen.http.schema.compiled.CompiledApiSpec
import oxygen.http.server.EndpointSchema
import oxygen.predef.test.*
import oxygen.zio.system.Path

/**
  * Reusable spec that guards the committed compiled HTTP API spec against the current-code endpoints --
  * the endpoint-schema analogue of `oxygen.sql.test.DbMigrationSpec`. A subclass supplies the committed
  * file path and the current endpoints; the spec fails CI when the committed doc is stale or when the
  * change is incompatible for clients, with an actionable message naming the env vars to re-run with.
  *
  *   - `OXYGEN_HTTP_ALLOW_UPDATE=true` writes the new spec (review + commit it).
  *   - `OXYGEN_HTTP_ALLOW_INCOMPATIBLE=true` additionally permits a breaking change.
  */
abstract class ApiSpecCompatibilitySpec extends OxygenSpecDefault {

  /** Filesystem path of the committed spec JSON (resolved from the working dir). */
  def apiSpecPath: String

  /** The current-code endpoints (usually `summon[DeriveEndpoints[Api]].endpoints.toArraySeq.map(_.schema)`). */
  def currentEndpoints: Seq[EndpointSchema]

  def specName: String = getClass.getSimpleName

  override final def testSpec: TestSpec =
    suite(specName)(
      test("committed api spec is up to date and compatible with the current endpoints") {
        for {
          path <- Path.of(apiSpecPath).orDie
          config <- Config.fromEnv.orDie
          spec = CompiledApiSpec.compileWithoutLineNos(currentEndpoints)
          outcome <- HttpApiSpecCheck.check(path, spec, config).orDieWith(e => new RuntimeException(e.toString))
        } yield outcome match
          case Outcome.UpToDate =>
            assertCompletes
          case Outcome.Wrote(writtenTo) =>
            assertCompletes.label(s"wrote api spec to $writtenTo -- review and commit it")
          case Outcome.PendingUpdate(_) =>
            assertTrue(false).label(
              s"Committed api spec is out of date with the current endpoints. " +
                s"Re-run with ${Config.allowUpdateEnv}=true to update it.",
            )
          case Outcome.BlockedIncompatible(comparison) =>
            assertTrue(false).label(
              s"Api spec change is incompatible (breaking for clients):\n${comparison.describe}\n" +
                s"Re-run with ${Config.allowUpdateEnv}=true and ${Config.allowIncompatibleEnv}=true to allow it.",
            )
      } @@ TestAspect.withLiveEnvironment, // Config.fromEnv must read the real OS env, not zio-test's empty TestSystem
    )

}
