package oxygen.cli

import java.nio.file.Files
import oxygen.predef.test.*
import oxygen.schema.PlainTextSchema
import zio.*

object GlobalPathSpec extends OxygenSpecDefault {

  private type TestBase = "/tmp/oxygen-cli-globalpath-test"

  private object inlineSupport {
    inline def complete[Base <: String & Singleton](in: String): Task[Seq[String]] =
      summon[CompletionOptions[GlobalPath[Base]]].completionOptions(in)

    inline def base[Base <: String & Singleton]: String = GlobalPath.base[Base]
  }

  override def testSpec: TestSpec =
    suite("GlobalPathSpec")(
      test("compile-time base drives completion") {
        ZIO.acquireReleaseWith {
          ZIO.attempt {
            val base = java.nio.file.Paths.get(inlineSupport.base[TestBase])
            Files.createDirectories(base)
            Files.createFile(base.resolve("alpha.txt"))
            ()
          }
        } { _ =>
          ZIO.attempt {
            Files.walk(java.nio.file.Paths.get(inlineSupport.base[TestBase])).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete(_))
          }.orDie.unit
        } { _ =>
          inlineSupport.complete[TestBase]("al").map { out =>
            assertTrue(out == Seq(GlobalPath[TestBase]("alpha.txt")))
          }
        }
      },
      test("plainTextSchema round-trips") {
        val decoded = summon[PlainTextSchema[GlobalPath[TestBase]]].decode("foo/bar")
        assertTrue(decoded == Right(GlobalPath[TestBase]("foo/bar")))
      },
    )

}
