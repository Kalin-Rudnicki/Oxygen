package oxygen.cli

import java.nio.file.Files
import oxygen.predef.test.*
import zio.*

object PathCompletionSpec extends OxygenSpecDefault {

  override def testSpec: TestSpec =
    suite("PathCompletionSpec")(
      test("parentAndPrefix splits a partial file name") {
        assertTrue(
          PathCompletion.parentAndPrefix("prod/ap") == ("prod/", "ap"),
          PathCompletion.parentAndPrefix("prod/") == ("prod/", ""),
          PathCompletion.parentAndPrefix("app") == ("", "app"),
        )
      },
      test("buildCompletions appends / for directories") {
        val out =
          PathCompletion.buildCompletions(
            parent = "prod/",
            prefix = "a",
            entries = Seq("app.json", "archive"),
            isDirectory = _ == "archive",
          )
        assertTrue(out == Seq("prod/app.json", "prod/archive/"))
      },
      test("expandSingleMatch drills into a uniquely matching directory") {
        val out =
          PathCompletion.expandSingleMatch(
            completions = Seq("dev/"),
            listAt = {
              case "dev/" => Seq("dev/app.json", "dev/notes.txt")
              case _      => Nil
            },
          )
        assertTrue(out == Seq("dev/app.json", "dev/notes.txt"))
      },
      test("expandSingleMatch drills recursively while matches stay unique") {
        val out =
          PathCompletion.expandSingleMatch(
            completions = Seq("prod/"),
            listAt = {
              case "prod/"         => Seq("prod/archive/")
              case "prod/archive/" => Seq("prod/archive/legacy.json")
              case _               => Nil
            },
          )
        assertTrue(out == Seq("prod/archive/legacy.json"))
      },
      test("expandSingleMatch keeps an empty uniquely matching directory") {
        val out =
          PathCompletion.expandSingleMatch(
            completions = Seq("empty/"),
            listAt = _ => Nil,
          )
        assertTrue(out == Seq("empty/"))
      },
      test("RelativePath.under delegates to underBasePath") {
        ZIO.acquireReleaseWith {
          ZIO.attempt {
            val base = Files.createTempDirectory("oxygen-relative-path")
            Files.createFile(base.resolve("alpha.txt"))
            base.toString
          }
        } { basePath =>
          ZIO.attempt(Files.walk(java.nio.file.Paths.get(basePath)).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete(_))).orDie.unit
        } { basePath =>
          RelativePath.under(basePath).completionOptions("al").map { out =>
            assertTrue(out == Seq(RelativePath("alpha.txt")))
          }
        }
      },
      test("underBasePath lists entries under a base directory") {
        ZIO.acquireReleaseWith {
          ZIO.attempt {
            val base = Files.createTempDirectory("oxygen-path-completion")
            val prod = Files.createDirectory(base.resolve("prod"))
            Files.createFile(prod.resolve("app.json"))
            Files.createDirectory(prod.resolve("archive"))
            base.toString
          }
        } { basePath =>
          ZIO.attempt(Files.walk(java.nio.file.Paths.get(basePath)).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete(_))).orDie.unit
        } { basePath =>
          CompletionOptions.underBasePath(basePath).completionOptions("prod/a").map { out =>
            assertTrue(out.contains(s"prod/app.json"), out.contains(s"prod/archive/"))
          }
        }
      },
      test("underBasePath drills into a uniquely matching directory prefix") {
        ZIO.acquireReleaseWith {
          ZIO.attempt {
            val base = Files.createTempDirectory("oxygen-path-drill")
            val dev = Files.createDirectory(base.resolve("dev"))
            Files.createFile(dev.resolve("app.json"))
            Files.createFile(dev.resolve("notes.txt"))
            base.toString
          }
        } { basePath =>
          ZIO.attempt(Files.walk(java.nio.file.Paths.get(basePath)).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete(_))).orDie.unit
        } { basePath =>
          CompletionOptions.underBasePath(basePath).completionOptions("de").map { out =>
            assertTrue(out == Seq("dev/app.json", "dev/notes.txt"))
          }
        }
      },
    )

}
