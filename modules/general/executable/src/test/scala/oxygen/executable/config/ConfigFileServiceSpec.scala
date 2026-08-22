package oxygen.executable.config

import java.nio.file.Files
import oxygen.json.JsonCodec
import oxygen.predef.test.*
import oxygen.zio.system.Path

object ConfigFileServiceSpec extends OxygenSpecDefault {

  final case class AppConfig(name: String, port: Int) derives JsonCodec

  /** A fresh, real temp directory (via `FileSystem.current`) for each test. */
  private val tempDir: UIO[Path] =
    ZIO.attempt(Files.createTempDirectory("oxygen-config-file-spec")).orDie
      .flatMap(jpath => Path.of(jpath.toString).orDie)

  override def testSpec: TestSpec =
    suite("ConfigFileServiceSpec")(
      suite("round-trip")(
        test("json save -> load returns the original value") {
          val cfg = AppConfig("svc", 8080)
          for {
            dir <- tempDir
            file = dir.resolve("local.json")
            _ <- ConfigFileService.save(file, cfg)
            loaded <- ConfigFileService.load[AppConfig](file)
          } yield assertTrue(loaded == cfg)
        },
        test("yaml save -> load returns the original value") {
          val cfg = AppConfig("svc", 9090)
          for {
            dir <- tempDir
            file = dir.resolve("global.yaml")
            _ <- ConfigFileService.save(file, cfg)
            loaded <- ConfigFileService.load[AppConfig](file)
          } yield assertTrue(loaded == cfg)
        },
        test("save overwrites an existing file (second write wins)") {
          val first = AppConfig("first", 1)
          val second = AppConfig("second", 2)
          for {
            dir <- tempDir
            file = dir.resolve("local.json")
            _ <- ConfigFileService.save(file, first)
            _ <- ConfigFileService.save(file, second)
            loaded <- ConfigFileService.load[AppConfig](file)
          } yield assertTrue(loaded == second)
        },
        test("save creates missing parent directories") {
          val cfg = AppConfig("nested", 1)
          for {
            dir <- tempDir
            file = dir.resolve("a").resolve("b").resolve("c.json")
            _ <- ConfigFileService.save(file, cfg)
            loaded <- ConfigFileService.load[AppConfig](file)
          } yield assertTrue(loaded == cfg)
        },
      ),
      suite("mergeDirectory")(
        test("merges every supported file, later (sorted) files win on key conflicts") {
          for {
            dir <- tempDir
            _ <- dir.resolve("01-base.json").write("""{"name":"A","port":1}""").orDie
            _ <- dir.resolve("02-override.json").write("""{"name":"B"}""").orDie
            merged <- ConfigFileService.mergeDirectory[AppConfig](dir)
          } yield assertTrue(merged == AppConfig("B", 1))
        },
        test("merges across json + yaml files") {
          for {
            dir <- tempDir
            _ <- dir.resolve("01.json").write("""{"name":"json"}""").orDie
            _ <- dir.resolve("02.yaml").write("port: 42\n").orDie
            merged <- ConfigFileService.mergeDirectory[AppConfig](dir)
          } yield assertTrue(merged == AppConfig("json", 42))
        },
        test("empty directory fails with EmptyDirectory") {
          for {
            dir <- tempDir
            res <- ConfigFileService.mergeDirectory[AppConfig](dir).either
          } yield assertTrue(res.left.toOption.exists(_.isInstanceOf[ConfigFileError.EmptyDirectory]))
        },
      ),
      suite("loadResolved (the @envConfig semantics)")(
        test("resolves a single file") {
          val cfg = AppConfig("file", 7)
          for {
            dir <- tempDir
            file = dir.resolve("cfg.json")
            _ <- ConfigFileService.save(file, cfg)
            loaded <- ConfigFileService.loadResolved[AppConfig](file)
          } yield assertTrue(loaded == cfg)
        },
        test("resolves a directory by merging") {
          for {
            dir <- tempDir
            _ <- dir.resolve("a.json").write("""{"name":"A","port":1}""").orDie
            loaded <- ConfigFileService.loadResolved[AppConfig](dir)
          } yield assertTrue(loaded == AppConfig("A", 1))
        },
        test("missing path fails with PathDoesNotExist") {
          for {
            dir <- tempDir
            missing = dir.resolve("does-not-exist.json")
            res <- ConfigFileService.loadResolved[AppConfig](missing).either
          } yield assertTrue(res.left.toOption.exists(_.isInstanceOf[ConfigFileError.PathDoesNotExist]))
        },
      ),
      suite("list / exists")(
        test("list returns only supported files, sorted by path") {
          for {
            dir <- tempDir
            _ <- ConfigFileService.save(dir.resolve("b.yaml"), AppConfig("b", 2))
            _ <- ConfigFileService.save(dir.resolve("a.json"), AppConfig("a", 1))
            _ <- dir.resolve("notes.txt").write("ignored").orDie
            files <- ConfigFileService.list(dir)
          } yield assertTrue(files.map(_.fileName.name) == Chunk("a.json", "b.yaml"))
        },
        test("exists reflects presence") {
          for {
            dir <- tempDir
            file = dir.resolve("c.json")
            before <- ConfigFileService.exists(file)
            _ <- ConfigFileService.save(file, AppConfig("c", 3))
            after <- ConfigFileService.exists(file)
          } yield assertTrue(!before, after)
        },
      ),
      suite("errors")(
        test("save to an unsupported extension fails with UnsupportedExtension") {
          for {
            dir <- tempDir
            res <- ConfigFileService.save(dir.resolve("config.txt"), AppConfig("x", 1)).either
          } yield assertTrue(res.left.toOption.exists(_.isInstanceOf[ConfigFileError.UnsupportedExtension]))
        },
        test("load of an unsupported extension fails with UnsupportedExtension") {
          for {
            dir <- tempDir
            _ <- dir.resolve("config.txt").write("whatever").orDie
            res <- ConfigFileService.load[AppConfig](dir.resolve("config.txt")).either
          } yield assertTrue(res.left.toOption.exists(_.isInstanceOf[ConfigFileError.UnsupportedExtension]))
        },
        test("load of malformed json fails with JsonDecodeFailure") {
          for {
            dir <- tempDir
            file = dir.resolve("bad.json")
            _ <- file.write("""{"name":"x"}""").orDie // missing required `port`
            res <- ConfigFileService.load[AppConfig](file).either
          } yield assertTrue(res.left.toOption.exists(_.isInstanceOf[ConfigFileError.JsonDecodeFailure]))
        },
      ),
    ).provide(ConfigFileService.test)

}
