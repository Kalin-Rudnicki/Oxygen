//

import Dependencies._
import Settings._
import sbtcrossproject.CrossProject

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Settings
//////////////////////////////////////////////////////////////////////////////////////////////////////

ThisBuild / watchBeforeCommand := Watch.clearScreen

enablePlugins(GitVersioning)
git.gitTagToVersionNumber := { tag =>
  if (tag.matches("^\\d+\\..*$")) Some(tag)
  else None
}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Aggregates
//////////////////////////////////////////////////////////////////////////////////////////////////////

lazy val `oxygen-root`: Project =
  project
    .in(file("."))
    .settings(
      nonPublishedProjectSettings,
      name := "oxygen-root",
      description := "oxygen-root",
    )
    .aggregate(
      `oxygen-modules`,
    )

lazy val `oxygen-modules`: Project =
  project
    .in(file("modules"))
    .settings(
      nonPublishedProjectSettings,
      name := "oxygen-modules",
      description := "oxygen-modules",
    )
    .aggregate(
      `oxygen-modules-jvm`,
      `oxygen-modules-js`,
      `oxygen-modules-native`,
    )

lazy val `oxygen-modules-jvm`: Project =
  project
    .in(file("modules/aggregates/.jvm"))
    .settings(
      nonPublishedProjectSettings,
      name := "oxygen-modules-jvm",
      description := "oxygen-modules-jvm",
    )
    .aggregate(
      // General
      `oxygen-core`.jvm,
      `oxygen-cli`.jvm,
      `oxygen-executable`.jvm,
      `oxygen-json`.jvm,
      `oxygen-meta`.jvm,
      `oxygen-quoted`.jvm,
      `oxygen-schema`.jvm,
      `oxygen-transform`.jvm,
      `oxygen-zio`.jvm,

      // sql
      `oxygen-storage`,
      `oxygen-sql`,
      `oxygen-sql-migration`,
      `oxygen-sql-test`,

      // http
      `oxygen-http`.jvm,
      // TODO (KR) : add
      // `oxygen-http-test`.jvm,

      // jwt
      `oxygen-crypto-model`.jvm,
      `oxygen-crypto-service`,

      // Testing
      `oxygen-test`.jvm,
      `oxygen-test-container`,

      // Internal
      `ut`.jvm,
    )

lazy val `oxygen-modules-js`: Project =
  project
    .in(file("modules/aggregates/.js"))
    .settings(
      nonPublishedProjectSettings,
      name := "oxygen-modules-js",
      description := "oxygen-modules-js",
    )
    .aggregate(
      // General
      `oxygen-core`.js,
      `oxygen-cli`.js,
      `oxygen-executable`.js,
      `oxygen-json`.js,
      `oxygen-meta`.js,
      `oxygen-quoted`.js,
      `oxygen-schema`.js,
      `oxygen-transform`.js,
      `oxygen-zio`.js,

      // http
      `oxygen-http`.js,

      // ui
      `oxygen-ui-web`,

      // jwt
      `oxygen-crypto-model`.js,

      // Testing
      `oxygen-test`.js,

      // Internal
      `ut`.js,
    )

lazy val `oxygen-modules-native`: Project =
  project
    .in(file("modules/aggregates/.native"))
    .settings(
      nonPublishedProjectSettings,
      name := "oxygen-modules-native",
      description := "oxygen-modules-native",
    )
    .aggregate(
      // General
      `oxygen-core`.native,
      `oxygen-cli`.native,
      `oxygen-executable`.native,
      `oxygen-json`.native,
      `oxygen-meta`.native,
      `oxygen-quoted`.native,
      `oxygen-schema`.native,
      `oxygen-transform`.native,
      `oxygen-zio`.native,

      // jwt
      `oxygen-crypto-model`.native,

      // Testing
      `oxygen-test`.native,

      // Internal
      `ut`.native,
    )

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      General
//////////////////////////////////////////////////////////////////////////////////////////////////////

lazy val `oxygen-core`: CrossProject =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .crossType(CrossType.Pure)
    .in(file("modules/general/core"))
    .enablePlugins(BuildInfoPlugin)
    .settings(
      publishedProjectSettings,
      name := "oxygen-core",
      description := "Basic utilities beneficial for any scala user.",
      buildInfoKeys := Seq[BuildInfoKey](version),
      buildInfoPackage := "oxygen.core",
    )

lazy val `oxygen-cli`: CrossProject =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .crossType(CrossType.Pure)
    .in(file("modules/general/cli"))
    .settings(
      publishedProjectSettings,
      name := "oxygen-cli",
      description := "Command line parsing for the enlightened.",
    )
    .dependsOn(
      `oxygen-core` % testAndCompile,
      `oxygen-test` % Test,
    )

lazy val `oxygen-executable`: CrossProject =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .crossType(CrossType.Pure)
    .in(file("modules/general/executable"))
    .settings(
      publishedProjectSettings,
      name := "oxygen-executable",
      description := "An entry-point for your ZIO applications that provides seamless integration with the `oxygen` ecosystem.",
    )
    .dependsOn(
      `oxygen-cli` % testAndCompile,
      `oxygen-zio` % testAndCompile,
      `oxygen-test` % Test,
    )

lazy val `oxygen-json`: CrossProject =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .crossType(CrossType.Pure)
    .in(file("modules/general/json"))
    .settings(
      publishedProjectSettings,
      name := "oxygen-json",
      description := "Why not run your own json library... Not enough exist already...",
    )
    .dependsOn(
      `oxygen-meta` % testAndCompile,
    )

lazy val `oxygen-crypto-model`: CrossProject =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .crossType(CrossType.Pure)
    .in(file("modules/crypto/model"))
    .settings(
      publishedProjectSettings,
      name := "oxygen-crypto-model",
      description := "Oxygen Crypto Model",
    )
    .dependsOn(
      `oxygen-json` % testAndCompile,
      `oxygen-test` % Test,
    )

lazy val `oxygen-crypto-service`: Project =
  project
    .in(file("modules/crypto/service"))
    .settings(
      publishedProjectSettings,
      name := "oxygen-crypto-service",
      description := "Oxygen Crypto Service",
      libraryDependencies ++= Seq(
        "org.mindrot" % "jbcrypt" % "0.4",
      ),
    )
    .dependsOn(
      `oxygen-crypto-model`.jvm % testAndCompile,
      `oxygen-zio`.jvm % testAndCompile,
    )

lazy val `oxygen-meta`: CrossProject =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .crossType(CrossType.Pure)
    .in(file("modules/general/meta"))
    .settings(
      publishedProjectSettings,
      name := "oxygen-meta",
      description := "Metaprogramming for scala-3.",
    )
    .dependsOn(
      `oxygen-core` % testAndCompile,
      `oxygen-quoted` % testAndCompile,
    )

lazy val `oxygen-quoted`: CrossProject =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .crossType(CrossType.Pure)
    .in(file("modules/general/quoted"))
    .settings(
      publishedProjectSettings,
      name := "oxygen-quoted",
      description := "Wrapper around scala.quoted.Quotes.reflect.*, exposing the types at a top-level.",
    )

lazy val `oxygen-schema`: CrossProject =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .crossType(CrossType.Pure)
    .in(file("modules/general/schema"))
    .settings(
      publishedProjectSettings,
      name := "oxygen-schema",
      description := "Schemas are like things, but instead of being things, they just talk about things.",
    )
    .dependsOn(
      `oxygen-crypto-model` % testAndCompile,
      `oxygen-json` % testAndCompile,
    )

lazy val `oxygen-storage`: Project =
  project
    .in(file("modules/sql/storage"))
    .settings(
      publishedProjectSettings,
      name := "oxygen-storage",
      description := "Parent library for oxygen-sql that doesn't assume you are using SQL.",
    )
    .dependsOn(
      `oxygen-zio`.jvm % testAndCompile,
      `oxygen-test`.jvm % Test,
    )

lazy val `oxygen-sql`: Project =
  project
    .in(file("modules/sql/core"))
    .settings(
      publishedProjectSettings,
      name := "oxygen-sql",
      description := "SQL library for scala.",
      libraryDependencies ++= Seq(
        postgres.organization % postgres.postgres % postgres.version,
      ),
    )
    .dependsOn(
      `oxygen-storage` % testAndCompile,
    )

lazy val `oxygen-sql-migration`: Project =
  project
    .in(file("modules/sql/migration"))
    .settings(
      publishedProjectSettings,
      name := "oxygen-sql-migration",
      description := "Automatically handle your SQL migrations from within scala!",
    )
    .dependsOn(
      `oxygen-sql` % testAndCompile,
    )

lazy val `oxygen-transform`: CrossProject =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .crossType(CrossType.Pure)
    .in(file("modules/general/transform"))
    .settings(
      publishedProjectSettings,
      name := "oxygen-transform",
      description := "Transform models between api/domain, domain/db, etc",
    )
    .dependsOn(
      `oxygen-meta` % testAndCompile,
      `oxygen-test` % Test,
    )

lazy val `oxygen-http`: CrossProject =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("modules/http/zio"))
    .settings(
      publishedProjectSettings,
      name := "oxygen-http-zio",
      description := "Use macros to turn traits into clients AND servers.",
      libraryDependencies ++= Seq(
        zio.organization %%% zio.http % zio.httpVersion,
      ),
    )
    .dependsOn(
      `oxygen-schema` % testAndCompile,
      `oxygen-zio` % testAndCompile,
      `oxygen-test` % Test,
    )

lazy val `oxygen-ui-web`: Project =
  project
    .in(file("modules/ui/web"))
    .enablePlugins(ScalaJSPlugin)
    .settings(
      publishedProjectSettings,
      name := "oxygen-ui-web",
      description := "Make your web-ui using scala and FP principles!",
      libraryDependencies ++= Seq(
        monocle.organization %%% monocle.core % monocle.version,
        monocle.organization %%% monocle.`macro` % monocle.version,
      ),
    )
    .dependsOn(
      `oxygen-http`.js % testAndCompile,
    )

lazy val `oxygen-zio`: CrossProject =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .crossType(CrossType.Pure)
    .in(file("modules/general/zio"))
    .settings(
      publishedProjectSettings,
      name := "oxygen-zio",
      description := "Opinionated library of basic utilities to integrate with the `zio` ecosystem.",
      libraryDependencies ++= Seq(
        zio.organization %%% zio.zio % zio.coreVersion,
        zio.organization %%% zio.streams % zio.coreVersion,
      ),
    )
    .dependsOn(
      `oxygen-json` % testAndCompile,
    )

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Testing
//////////////////////////////////////////////////////////////////////////////////////////////////////

lazy val `oxygen-test`: CrossProject =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .crossType(CrossType.Pure)
    .in(file("modules/general/test-utils"))
    .settings(
      publishedProjectSettings,
      name := "oxygen-test",
      description := "Thin layer on top of `zio-test` that provides usability benefits within the `oxygen` ecosystem.",
      libraryDependencies ++= Seq(
        zio.organization %%% zio.test % zio.coreVersion,
        zio.organization %%% zio.testSbt % zio.coreVersion,
      ),
    )
    .dependsOn(
      `oxygen-zio` % testAndCompile,
    )

lazy val `oxygen-test-container`: Project =
  project
    .in(file("modules/general/test-container"))
    .settings(
      publishedProjectSettings,
      name := "oxygen-test-container",
      description := "Test containers for ZIO.",
    )
    .dependsOn(
      `oxygen-zio`.jvm % testAndCompile,
      `oxygen-test`.jvm % testAndCompile,
    )

lazy val `oxygen-sql-test`: Project =
  project
    .in(file("modules/sql/test-utils"))
    .settings(
      publishedProjectSettings,
      name := "oxygen-sql-test",
      description := "Test utils for oxygen-sql.",
    )
    .dependsOn(
      `oxygen-test-container` % testAndCompile,
      `oxygen-sql` % testAndCompile,
    )

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Internal
//////////////////////////////////////////////////////////////////////////////////////////////////////

lazy val `it`: Project =
  project
    .in(file("modules/aggregates/it-test"))
    .settings(
      nonPublishedProjectSettings,
      name := "oxygen-it",
      description := "oxygen-it",
    )
    .aggregate(
      `sql-it`,
      `http-it`,
    )

lazy val `sql-it`: Project =
  project
    .in(file("modules/sql/it-test"))
    .settings(
      nonPublishedProjectSettings,
      name := "sql-it",
      description := "sql-it",
      Test / testOptions += Tests.Argument(
        TestFramework("zio.test.sbt.ZTestFramework"),
        "-ignore-tags",
        "performance",
      ),
    )
    .dependsOn(
      `oxygen-sql-migration` % testAndCompile,
      `oxygen-sql-test` % testAndCompile,
    )

lazy val `http-it`: Project =
  project
    .in(file("modules/http/it-test"))
    .settings(
      nonPublishedProjectSettings,
      name := "http-it",
      description := "http-it",
    )
    .dependsOn(
      // TODO (KR) : replace
      // `oxygen-http-test`
      `oxygen-http`.jvm % testAndCompile,
    )

lazy val `ut`: CrossProject =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .crossType(CrossType.Pure)
    .in(file("modules/tests/pre-test-unit-tests"))
    .settings(
      nonPublishedProjectSettings,
      name := "oxygen-core",
      description := "This serves no purpose besides being able to use `oxygen-test` to write tests for sub-projects that `oxygen-test` depends on.",
    )
    .dependsOn(
      `oxygen-schema` % testAndCompile,
      `oxygen-zio` % testAndCompile,
      `oxygen-test` % Test,
    )

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Example Project
//////////////////////////////////////////////////////////////////////////////////////////////////////

lazy val `example`: Project =
  project
    .in(file("example"))
    .settings(
      nonPublishedProjectSettings,
      name := "oxygen-example",
      description := "oxygen-example",
    )
    .aggregate(
      // cross
      `example-core`.jvm,
      `example-core`.js,
      `example-api-models`.jvm,
      `example-api-models`.js,
      `example-api`.jvm,
      `example-api`.js,
      // jvm-only
      `example-domain-models`,
      `example-domain`,
      `example-domain-impl`,
      `example-web-server`,
      // js-only
      `example-ui`,
    )

lazy val `example-core`: CrossProject =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("example/modules/core"))
    .settings(
      nonPublishedProjectSettings,
      scalacOptions += "-experimental",
      name := "oxygen-example-core",
      description := "oxygen-example-core",
    )
    .dependsOn(
      `oxygen-core` % testAndCompile,
    )

lazy val `example-api-models`: CrossProject =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("example/modules/api-models"))
    .settings(
      nonPublishedProjectSettings,
      scalacOptions += "-experimental",
      name := "oxygen-example-api-models",
      description := "oxygen-example-api-models",
    )
    .dependsOn(
      `example-core` % testAndCompile,
      `oxygen-http` % testAndCompile,
    )

lazy val `example-api`: CrossProject =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("example/modules/api"))
    .settings(
      nonPublishedProjectSettings,
      scalacOptions += "-experimental",
      name := "oxygen-example-api",
      description := "oxygen-example-api",
    )
    .dependsOn(
      `example-api-models` % testAndCompile,
    )

lazy val `example-domain-models`: Project =
  project
    .in(file("example/modules/domain-models"))
    .settings(
      nonPublishedProjectSettings,
      scalacOptions += "-experimental",
      name := "oxygen-example-domain-models",
      description := "oxygen-example-domain-models",
    )
    .dependsOn(
      `example-core`.jvm % testAndCompile,
      `oxygen-crypto-model`.jvm % testAndCompile,
    )

lazy val `example-domain`: Project =
  project
    .in(file("example/modules/domain"))
    .settings(
      nonPublishedProjectSettings,
      scalacOptions += "-experimental",
      name := "oxygen-example-domain",
      description := "oxygen-example-domain",
      libraryDependencies ++= Seq(
        "org.mindrot" % "jbcrypt" % "0.4",
      ),
    )
    .dependsOn(
      `example-domain-models` % testAndCompile,
      `oxygen-crypto-service` % testAndCompile,
      `oxygen-storage` % testAndCompile,
    )

lazy val `example-domain-impl`: Project =
  project
    .in(file("example/modules/domain-impl"))
    .settings(
      nonPublishedProjectSettings,
      scalacOptions += "-experimental",
      name := "oxygen-example-domain-impl",
      description := "oxygen-example-domain-impl",
    )
    .dependsOn(
      `oxygen-sql` % testAndCompile,
      `oxygen-sql-migration` % testAndCompile,
      `oxygen-transform`.jvm % testAndCompile,
      `example-domain` % testAndCompile,
    )

lazy val `example-web-server`: Project =
  project
    .in(file("example/apps/web-server"))
    .settings(
      nonPublishedProjectSettings,
      scalacOptions += "-experimental",
      name := "oxygen-example-web-server",
      description := "oxygen-example-web-server",
    )
    .dependsOn(
      `oxygen-executable`.jvm % testAndCompile,
      `example-api`.jvm % testAndCompile,
      `example-domain-impl` % testAndCompile,
    )

val webComp: InputKey[Unit] = inputKey("webComp")
val webCompDirs = settingKey[Seq[File]]("webCompDirs")

lazy val `example-ui`: Project =
  project
    .in(file("example/apps/ui"))
    .enablePlugins(ScalaJSPlugin)
    .settings(
      nonPublishedProjectSettings,
      scalacOptions += "-experimental",
      name := "oxygen-example-ui",
      description := "oxygen-example-ui",
      scalaJSUseMainModuleInitializer := true,
      // webComp
      webCompDirs := Seq(
        file("example/apps/web-server/src/main/resources/res/js"),
      ),
      webComp :=
        Def.inputTaskDyn {
          import complete.DefaultParsers._

          val args: List[String] = spaceDelimited("<arg>").parsed.toList
          val (task, taskName) =
            if (args.contains("--full")) (fullLinkJS, "opt")
            else (fastLinkJS, "fastopt")

          val copySourceMap = !args.contains("--no-source-map")

          Def.sequential(
            Def
              .inputTask { println("Running 'webComp'...") }
              .toTask(""),
            Compile / task,
            Def
              .inputTask {
                def jsFile(name: String): File = {
                  val targetDir = (Compile / task / crossTarget).value
                  val projectName = normalizedName.value
                  new File(s"$targetDir/$projectName-$taskName/$name")
                }

                val files =
                  jsFile("main.js") ::
                    (if (copySourceMap) jsFile("main.js.map") :: Nil else Nil)

                webCompDirs.value.foreach { moveToDir =>
                  println(s"copying scripts to '$moveToDir'")

                  moveToDir.mkdirs()
                  moveToDir.listFiles.foreach { f =>
                    if (f.name.contains("main.js"))
                      f.delete()
                  }
                  files.foreach { f =>
                    IO.copyFile(f, new File(moveToDir, f.getName))
                  }
                }

                ()
              }
              .toTask(""),
          )
        }.evaluated,
    )
    .dependsOn(
      `example-api`.js,
      `oxygen-ui-web`,
    )

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Commands
//////////////////////////////////////////////////////////////////////////////////////////////////////

addCommandAlias("jvm-compile", "oxygen-modules-jvm/compile")
addCommandAlias("jvm-test-compile", "oxygen-modules-jvm/test:compile")
addCommandAlias("jvm-test", "oxygen-modules-jvm/test")

addCommandAlias("js-compile", "oxygen-modules-js/compile")
addCommandAlias("js-test-compile", "oxygen-modules-js/test:compile")
addCommandAlias("js-test", "oxygen-modules-js/test")

addCommandAlias("native-compile", "oxygen-modules-native/compile")
addCommandAlias("native-test-compile", "oxygen-modules-native/test:compile")
addCommandAlias("native-test", "oxygen-modules-native/test")

addCommandAlias("fmt", "scalafmtSbt; scalafmtAll; it/scalafmtAll; example/scalafmtAll;")
addCommandAlias("fmt-check", "scalafmtSbtCheck; scalafmtCheckAll; it/scalafmtCheckAll; example/scalafmtCheckAll;")
