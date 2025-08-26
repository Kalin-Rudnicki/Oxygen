//

import Dependencies.*
import Settings.*
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
      `oxygen-zio`.jvm,

      // sql
      `oxygen-sql`,
      `oxygen-sql-migration`,
      `oxygen-sql-test`,

      // http
      `oxygen-http`.jvm,
      // TODO (KR) : add
      // `oxygen-http-test`.jvm,

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
      `oxygen-zio`.js,

      // http
      `oxygen-http`.js,

      // ui
      `oxygen-ui-web`,

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
      `oxygen-zio`.native,

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
      `oxygen-json` % testAndCompile,
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
      `oxygen-zio`.jvm % testAndCompile,
      `oxygen-test`.jvm % Test,
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

// TODO (KR) : add this when there is something to put here
/*
lazy val `oxygen-http-test`: CrossProject =
  // crossProject(JSPlatform, JVMPlatform, NativePlatform)
  crossProject(JVMPlatform) // only publish JVM, for now
    .crossType(CrossType.Pure)
    .in(file("modules/http-test"))
    .settings(
      publishedProjectSettings,
      name := "oxygen-http-test",
      description := "Test utils for oxygen-http-*.",
    )
    .dependsOn(
      `oxygen-http-client` % testAndCompile,
      `oxygen-http-server` % testAndCompile,
      `oxygen-test` % testAndCompile,
    )
 */

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
    .in(file("example/modules"))
    .settings(
      nonPublishedProjectSettings,
      name := "oxygen-example",
      description := "oxygen-example",
    )
    .aggregate(
      `example-ui`,
    )

lazy val `example-ui`: Project =
  project
    .in(file("example/modules/ui"))
    .enablePlugins(ScalaJSPlugin)
    .settings(
      nonPublishedProjectSettings,
      name := "oxygen-example-ui",
      description := "oxygen-example-ui",
      scalaJSUseMainModuleInitializer := true,
    )
    .dependsOn(`oxygen-ui-web`)

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

addCommandAlias("fmt", "scalafmtAll")
