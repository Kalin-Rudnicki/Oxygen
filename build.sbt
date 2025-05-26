//

import Dependencies.*
import Settings.*
import sbtcrossproject.CrossProject

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Settings
//////////////////////////////////////////////////////////////////////////////////////////////////////

ThisBuild / watchBeforeCommand := Watch.clearScreen

ThisBuild / sonatypeCredentialHost := "s01.oss.sonatype.org"
sonatypeRepository := "https://s01.oss.sonatype.org/service/local"

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
    .in(file("aggregates/.jvm"))
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
      `oxygen-web-model`.jvm,
      `oxygen-zio`.jvm,

      // Testing
      `oxygen-test`.jvm,

      // Internal
      `ut`.jvm,
    )

lazy val `oxygen-modules-js`: Project =
  project
    .in(file("aggregates/.js"))
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
      `oxygen-web-model`.js,
      `oxygen-zio`.js,

      // Testing
      `oxygen-test`.js,

      // Internal
      `ut`.js,
    )

lazy val `oxygen-modules-native`: Project =
  project
    .in(file("aggregates/.native"))
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
      `oxygen-web-model`.native,
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
    .in(file("modules/core"))
    .settings(
      publishedProjectSettings,
      name := "oxygen-core",
      description := "Basic utilities beneficial for any scala user.",
    )

lazy val `oxygen-cli`: CrossProject =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .crossType(CrossType.Pure)
    .in(file("modules/cli"))
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
    .in(file("modules/executable"))
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
    .in(file("modules/json"))
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
    .in(file("modules/meta"))
    .settings(
      publishedProjectSettings,
      name := "oxygen-meta",
      description := "Metaprogramming for scala-3.",
    )
    .dependsOn(
      `oxygen-core` % testAndCompile,
    )

lazy val `oxygen-web-model`: CrossProject =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .crossType(CrossType.Pure)
    .in(file("modules/web-model"))
    .settings(
      publishedProjectSettings,
      name := "oxygen-web-model",
      description := "Adds standard models representing the web.",
    )
    .dependsOn(
      `oxygen-core` % testAndCompile,
      `oxygen-test` % Test,
    )

lazy val `oxygen-zio`: CrossProject =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .crossType(CrossType.Pure)
    .in(file("modules/zio"))
    .settings(
      publishedProjectSettings,
      name := "oxygen-zio",
      description := "Opinionated library of basic utilities to integrate with the `zio` ecosystem.",
      libraryDependencies ++= Seq(
        zio.organization %%% zio.zio % zio.coreVersion,
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
    .in(file("modules/testing"))
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

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Internal
//////////////////////////////////////////////////////////////////////////////////////////////////////

lazy val `it`: Project =
  project
    .in(file("testing/integration-tests"))
    .settings(
      nonPublishedProjectSettings,
      name := "oxygen-it",
      description := "oxygen-it",
    )
    .aggregate(
    )

lazy val `ut`: CrossProject =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .crossType(CrossType.Pure)
    .in(file("testing/pre-test-unit-tests"))
    .settings(
      nonPublishedProjectSettings,
      name := "oxygen-core",
      description := "This serves no purpose besides being able to use `oxygen-test` to write tests for sub-projects that `oxygen-test` depends on.",
    )
    .dependsOn(
      `oxygen-zio` % testAndCompile,
      `oxygen-test` % Test,
    )

addCommandAlias("jvm-compile", "oxygen-modules-jvm/compile")
addCommandAlias("jvm-test-compile", "oxygen-modules-jvm/test:compile")
addCommandAlias("jvm-test", "oxygen-modules-jvm/test")
