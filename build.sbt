//

import Dependencies.*
import Settings.*
import sbtcrossproject.CrossProject

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Settings
//////////////////////////////////////////////////////////////////////////////////////////////////////

ThisBuild / watchBeforeCommand := Watch.clearScreen

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
      `oxygen-json`.jvm,
      `oxygen-zio`.jvm,

      // Testing
      `oxygen-test`.jvm,

      // Internal
      `oxygen-pre-test-ut`.jvm,
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
      `oxygen-json`.js,
      `oxygen-zio`.js,

      // Testing
      `oxygen-test`.js,

      // Internal
      `oxygen-pre-test-ut`.js,
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
      `oxygen-json`.native,
      `oxygen-zio`.native,

      // Testing
      `oxygen-test`.native,

      // Internal
      `oxygen-pre-test-ut`.native,
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
      libraryDependencies ++= Seq(
        zio.organization %%% zio.izumiReflect % zio.izumiVersion,
      ),
      scalacOptions ++= Seq("-Yretain-trees"),
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

lazy val `oxygen-json`: CrossProject =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .crossType(CrossType.Pure)
    .in(file("modules/json"))
    .settings(
      publishedProjectSettings,
      name := "oxygen-json",
      description := "Lightweight wrapper around `zio-json` to add compatibility with the `oxygen` ecosystem.",
      libraryDependencies ++= Seq(
        zio.organization %%% zio.zioJson % zio.zioJsonVersion,
      ),
    )
    .dependsOn(
      `oxygen-core` % testAndCompile,
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
        zio.organization %%% zio.zioJson % zio.zioJsonVersion,
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

lazy val `oxygen-pre-test-ut`: CrossProject =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .crossType(CrossType.Pure)
    .in(file("modules/pre-test-ut"))
    .settings(
      nonPublishedProjectSettings,
      name := "oxygen-core",
      description := "This serves no purpose besides being able to use `oxygen-test` to write tests for sub-projects that `oxygen-test` depends on.",
    )
    .dependsOn(
      `oxygen-zio` % testAndCompile,
      `oxygen-test` % Test,
    )
