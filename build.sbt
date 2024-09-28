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
    .in(file("modules/.jvm"))
    .settings(
      nonPublishedProjectSettings,
      name := "oxygen-modules-jvm",
      description := "oxygen-modules-jvm",
    )
    .aggregate(
      // General
      `oxygen-core`.jvm,
      `oxygen-json`.jvm,

      // Testing
      `oxygen-test`.jvm,

      // Internal
      `oxygen-pre-test-ut`.jvm,
    )

lazy val `oxygen-modules-js`: Project =
  project
    .in(file("modules/.js"))
    .settings(
      nonPublishedProjectSettings,
      name := "oxygen-modules-js",
      description := "oxygen-modules-js",
    )
    .aggregate(
      // General
      `oxygen-core`.js,
      `oxygen-json`.js,

      // Testing
      `oxygen-test`.js,

      // Internal
      `oxygen-pre-test-ut`.js,
    )

lazy val `oxygen-modules-native`: Project =
  project
    .in(file("modules/.native"))
    .settings(
      nonPublishedProjectSettings,
      name := "oxygen-modules-native",
      description := "oxygen-modules-native",
    )
    .aggregate(
      // General
      `oxygen-core`.native,
      `oxygen-json`.native,

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
      `oxygen-json` % testAndCompile, // TODO (KR) : this will need to depend on whichever library adds zio+Logger and all that stuff
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
      `oxygen-core` % testAndCompile,
      `oxygen-test` % Test,
    )
