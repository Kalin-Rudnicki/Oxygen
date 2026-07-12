//

import Dependencies.*
import Settings.*
import org.scalajs.linker.interface.{ModuleInitializer, ModuleSplitStyle}
import sbtcrossproject.CrossProject
import scala.sys.process.*

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Settings
//////////////////////////////////////////////////////////////////////////////////////////////////////

ThisBuild / watchBeforeCommand := Watch.clearScreen
ThisBuild / resolvers ++= Seq(Resolver.sonatypeCentralSnapshots, Resolver.mavenLocal)
ThisBuild / updateOptions ~= (_.withLatestSnapshots(false))

enablePlugins(GitVersioning)
git.gitTagToVersionNumber := { tag =>
  if (tag.matches("^\\d+\\..*$")) Some(tag)
  else None
}

ThisBuild / version := { // TODO (KR) : I hate doing this, but one/both of the git plugins seems to be bricked. Remove if they figure their shit out. :(
  (for {
    gitDesc <-
      try {
        Some(
          List("git", "describe", "--tags")
            .!!(
              new ProcessLogger {
                override def out(s: => String): Unit = ()
                override def err(s: => String): Unit = ()
                override def buffer[T](f: => T): T = f
              },
            )
            .trim,
        )
      } catch { case _: Throwable => None }
    tagV <-
      if (gitDesc.matches("^[0-9]+\\..*$")) Some(gitDesc)
      else None
    isSnapshot = tagV.matches("^.*-\\d+-.*$")
    suffix = if (isSnapshot) "-SNAPSHOT" else ""
  } yield tagV + suffix).getOrElse("unknown")
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
      `oxygen-quoted`.jvm,
      `oxygen-schema`.jvm,
      `oxygen-transform`.jvm,
      `oxygen-yaml`.jvm,
      `oxygen-zio`.jvm,

      // sql
      `oxygen-storage`,
      `oxygen-storage-in-memory`,
      `oxygen-sql`,
      `oxygen-sql-migration`,
      `oxygen-sql-test`,

      // events
      `oxygen-events`,
      `oxygen-events-in-memory`,
      `oxygen-events-pulsar`,
      // TODO (KR) : add
      // `oxygen-events-pulsar-test`,

      // http
      `oxygen-http`.jvm,
      // TODO (KR) : add
      // `oxygen-http-test`.jvm,

      // jwt
      `oxygen-crypto-model`.jvm,
      `oxygen-crypto-service`,

      // payments
      `oxygen-payment-models`.jvm,
      `oxygen-stripe-models`.jvm,
      `oxygen-payments-stripe-service`,

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
      `oxygen-quoted`.js,
      `oxygen-schema`.js,
      `oxygen-transform`.js,
      `oxygen-yaml`.js,
      `oxygen-zio`.js,

      // http
      `oxygen-http`.js,

      // ui
      `oxygen-ui-electron`,
      `oxygen-ui-web`,

      // oidc
      `oxygen-oidc`,

      // jwt
      `oxygen-crypto-model`.js,

      // payments
      `oxygen-payment-models`.js,
      `oxygen-stripe-models`.js,
      `oxygen-payments-stripe-ui`,

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
      `oxygen-quoted`.native,
      `oxygen-schema`.native,
      `oxygen-transform`.native,
      `oxygen-yaml`.native,
      `oxygen-zio`.native,

      // jwt
      `oxygen-crypto-model`.native,

      // Testing
      `oxygen-test`.native,

      // Internal
      `ut`.native,
    )

lazy val `oxygen-all`: Project =
  project
    .in(file("modules/aggregates/all"))
    .settings(
      nonPublishedProjectSettings,
      name := "oxygen-all",
      description := "oxygen-all",
    )
    .aggregate(
      `oxygen-modules`,
      `it`,
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
    .dependsOn(
      `oxygen-quoted` % testAndCompile,
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
      `oxygen-schema` % testAndCompile,
      `oxygen-zio` % testAndCompile,
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
      `oxygen-yaml` % testAndCompile,
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
      `oxygen-core` % testAndCompile,
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
      `oxygen-test`.jvm % Test,
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

lazy val `oxygen-events`: Project =
  project
    .in(file("modules/events/core"))
    .settings(
      publishedProjectSettings,
      name := "oxygen-events",
      description := "Generic oxygen events.",
    )
    .dependsOn(
      `oxygen-schema`.jvm % testAndCompile,
      `oxygen-zio`.jvm % testAndCompile,
      `oxygen-test`.jvm % Test,
    )

lazy val `oxygen-events-in-memory`: Project =
  project
    .in(file("modules/events/in-memory"))
    .settings(
      publishedProjectSettings,
      name := "oxygen-events-in-memory",
      description := "In-memory implementation of oxygen-events.",
    )
    .dependsOn(
      `oxygen-events` % testAndCompile,
    )

lazy val `oxygen-events-pulsar`: Project =
  project
    .in(file("modules/events/pulsar"))
    .settings(
      publishedProjectSettings,
      name := "oxygen-events-pulsar",
      description := "Apache pulsar implementation of oxygen-events.",
      libraryDependencies ++= Seq(
        pulsar.organization % pulsar.client % pulsar.version,
        pulsar.organization % pulsar.adminClient % pulsar.version,
      ),
    )
    .dependsOn(
      `oxygen-events` % testAndCompile,
      `oxygen-schema`.jvm % testAndCompile,
    )

lazy val `oxygen-storage-in-memory`: Project =
  project
    .in(file("modules/sql/storage-in-memory"))
    .settings(
      publishedProjectSettings,
      name := "oxygen-storage-in-memory",
      description := "In-memory implementation of oxygen-storage",
    )
    .dependsOn(
      `oxygen-storage` % testAndCompile,
    )

lazy val `oxygen-payment-models`: CrossProject =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("modules/payments/models"))
    .settings(
      publishedProjectSettings,
      name := "oxygen-payments-models",
      description := "Core payment models",
    )
    .dependsOn(
      `oxygen-schema` % testAndCompile,
    )

lazy val `oxygen-stripe-models`: CrossProject =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("modules/payments/stripe-models"))
    .settings(
      publishedProjectSettings,
      name := "oxygen-payments-stripe-models",
      description := "Core stripe models",
    )
    .dependsOn(
      `oxygen-payment-models` % testAndCompile,
    )

lazy val `oxygen-payments-stripe-ui`: Project =
  project
    .in(file("modules/payments/stripe-ui"))
    .enablePlugins(ScalaJSPlugin)
    .settings(
      publishedProjectSettings,
      name := "oxygen-payments-stripe-ui",
      description := "Stripe UI integration",
    )
    .dependsOn(
      `oxygen-ui-web` % testAndCompile,
      `oxygen-stripe-models`.js % testAndCompile,
    )

lazy val `oxygen-payments-stripe-service`: Project =
  project
    .in(file("modules/payments/stripe-service"))
    .settings(
      publishedProjectSettings,
      name := "oxygen-payments-stripe-service",
      description := "Stripe backend integration",
      scalacOptions += "-experimental",
      libraryDependencies ++= Seq(
        "com.stripe" % "stripe-java" % "33.1.0",
      ),
    )
    .dependsOn(
      `oxygen-http`.jvm % testAndCompile,
      `oxygen-stripe-models`.jvm % testAndCompile,
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
      `oxygen-core` % testAndCompile,
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
    // MCP support (JVM server) validates JWT bearer tokens via oxygen-crypto-service.
    .jvmConfigure(_.dependsOn(`oxygen-crypto-service` % testAndCompile))

lazy val `oxygen-ui-electron`: Project =
  project
    .in(file("modules/ui/electron"))
    .enablePlugins(ScalaJSPlugin)
    .settings(
      publishedProjectSettings,
      name := "oxygen-ui-electron",
      description := "Make your desktop-ui using scala and FP principles!",
    )
    .dependsOn(
      `oxygen-ui-web` % testAndCompile,
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

lazy val `oxygen-oidc`: Project =
  project
    .in(file("modules/oidc"))
    .enablePlugins(ScalaJSPlugin)
    .settings(
      publishedProjectSettings,
      name := "oxygen-oidc",
      description := "OpenID Connect relying-party client for Scala.js SPAs (Authorization Code + PKCE).",
    )
    .dependsOn(
      `oxygen-http`.js % testAndCompile,
      `oxygen-crypto-model`.js % testAndCompile,
    )

lazy val `oxygen-yaml`: CrossProject =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .crossType(CrossType.Pure)
    .in(file("modules/general/yaml"))
    .settings(
      publishedProjectSettings,
      name := "oxygen-yaml",
      description := "Wrapper around scala-yaml to work with oxygen-json.",
      libraryDependencies ++= Seq(
        virtusLabs.organization %% virtusLabs.scalaYaml % virtusLabs.scalaYamlVersion,
      ),
    )
    .dependsOn(
      `oxygen-json` % testAndCompile,
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
      `oxygen-schema` % testAndCompile,
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
      `oxygen-sql-migration` % testAndCompile,
    )

// TODO (KR) :
// lazy val `oxygen-events-pulsar-test`: Project =
//   project
//     .in(file("modules/events/pulsar-test-utils"))
//     .settings(
//       publishedProjectSettings,
//       name := "oxygen-events-pulsar-test",
//       description := "Test utils for oxygen-events-pulsar.",
//     )
//     .dependsOn(
//       `oxygen-test-container` % testAndCompile,
//       `oxygen-events-pulsar` % testAndCompile,
//     )

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
      `events-it`,
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

lazy val `events-it`: Project =
  project
    .in(file("modules/events/it-test"))
    .settings(
      nonPublishedProjectSettings,
      name := "events-it",
      description := "events-it",
    )
    .dependsOn(
      `oxygen-events` % testAndCompile,
      `oxygen-events-in-memory` % testAndCompile,
      `oxygen-events-pulsar` % testAndCompile,
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
      `example-app`,
      // js-only
      `example-ui-web`,
      `example-ui-electron`,
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
      `oxygen-schema` % testAndCompile,
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
      `oxygen-stripe-models` % testAndCompile,
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
      `oxygen-payments-stripe-service` % testAndCompile,
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
    .enablePlugins(AssemblyPlugin)
    .settings(
      nonPublishedProjectSettings,
      scalacOptions += "-experimental",
      name := "oxygen-example-web-server",
      description := "oxygen-example-web-server",
      mainClass := Some("oxygen.example.webServer.WebServerMain"),
      assemblyJarName := "../../../../../target/artifacts/jars/example-web-server.jar",
      assemblyMergeStrategy := {
        case PathList("META-INF", "native-image", _*) => MergeStrategy.concat
        case PathList("META-INF", _*)                 => MergeStrategy.discard
        case _                                        => MergeStrategy.first // Use default for other files
      },
    )
    .dependsOn(
      `oxygen-executable`.jvm % testAndCompile,
      `example-api`.jvm % testAndCompile,
      `example-domain-impl` % testAndCompile,
      `oxygen-sql-test` % Test,
    )

lazy val `example-app`: Project =
  project
    .in(file("example/apps/example-app"))
    .enablePlugins(AssemblyPlugin)
    .settings(
      nonPublishedProjectSettings,
      scalacOptions += "-experimental",
      name := "oxygen-example-app",
      description := "oxygen-example-app",
      mainClass := Some("oxygen.example.exampleApp.ShowcaseApp"),
      assemblyJarName := "../../../../../target/artifacts/jars/example-app.jar",
      assemblyMergeStrategy := {
        case PathList("META-INF", "native-image", _*) => MergeStrategy.concat
        case PathList("META-INF", _*)                 => MergeStrategy.discard
        case _                                        => MergeStrategy.first
      },
    )
    .dependsOn(
      `oxygen-executable`.jvm % testAndCompile,
      `oxygen-schema`.jvm % testAndCompile,
    )

val webComp: InputKey[Unit] = inputKey("webComp")
val webCompDirs = settingKey[Seq[File]]("webCompDirs")

lazy val `example-ui-web`: Project =
  project
    .in(file("example/apps/ui"))
    .enablePlugins(ScalaJSPlugin)
    .settings(
      nonPublishedProjectSettings,
      scalacOptions += "-experimental",
      name := "oxygen-example-ui-web",
      description := "oxygen-example-ui-web",
      scalaJSUseMainModuleInitializer := true,
      // webComp
      webCompDirs := Seq(
        file("example/apps/web-server/res/js"),
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
      `example-api`.js % testAndCompile,
      `oxygen-ui-web` % testAndCompile,
      `oxygen-payments-stripe-ui` % testAndCompile,
    )

lazy val `example-ui-electron`: Project =
  project
    .in(file("example/apps/ui-electron"))
    .enablePlugins(ScalaJSPlugin)
    .settings(
      nonPublishedProjectSettings,
      scalacOptions += "-experimental",
      name := "oxygen-example-ui-electron",
      description := "oxygen-example-ui-electron",
      scalaJSModuleInitializers := Seq(
        ModuleInitializer
          .mainMethodWithArgs("oxygen.example.ui.electron.Main", "main")
          .withModuleID("electron-main"),
        ModuleInitializer
          .mainMethodWithArgs("oxygen.example.ui.electron.Renderer", "main")
          .withModuleID("electron-renderer"),
      ),
      Compile / fastLinkJS / crossTarget := target.value / "electron",
      scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },
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
                import scala.sys.process._

                val rootTargetDir = (`oxygen-root` / target).value
                val projectTargetDir = (Compile / task / crossTarget).value

                def compiledJsFile(name: String): File = {
                  val projectName = normalizedName.value
                  new File(s"$projectTargetDir/$projectName-$taskName/$name")
                }
                def outFile(name: String): File = {
                  val projectName = normalizedName.value
                  new File(s"$projectTargetDir/out/$name")
                }

                val compiledRendererFile = compiledJsFile("electron-renderer.js")
                val compiledMainFile = compiledJsFile("electron-main.js")
                val outRendererFile = outFile("electron-renderer.js")
                val outMainFile = outFile("electron-main.js")

                // TODO (KR) : remove --no-sandbox

                val appName = "oxygen-ui-electron"
                val appVersion = "1.0.0"

                val indexHtmlContents =
                  s"""
                     |<!DOCTYPE html>
                     |<html>
                     |<head>
                     |  <meta charset="UTF-8">
                     |  <title>Oxygen Electron App</title>
                     |</head>
                     |<body>
                     |  <div id="app">Loading...</div>
                     |  <script src="electron-renderer.js"></script>
                     |</body>
                     |</html>
                     |""".stripMargin
                val packageJsonContents =
                  s"""
                     |{
                     |  "name": "$appName",
                     |  "version": "$appVersion",
                     |  "main": "electron-main.js",
                     |  "description": "$appName",
                     |  "author": {
                     |    "name": "Oxygen",
                     |    "email": "oxygen@gmail.com"
                     |  },
                     |  "homepage": "https://github.com/Kalin-Rudnicki/Oxygen",
                     |  "scripts": {
                     |    "start": "electron --no-sandbox .",
                     |    "dist": "electron-builder --linux"
                     |  },
                     |  "devDependencies": {
                     |    "electron": "^41.4.0",
                     |    "electron-builder": "^26.8.1"
                     |  },
                     |  "build": {
                     |    "appId": "com.yourname.myoxygen",
                     |    "productName": "$appName",
                     |    "linux": {
                     |      "target": ["AppImage", "deb"],
                     |      "category": "Utility"
                     |    }
                     |  }
                     |}
                     |""".stripMargin

                outFile("").mkdirs()
                IO.write(outFile("index.html"), indexHtmlContents)
                IO.write(outFile("package.json"), packageJsonContents)
                List("esbuild", compiledRendererFile.toString, "--bundle", s"--outfile=$outRendererFile", "--platform=node").!
                List("esbuild", compiledMainFile.toString, "--bundle", s"--outfile=$outMainFile", "--platform=node", "--external:electron").!

                Process(List("npm", "install"), outFile("")).!
                Process(List("npm", "run", "dist"), outFile("")).!
                IO.copyFile(outFile(s"dist/$appName-$appVersion.AppImage"), new File(s"$rootTargetDir/$appName.AppImage"))

                ()
              }
              .toTask(""),
          )
        }.evaluated,
    )
    .dependsOn(
      `oxygen-ui-electron`,
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

addCommandAlias("comp", "oxygen-all/test:compile;")
