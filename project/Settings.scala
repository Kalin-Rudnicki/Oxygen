//

import sbt.*
import sbt.Keys.*
import xerial.sbt.Sonatype.SonatypeKeys.{sonatypeCredentialHost, sonatypeRepository}

object Settings {

  // =====|  |=====

  private val Scala_3 = "3.6.4"

  private val GithubUsername = "Kalin-Rudnicki"
  private val GithubProject = "Oxygen"

  // =====|  |=====

  val testAndCompile = "test->test;compile->compile"
  val testToTest = "test->test"

  private val settingsForAll: Seq[Def.Setting[_]] =
    Seq(
      scalaVersion := Scala_3,
      organization := Dependencies.kalinRudnicki.organization,
      scalacOptions ++= Seq("-source:future", "-Ycheck-all-patmat", "-Wunused:all", "-Werror", "-language:implicitConversions", "-deprecation", "-feature", "-experimental"),
      name := { throw new RuntimeException("You must define a project name!!!") },
      description := { throw new RuntimeException("You must define a project description!!!") },
      testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    )

  val nonPublishedProjectSettings: Seq[Def.Setting[_]] =
    settingsForAll ++ Seq(
      publish / skip := true,
    )

  val publishedProjectSettings: Seq[Def.Setting[_]] =
    settingsForAll ++ Seq(
      licenses := List("MIT" -> new URL("https://opensource.org/licenses/MIT")),
      homepage := Some(url(s"https://github.com/$GithubUsername/$GithubProject")),
      developers := List(
        Developer(
          id = "Kalin-Rudnicki",
          name = "Kalin Rudnicki",
          email = "kalin.rudnicki@gmail.com",
          url = url(s"https://github.com/$GithubUsername"),
        ),
      ),
      sonatypeCredentialHost := "s01.oss.sonatype.org",
      sonatypeRepository := "https://s01.oss.sonatype.org/service/local",
    )

}
