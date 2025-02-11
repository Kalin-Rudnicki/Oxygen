//

import sbt.*
import sbt.Keys.*

object Settings {

  // =====|  |=====

  private val Scala_3 = "3.6.3"

  private val GithubUsername = "Kalin-Rudnicki"
  private val GithubProject = "oxygen"

  // =====|  |=====

  val testAndCompile = "test->test;compile->compile"
  val testToTest = "test->test"

  private val settingsForAll: Seq[Def.Setting[_]] =
    Seq(
      scalaVersion := Scala_3,
      organization := Dependencies.kalinRudnicki.organization,
      scalacOptions ++= Seq("-source:future", "-Ycheck-all-patmat", "-Wunused:all", "-Werror", "-language:implicitConversions", "-deprecation", "-feature"),
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
      // TODO (KR) :
    )

}
