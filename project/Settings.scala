//

import sbt.*
import sbt.Keys.*

object Settings {

  // =====|  |=====

  private val Scala_3 = "3.7.4"

  private val GithubUsername = "Kalin-Rudnicki"
  private val GithubProject = "Oxygen"

  // =====|  |=====

  val testAndCompile = "test->test;compile->compile"
  val testToTest = "test->test"

  private def settingsForAll: Seq[Def.Setting[_]] =
    Seq(
      scalaVersion := Scala_3,
      organization := Dependencies.kalinRudnicki.organization,
      dependencyOverrides ++= Seq(
        "org.scala-lang" %% "scala3-library" % Scala_3,
      ),
      scalacOptions ++= Seq("-source:future", "-Ycheck-all-patmat", "-Wunused:all", "-Werror", "-language:implicitConversions", "-deprecation", "-feature", "-Yretain-trees"),
      name := { throw new RuntimeException("You must define a project name!!!") },
      javacOptions ++= Seq("-source", "17", "-target", "17"),
      usePipelining := true,
      description := { throw new RuntimeException("You must define a project description!!!") },
      testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    )

  def nonPublishedProjectSettings: Seq[Def.Setting[_]] =
    settingsForAll ++ Seq(
      publish / skip := true,
    )

  def publishedProjectSettings: Seq[Def.Setting[_]] =
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
    )

  def addCrossDirectory(path: String): Def.Setting[_] =
    (Compile / unmanagedSourceDirectories) +=
      baseDirectory.value / ".." / path / "src" / "main" / "scala"

}
