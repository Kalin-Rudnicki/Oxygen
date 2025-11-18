//

import sbt.*
import sbt.Keys.*
import scala.sys.process.*

object Settings {

  // =====|  |=====

  private val Scala_3 = "3.7.1"

  private val GithubUsername = "Kalin-Rudnicki"
  private val GithubProject = "Oxygen"

  // =====|  |=====

  val testAndCompile = "test->test;compile->compile"
  val testToTest = "test->test"

  private def settingsForAll: Seq[Def.Setting[_]] =
    Seq(
      scalaVersion := Scala_3,
      organization := Dependencies.kalinRudnicki.organization,
      scalacOptions ++= Seq("-source:future", "-Ycheck-all-patmat", "-Wunused:all", "-Werror", "-language:implicitConversions", "-deprecation", "-feature", "-Yretain-trees"),
      name := { throw new RuntimeException("You must define a project name!!!") },
      javacOptions ++= Seq("-source", "17", "-target", "17"),
      usePipelining := false,
      description := { throw new RuntimeException("You must define a project description!!!") },
      testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
      version := { // TODO (KR) : I hate doing this, but one/both of the git plugins seems to be bricked. Remove if they figure their shit out.
        (for {
          gitDesc <-
            try {
              Some(
                List("git", "describe", "--tags", "--exact-match")
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
        } yield tagV).getOrElse(version.value + "-SNAPSHOT")
      },
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
