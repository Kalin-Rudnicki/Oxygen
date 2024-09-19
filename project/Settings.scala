//

import sbt.*
import sbt.Keys.*

object Settings {

  // =====|  |=====

  private val Scala_3 = "3.3.0"

  private val MyOrg = "io.github.kalin-rudnicki"
  private val GithubUsername = "Kalin-Rudnicki"
  private val GithubProject = "oxygen"

  // =====|  |=====

  private val settingsForAll: Seq[Def.Setting[_]] =
    Seq(
      scalaVersion := Scala_3,
      organization := MyOrg,
      name := { throw new RuntimeException("You must define a project name!!!") },
      description := { throw new RuntimeException("You must define a project description!!!") },
    )

  val nonPublishedProjectSettings: Seq[Def.Setting[_]] =
    settingsForAll ++ Seq(
      publish / skip := true,
    )

  // TODO (KR) : shared settings

}
