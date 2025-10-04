//

object Dependencies {

  object kalinRudnicki {

    val organization = "io.github.kalin-rudnicki"

  }

  object monocle {

    val organization = "dev.optics"

    val version = "3.3.0"

    val core = "monocle-core"
    val `macro` = "monocle-macro"

  }

  object postgres {

    val organization = "org.postgresql"

    val version = "42.7.5"

    val postgres = "postgresql"

  }

  object zio {

    val organization = "dev.zio"

    // =====| Core |=====

    val coreVersion = "2.1.21"

    val zio = "zio"
    val streams = "zio-streams"
    val test = "zio-test"
    val testSbt = "zio-test-sbt"

    // =====| HTTP |=====

    val httpVersion = "3.5.1"

    val http = "zio-http"

  }

}
