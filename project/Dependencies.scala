//

object Dependencies {

  object kalinRudnicki {

    val organization = "io.github.kalin-rudnicki"

  }

  object postgres {

    val organization = "org.postgresql"

    val version = "42.7.5"

    val postgres = "postgresql"

  }

  object zio {

    val organization = "dev.zio"

    // =====| Core |=====

    val coreVersion = "2.1.15"

    val zio = "zio"
    val streams = "zio-streams"
    val test = "zio-test"
    val testSbt = "zio-test-sbt"

    // =====| Izumi |=====

    val izumiVersion = "2.3.9"

    val izumiReflect = "izumi-reflect"

    // =====| Json |=====

    val zioJsonVersion = "0.7.3"

    val zioJson = "zio-json"

  }

}
