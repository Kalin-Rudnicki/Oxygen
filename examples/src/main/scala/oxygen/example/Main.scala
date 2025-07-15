package oxygen.example

import java.time.{Duration, Instant}

object ApiModels {

  final case class UserId(id: Long)

  final case class Note(
      note: String,
      timestamp: Instant,
  )

  final case class User(
      id: UserId,
      first: String,
      last: String,
      age: Int,
      notes: List[Note],
  ) {

    override def toString: String =
      s"$first $last (${id.id})${notes.map { n => s"\n    - [${n.timestamp}] $n" }.mkString}"

  }

}

object DomainModels {

  final case class UserId(id: Long)

  final case class Note(
      id: Long,
      note: String,
      timestamp: Instant,
  )

  final case class User(
      id: UserId,
      first: String,
      last: String,
      age: Int,
      notes: Vector[Note],
  )

}

object Main extends scala.App {

  given Transformer[DomainModels.UserId, ApiModels.UserId] = Transformer.derived
  given Transformer[DomainModels.Note, ApiModels.Note] = Transformer.derived
  given Transformer[DomainModels.User, ApiModels.User] = Transformer.derived

  val user1: DomainModels.User =
    DomainModels.User(
      DomainModels.UserId(1L),
      "f",
      "l",
      1,
      Vector(
        DomainModels.Note(10L, "n1", Instant.EPOCH),
        DomainModels.Note(11L, "n2", Instant.EPOCH.plus(Duration.ofDays(1))),
        DomainModels.Note(12L, "n3", Instant.EPOCH.plus(Duration.ofDays(2))),
      ),
    )

  println()
  println("domain:")
  println(user1)
  println()
  println("api:")
  println(user1.transformTo[ApiModels.User])
  println()

}
