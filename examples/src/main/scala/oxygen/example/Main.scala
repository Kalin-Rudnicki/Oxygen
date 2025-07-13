package oxygen.example

import java.time.Instant

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
      notes: Seq[Note],
  )

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
      notes: List[Note],
  )

}

object Main extends scala.App {

  given Transformer[DomainModels.UserId, ApiModels.UserId] = Transformer.derived
  given Transformer[DomainModels.Note, ApiModels.Note] = Transformer.derived
  given Transformer[DomainModels.User, ApiModels.User] = Transformer.derived

  println(Vector("a", "b", "c").transformTo[List[String]])

}
