package oxygen.http

import java.time.Instant
import java.util.UUID
import oxygen.http.core.ServerSentEvents
import oxygen.predef.core.*
import oxygen.zio.instances.chunkSeqOps
import scala.annotation.experimental
import zio.*
import zio.stream.*

@experimental
final case class UserApiImpl(ref: Ref[Map[UUID, User]]) extends UserApi {

  override def userById(id: UUID): IO[ApiError, User] =
    ZIO.logInfo(s"looking for user with id $id") *>
      ref.get.map(_.get(id)).someOrFail(ApiError.NoSuchUser(id))

  override def allUsers(): UIO[Chunk[User]] =
    ZIO.logInfo("looking for all users") *>
      ref.get.map(_.values.into[Chunk])

  override def createUser(create: CreateUser): UIO[User] =
    for {
      _ <- ZIO.logInfo(s"Attempting to create user ${create.first} ${create.last}")
      id <- Random.nextUUID
      user = User(id, create.first, create.last, create.age)
      _ <- ref.update(_.updated(user.id, user))
    } yield user

  override def userSearch(firstName: Option[String], lastName: Option[String]): UIO[Set[User]] =
    for {
      _ <- ZIO.logInfo(s"searching for user with firstName=$firstName, lastName=$lastName")
      all <- ref.get.map(_.values.into[Chunk])
      filters =
        Seq[Option[User => Boolean]](
          firstName.map { firstName => { (u: User) => u.first == firstName } },
          lastName.map { lastName => { (u: User) => u.last == lastName } },
        ).flatten
      filtered = if filters.isEmpty then all else all.filter { u => filters.forall(_(u)) }
    } yield filtered.toSet

  override def userEvents(
      userId: UUID,
      numEvents: Option[Int],
  ): ServerSentEvents[String, UserEvent] =
    ServerSentEvents.makeBasic {
      ref.get.flatMap {
        _.get(userId) match
          case Some(user) => ZIO.succeed(user)
          case None       => ZIO.fail(s"No such user: $userId")
      }
    } { user =>
      val randomEvent: UIO[UserEvent] =
        for {
          bool <- Random.nextBoolean
          eventUserId <- if bool then ZIO.succeed(user.id) else Random.nextUUID
          eventId <- Random.nextUUID
        } yield UserEvent(eventUserId, s"Random event ($eventId)")

      ZStream.succeed(UserEvent(user.id, "Welcome to the stream!")) ++
        ZStream.fromIterableZIO { (Random.RandomLive.nextIntBetween(100, 500).flatMap { t => Clock.ClockLive.sleep(t.millis) } *> randomEvent).replicateZIO(numEvents.get) }
    }

  override def macroTest(
      value: CustomPathItem,
      instant: Option[Instant],
      limit: Option[Int],
      authorization: String,
  ): IO[String, String] =
    ZIO.succeed("success")

}
object UserApiImpl {

  val layer: ULayer[UserApi] =
    ZLayer { Ref.make(Map.empty[UUID, User]).map(UserApiImpl(_)) }

}
