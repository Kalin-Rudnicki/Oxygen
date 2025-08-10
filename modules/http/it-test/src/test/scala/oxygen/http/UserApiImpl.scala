package oxygen.http

import java.util.UUID
import oxygen.predef.core.*
import scala.annotation.experimental
import zio.*

@experimental
final case class UserApiImpl(ref: Ref[Map[UUID, User]]) extends UserApi {

  override def userById(id: UUID): IO[ApiError, User] =
    ZIO.logInfo(s"looking for user with id $id") *>
      ref.get.map(_.get(id)).someOrFail(ApiError.NoSuchUser(id))

  override def allUsers(): UIO[Contiguous[User]] =
    ZIO.logInfo("looking for all users") *>
      ref.get.map(_.values.into[Contiguous])

  override def createUser(create: CreateUser): UIO[User] =
    for {
      _ <- ZIO.logInfo(s"Attempting to create user ${create.first} ${create.last}")
      id <- Random.nextUUID
      user = User(id, create.first, create.last, create.age)
      _ <- ref.update(_.updated(user.id, user))
    } yield user

}
object UserApiImpl {

  val layer: ULayer[UserApi] =
    ZLayer { Ref.make(Map.empty[UUID, User]).map(UserApiImpl(_)) }

}
