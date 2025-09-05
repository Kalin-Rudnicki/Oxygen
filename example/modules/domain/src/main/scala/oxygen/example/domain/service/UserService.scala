package oxygen.example.domain.service

import org.mindrot.jbcrypt.BCrypt
import oxygen.example.core.model.user.*
import oxygen.example.domain.model.error.*
import oxygen.example.domain.model.user.*
import oxygen.example.domain.repo.UserRepo
import zio.*

final class UserService(userRepo: UserRepo) {

  def register(req: Register): IO[RegistrationError, SimpleUser] =
    for {
      user <- userRepo.findUserByEmail(req.email)
      _ <- ZIO.fail(RegistrationError.EmailAlreadyExists(req.email)).whenDiscard(user.nonEmpty)
      id <- Random.nextUUID
      now <- Clock.instant
      salt <- ZIO.succeed { BCrypt.gensalt() }
      hashedPassword <- ZIO.succeed { BCrypt.hashpw(req.password, salt) }
      user = FullUser(
        id = UserId(id),
        email = req.email,
        firstName = req.firstName,
        lastName = req.lastName,
        hashedPassword = HashedPassword(hashedPassword),
        createdAt = now,
      )
      _ <- userRepo.insertUser(user)
      simpleUser = user.toSimple
      _ <- ZIO.logInfo(s"Registered new user ${simpleUser.show}")
    } yield simpleUser

  def login(req: Login): IO[LoginError, SimpleUser] =
    for {
      user <- userRepo.findUserByEmail(req.email).someOrFail(LoginError.EmailDoesNotExist(req.email))
      _ <- ZIO.fail(LoginError.InvalidPassword(req.email)).unless(BCrypt.checkpw(req.password, user.hashedPassword.value))
    } yield user.toSimple

  def getUser(userId: UserId): IO[DomainError.UserIdDoesNotExist, SimpleUser] =
    userRepo.getUserById(userId).map(_.toSimple)

}
object UserService {

  val layer: URLayer[UserRepo, UserService] =
    ZLayer.fromFunction { UserService.apply }

}
