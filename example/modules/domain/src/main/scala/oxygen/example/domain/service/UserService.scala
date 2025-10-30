package oxygen.example.domain.service

import oxygen.crypto.service.PasswordService
import oxygen.example.core.model.user.*
import oxygen.example.domain.model.error.*
import oxygen.example.domain.model.user.*
import oxygen.example.domain.repo.UserRepo
import zio.*

final class UserService(userRepo: UserRepo, passwordService: PasswordService) {

  def register(req: Register): IO[RegistrationError, SimpleUser] =
    for {
      user <- userRepo.findUserByEmail(req.email)
      _ <- ZIO.fail(RegistrationError.EmailAlreadyExists(req.email)).whenDiscard(user.nonEmpty)
      id <- Random.nextUUID
      now <- Clock.instant
      hashedPassword <- passwordService.hashPassword(req.password)
      user = FullUser(
        id = UserId(id),
        email = req.email,
        firstName = req.firstName,
        lastName = req.lastName,
        hashedPassword = hashedPassword,
        createdAt = now,
      )
      _ <- userRepo.insert(user)
      simpleUser = user.toSimple
      _ <- ZIO.logInfo(s"Registered new user ${simpleUser.show}")
    } yield simpleUser

  def login(req: Login): IO[LoginError, SimpleUser] =
    for {
      user <- userRepo.findUserByEmail(req.email).someOrFail(LoginError.EmailDoesNotExist(req.email))
      passwordIsValid <- passwordService.validate(req.password, user.hashedPassword)
      _ <- ZIO.fail(LoginError.InvalidPassword(req.email)).unless(passwordIsValid)
    } yield user.toSimple

  def getUser(userId: UserId): IO[DomainError.UserIdDoesNotExist, SimpleUser] =
    userRepo.getUserById(userId).map(_.toSimple)

}
object UserService {

  val layer: URLayer[UserRepo & PasswordService, UserService] =
    ZLayer.fromFunction { UserService.apply }

}
