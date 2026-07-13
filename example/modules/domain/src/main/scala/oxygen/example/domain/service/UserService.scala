package oxygen.example.domain.service

import oxygen.crypto.service.PasswordService
import oxygen.example.core.model.*
import oxygen.example.domain.model.error.*
import oxygen.example.domain.model.user.*
import oxygen.example.domain.repo.UserRepo
import oxygen.payments.stripe.model.CreateCustomerRequest
import oxygen.payments.stripe.service.StripeService
import oxygen.predef.core.*
import oxygen.stripe.model.StripeCustomerId
import zio.*

final class UserService(userRepo: UserRepo, passwordService: PasswordService, stripeService: StripeService) {

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
        stripeCustomerId = None,
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

  private def genCustomerId(user: FullUser): IO[DomainError, StripeCustomerId] =
    for {
      _ <- ZIO.logInfo(s"Generating a stripe customer for ${user.show}")
      genCustomer <- stripeService.createCustomer(CreateCustomerRequest(user.fullName.some, user.email.some)).orDie // TODO (KR) :
    } yield genCustomer.id

  def ensureStripeCustomer(user: FullUser): IO[DomainError, (FullUser, StripeCustomerId)] =
    user.stripeCustomerId match {
      case Some(stripeCustomerId) => ZIO.succeed { (user, stripeCustomerId) }
      case None                   =>
        for {
          stripeCustomerId <- genCustomerId(user)
          updatedUser = user.copy(stripeCustomerId = stripeCustomerId.some)
          _ <- userRepo.update(updatedUser)
        } yield (updatedUser, stripeCustomerId)
    }

}
object UserService {

  val layer: URLayer[UserRepo & PasswordService & StripeService, UserService] =
    ZLayer.fromFunction { UserService.apply }

}
