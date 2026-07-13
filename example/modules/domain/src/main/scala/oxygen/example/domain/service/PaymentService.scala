package oxygen.example.domain.service

import oxygen.example.core.model.*
import oxygen.example.domain.model.error.*
import oxygen.example.domain.model.payment.*
import oxygen.example.domain.model.user.*
import oxygen.example.domain.repo.UserRepo
import oxygen.payments.stripe.model.{CreateCustomerRequest, CreateSetupIntentRequest}
import oxygen.payments.stripe.service.StripeService
import oxygen.predef.core.*
import oxygen.stripe.model.*
import zio.*

final class PaymentService(
    userRepo: UserRepo,
    stripeService: StripeService,
) {

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

  def initPaymentMethod(userId: UserId, customerId: StripeCustomerId): IO[DomainError, (key: StripePublishableKey, init: InitPaymentMethod)] =
    for {
      now <- Clock.instant
      initId <- Random.nextUUID.map(InitPaymentMethodId(_))
      stripeInit <- stripeService.createSetupIntent(CreateSetupIntentRequest(customerId)).orDie // TODO (KR) :
    } yield (
      key = stripeInit.publishableKey,
      init = InitPaymentMethod(
        id = initId,
        userId = userId,
        clientSecret = stripeInit.clientSecret,
        createdAt = now,
      ),
    )

  /////// Helpers ///////////////////////////////////////////////////////////////

  private def genCustomerId(user: FullUser): IO[DomainError, StripeCustomerId] =
    for {
      _ <- ZIO.logInfo(s"Generating a stripe customer for ${user.show}")
      genCustomer <- stripeService.createCustomer(CreateCustomerRequest(user.fullName.some, user.email.some)).orDie // TODO (KR) :
    } yield genCustomer.id

}
object PaymentService {

  val layer: URLayer[UserRepo & StripeService, PaymentService] =
    ZLayer.fromFunction { PaymentService.apply }

}
