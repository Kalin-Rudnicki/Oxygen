package oxygen.example.domain.service

import oxygen.example.core.model.*
import oxygen.example.domain.model.error.*
import oxygen.example.domain.model.payment.*
import oxygen.example.domain.model.user.*
import oxygen.example.domain.repo.*
import oxygen.payments.stripe.model.{CreateCustomerRequest, CreateSetupIntentRequest}
import oxygen.payments.stripe.service.StripeService
import oxygen.predef.core.*
import oxygen.stripe.model.*
import zio.*

final class PaymentService(
    userRepo: UserRepo,
    stripeService: StripeService,
    initPaymentRepo: InitPaymentRepo,
    paymentRepo: PaymentRepo,
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
      initPaymentMethod = InitPaymentMethod(
        id = initId,
        userId = userId,
        stripeId = stripeInit.id,
        clientSecret = stripeInit.clientSecret,
        createdAt = now,
        completedAt = None,
      )
      _ <- initPaymentRepo.insert(initPaymentMethod)
    } yield (key = stripeInit.publishableKey, init = initPaymentMethod)

  def completePaymentMethod(userId: UserId, initId: InitPaymentMethodId): IO[DomainError, PaymentMethod] =
    for {
      now <- Clock.instant
      id <- Random.nextUUID.map(PaymentMethodId(_))
      init <- initPaymentRepo.findByKey(initId).map(_.filter(_.userId == userId)).someOrElseZIO { ZIO.dieMessage(s"Invalid [userId: $userId] [initId: $initId]") }
      _ <- ZIO.dieMessage(s"Payment already initialized [userId: $userId] [initId: $initId]").whenDiscard { init.completedAt.nonEmpty }
      stripePaymentMethod <- stripeService.getPaymentMethodFromSetupIntent(init.stripeId).orDie // TODO (KR) :
      pms <- paymentRepo.paymentMethodsForUser(userId)
      paymentMethod = PaymentMethod(
        id = id,
        userId = userId,
        stripeId = stripePaymentMethod.id,
        name = None,
        repr = stripePaymentMethod.methodType,
        ord = pms.size,
        createdAt = now,
      )
      _ <- paymentRepo.insert(paymentMethod)
    } yield paymentMethod

  /////// Helpers ///////////////////////////////////////////////////////////////

  private def genCustomerId(user: FullUser): IO[DomainError, StripeCustomerId] =
    for {
      _ <- ZIO.logInfo(s"Generating a stripe customer for ${user.show}")
      genCustomer <- stripeService.createCustomer(CreateCustomerRequest(user.fullName.some, user.email.some)).orDie // TODO (KR) :
    } yield genCustomer.id

}
object PaymentService {

  val layer: URLayer[UserRepo & StripeService & InitPaymentRepo & PaymentRepo, PaymentService] =
    ZLayer.fromFunction { PaymentService.apply }

}
