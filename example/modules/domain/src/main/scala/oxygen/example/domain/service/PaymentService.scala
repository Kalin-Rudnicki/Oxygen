package oxygen.example.domain.service

import oxygen.core.model.currency.PreciseMoney
import oxygen.example.core.model.*
import oxygen.example.domain.model.error.*
import oxygen.example.domain.model.payment.*
import oxygen.example.domain.model.user.*
import oxygen.example.domain.repo.*
import oxygen.payments.stripe.model.{CreateCustomerRequest, CreatePaymentIntentRequest, CreatePaymentIntentResponse, CreateSetupIntentRequest}
import oxygen.payments.stripe.service.StripeService
import oxygen.predef.core.*
import oxygen.stripe.model.*
import zio.*

final class PaymentService(
    userRepo: UserRepo,
    stripeService: StripeService,
    initPaymentMethodRepo: InitPaymentMethodRepo,
    paymentMethodRepo: PaymentMethodRepo,
    paymentRepo: PaymentRepo,
) {

  def ensureStripeCustomer(user: FullUser): IO[DomainError, FullUser.WithStripe] =
    user match {
      case user: FullUser.WithStripe    => ZIO.logDebug(s"${user.show} is already stripe-initialized").as { user }
      case user: FullUser.WithoutStripe =>
        for {
          stripeCustomerId <- initCustomerId(user)
          updatedUser = user.withStripeCustomerId(stripeCustomerId)
          _ <- userRepo.update(updatedUser)
        } yield updatedUser
    }

  def initPaymentMethod(user: FullUser.WithStripe): IO[DomainError, (key: StripePublishableKey, init: InitPaymentMethod)] =
    for {
      _ <- ZIO.logInfo(s"Initializing payment method for ${user.show}")
      now <- Clock.instant
      initId <- Random.nextUUID.map(InitPaymentMethodId(_))
      stripeInit <- stripeService.createSetupIntent(CreateSetupIntentRequest(user.stripeCustomerId)).orDie // TODO (KR) :
      initPaymentMethod = InitPaymentMethod(
        id = initId,
        userId = user.id,
        stripeId = stripeInit.id,
        clientSecret = stripeInit.clientSecret,
        createdAt = now,
        completedAt = None,
      )
      _ <- initPaymentMethodRepo.insert(initPaymentMethod)
    } yield (key = stripeInit.publishableKey, init = initPaymentMethod)

  def completePaymentMethod(userId: UserId, initId: InitPaymentMethodId): IO[DomainError, PaymentMethod] =
    for {
      now <- Clock.instant
      id <- Random.nextUUID.map(PaymentMethodId(_))
      init <- initPaymentMethodRepo.findByKey(initId).map(_.filter(_.userId == userId)).someOrElseZIO { ZIO.dieMessage(s"Invalid [userId: $userId] [initId: $initId]") }
      _ <- ZIO.dieMessage(s"Payment already initialized [userId: $userId] [initId: $initId]").whenDiscard { init.completedAt.nonEmpty }
      stripePaymentMethod <- stripeService.getPaymentMethodFromSetupIntent(init.stripeId).orDie // TODO (KR) :
      pms <- paymentMethodRepo.paymentMethodsForUser(userId)
      paymentMethod = PaymentMethod(
        id = id,
        userId = userId,
        stripeId = stripePaymentMethod.id,
        name = None,
        repr = stripePaymentMethod.methodType,
        ord = pms.size,
        createdAt = now,
      )
      _ <- paymentMethodRepo.insert(paymentMethod)
    } yield paymentMethod

  def getPaymentMethods(userId: UserId): UIO[Seq[PaymentMethod]] =
    paymentMethodRepo.paymentMethodsForUser(userId)

  def charge(user: FullUser.WithStripe, paymentMethod: PaymentMethod, amount: PreciseMoney, description: String): IO[DomainError, Payment] =
    for {
      now <- Clock.instant
      id <- Random.nextUUID.map(PaymentId(_))
      stripeRequest = CreatePaymentIntentRequest(
        customerId = user.stripeCustomerId,
        paymentMethodId = paymentMethod.stripeId,
        amount = amount,
        description = description,
        email = user.email.some,
        idempotencyKey = id.toString.some,
      )
      stripeResponse <- stripeService.createPaymentIntent(stripeRequest).orDie // TODO (KR) :
      // TODO (KR) : should do way better than this - just an example
      status = stripeResponse match
        case _: CreatePaymentIntentResponse.Succeeded                => "success"
        case _: CreatePaymentIntentResponse.Processing               => "processing"
        case _: CreatePaymentIntentResponse.RequiresAction           => "requires-action"
        case _: CreatePaymentIntentResponse.RequiresPaymentMethod    => "requires-payment-method"
        case _: CreatePaymentIntentResponse.Canceled                 => "cancelled"
        case CreatePaymentIntentResponse.UnexpectedStatus(_, status) => s"unexpected:$status"
      payment = Payment(
        id = id,
        userId = user.id,
        paymentMethodId = paymentMethod.id,
        stripeId = stripeResponse.paymentIntentId,
        amount = amount,
        description = description,
        status = status,
        createdAt = now,
      )
      _ <- paymentRepo.insert(payment)
    } yield payment

  /////// Helpers ///////////////////////////////////////////////////////////////

  private def initCustomerId(user: FullUser): IO[DomainError, StripeCustomerId] =
    for {
      _ <- ZIO.logInfo(s"Generating a stripe customer for ${user.show}")
      genCustomer <- stripeService.createCustomer(CreateCustomerRequest(user.fullName.some, user.email.some)).orDie // TODO (KR) :
    } yield genCustomer.id

}
object PaymentService {

  val layer: URLayer[UserRepo & StripeService & InitPaymentMethodRepo & PaymentMethodRepo & PaymentRepo, PaymentService] =
    ZLayer.fromFunction { PaymentService.apply }

}
