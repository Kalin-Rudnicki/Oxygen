package oxygen.payments.stripe.service

import oxygen.payments.stripe.model.*
import oxygen.stripe.model.*
import zio.*

trait StripeService {

  def createCustomer(req: CreateCustomerRequest): IO[StripeError, CreateCustomerResponse]

  def createSetupIntent(req: CreateSetupIntentRequest): IO[StripeError, CreateSetupIntentResponse]

  def retrieveSetupIntent(setupIntentId: StripeSetupIntentId): IO[StripeError, RetrieveSetupIntentResponse]

  /** After FE `confirmSetup`, resolve the vaulted PM. Fails with [[StripeError.SetupIntentMissingPaymentMethod]] if none yet. */
  def getPaymentMethodFromSetupIntent(setupIntentId: StripeSetupIntentId): IO[StripeError, PaymentMethodInfo]

  def listPaymentMethods(customerId: StripeCustomerId): IO[StripeError, ListPaymentMethodsResponse]

  def createPaymentIntent(req: CreatePaymentIntentRequest): IO[StripeError, CreatePaymentIntentResponse]

}
object StripeService {

  val unimplementedLayer: ULayer[StripeService] = ZLayer.succeed { StripeService.Unimplemented }

  object Unimplemented extends StripeService {

    override def createCustomer(req: CreateCustomerRequest): IO[StripeError, CreateCustomerResponse] =
      ZIO.dieMessage("StripeService.Unimplemented")

    override def createSetupIntent(req: CreateSetupIntentRequest): IO[StripeError, CreateSetupIntentResponse] =
      ZIO.dieMessage("StripeService.Unimplemented")

    override def retrieveSetupIntent(setupIntentId: StripeSetupIntentId): IO[StripeError, RetrieveSetupIntentResponse] =
      ZIO.dieMessage("StripeService.Unimplemented")

    override def getPaymentMethodFromSetupIntent(setupIntentId: StripeSetupIntentId): IO[StripeError, PaymentMethodInfo] =
      ZIO.dieMessage("StripeService.Unimplemented")

    override def listPaymentMethods(customerId: StripeCustomerId): IO[StripeError, ListPaymentMethodsResponse] =
      ZIO.dieMessage("StripeService.Unimplemented")

    override def createPaymentIntent(req: CreatePaymentIntentRequest): IO[StripeError, CreatePaymentIntentResponse] =
      ZIO.dieMessage("StripeService.Unimplemented")

  }

}
