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
