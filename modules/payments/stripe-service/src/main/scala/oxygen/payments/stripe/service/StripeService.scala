package oxygen.payments.stripe.service

import oxygen.payments.stripe.model.*
import oxygen.stripe.model.*
import zio.*

trait StripeService {

  def createCustomer(req: CreateCustomerRequest): IO[StripeError, CreateCustomerResponse]

  def createSetupIntent(req: CreateSetupIntentRequest): IO[StripeError, CreateSetupIntentResponse]

  def retrieveSetupIntent(setupIntentId: StripeSetupIntentId): IO[StripeError, RetrieveSetupIntentResponse]

  def listPaymentMethods(customerId: StripeCustomerId): IO[StripeError, ListPaymentMethodsResponse]

  def createPaymentIntent(req: CreatePaymentIntentRequest): IO[StripeError, CreatePaymentIntentResponse]

}
