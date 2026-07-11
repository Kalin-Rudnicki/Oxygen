package oxygen.payments.stripe.service

import oxygen.payments.stripe.model.*
import oxygen.stripe.model.*
import zio.*

trait StripeService {

  def createCustomer(req: CreateCustomerRequest): IO[StripeError, CreateCustomerResponse]

  def createSetupIntent(customerId: StripeCustomerId): IO[StripeError, StripeSetupIntentClientSecret]

  def createPayment(req: CreatePaymentRequest): IO[StripeError, CreatePaymentResponse]

}
