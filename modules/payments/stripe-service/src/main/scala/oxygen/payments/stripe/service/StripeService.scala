package oxygen.payments.stripe.service

import oxygen.payments.stripe.model.*
import oxygen.stripe.model.*
import zio.*

trait StripeService {

  def createCustomer(req: CreateCustomerRequest): IO[StripeError, StripeCustomerId]

  def createSetupIntent(customerId: StripeCustomerId): IO[StripeError, StripeSetupIntentClientSecret]

  // FIX-PRE-MERGE (KR) : 
  def createPayment(req: CreatePaymentRequest): IO[StripeError, Any]

}
