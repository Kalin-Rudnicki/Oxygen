package oxygen.payments.stripe.service

import oxygen.payments.stripe.model.*
import oxygen.stripe.model.*
import zio.*

trait StripeService {

  def createCustomer(req: CreateCustomer): IO[StripeError, StripeCustomerId]

  def createSetupIntent(customerId: StripeCustomerId): IO[StripeError, StripeSetupIntentClientSecret]

}
