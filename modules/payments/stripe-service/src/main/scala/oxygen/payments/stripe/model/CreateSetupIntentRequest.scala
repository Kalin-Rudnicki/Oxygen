package oxygen.payments.stripe.model

import oxygen.stripe.model.*

final case class CreateSetupIntentRequest(
    customerId: StripeCustomerId,
)
