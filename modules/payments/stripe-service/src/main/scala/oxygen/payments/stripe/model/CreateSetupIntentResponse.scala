package oxygen.payments.stripe.model

import oxygen.stripe.model.*

final case class CreateSetupIntentResponse(
    publishableKey: StripePublishableKey,
    clientSecret: StripeSetupIntentClientSecret,
)
