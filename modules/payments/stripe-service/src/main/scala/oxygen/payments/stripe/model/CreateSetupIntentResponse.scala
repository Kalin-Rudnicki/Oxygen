package oxygen.payments.stripe.model

import oxygen.stripe.model.*

final case class CreateSetupIntentResponse(
    id: StripeSetupIntentId,
    publishableKey: StripePublishableKey,
    clientSecret: StripeSetupIntentClientSecret,
)
