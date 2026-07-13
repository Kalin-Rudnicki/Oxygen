package oxygen.example.api.model.payment

import oxygen.example.core.model.*
import oxygen.schema.JsonSchema
import oxygen.stripe.model.*

final case class InitPaymentMethodResponse(
    id: InitPaymentMethodId,
    publishableKey: StripePublishableKey,
    clientSecret: StripeSetupIntentClientSecret,
) derives JsonSchema
