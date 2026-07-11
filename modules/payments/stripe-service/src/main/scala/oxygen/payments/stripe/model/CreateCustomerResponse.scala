package oxygen.payments.stripe.model

import oxygen.core.model.currency.*
import oxygen.stripe.model.*

final case class CreateCustomerResponse(
    id: StripeCustomerId,
)
