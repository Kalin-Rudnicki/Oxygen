package oxygen.payments.stripe.model

import oxygen.stripe.model.*

final case class CreateCustomerResponse(
    id: StripeCustomerId,
    // FIX-PRE-MERGE (KR) : any other critical fields?
)
