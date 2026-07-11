package oxygen.payments.stripe.model

import oxygen.core.model.Email

final case class CreateCustomerRequest(
    name: Option[String],
    email: Option[Email],
)
