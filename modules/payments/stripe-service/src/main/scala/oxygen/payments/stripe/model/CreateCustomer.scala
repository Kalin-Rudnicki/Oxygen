package oxygen.payments.stripe.model

import oxygen.core.model.Email
import oxygen.schema.JsonSchema

final case class CreateCustomer(
    name: Option[String],
    email: Option[Email],
) derives JsonSchema
