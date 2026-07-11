package oxygen.example.api.model.payment

import oxygen.example.core.model.*
import oxygen.schema.JsonSchema

final case class CompletePaymentMethodRequest(
    id: InitPaymentMethodId,
) derives JsonSchema
