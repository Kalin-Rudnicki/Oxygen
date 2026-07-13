package oxygen.example.api.model.payment

import java.time.Instant
import oxygen.example.core.model.*
import oxygen.payments.model.PaymentMethodType
import oxygen.schema.JsonSchema

final case class PaymentMethod(
    id: PaymentMethodId,
    name: Option[String],
    repr: PaymentMethodType,
    createdAt: Instant,
) derives JsonSchema
