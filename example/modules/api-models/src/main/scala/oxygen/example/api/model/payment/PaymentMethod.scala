package oxygen.example.api.model.payment

import java.time.Instant
import oxygen.example.core.model.*
import oxygen.schema.JsonSchema

final case class PaymentMethod(
    id: PaymentMethodId,
    name: Option[String],
    // FIX-PRE-MERGE (KR) :
    createdAt: Instant,
) derives JsonSchema
