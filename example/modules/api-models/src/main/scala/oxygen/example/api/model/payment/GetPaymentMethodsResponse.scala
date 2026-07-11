package oxygen.example.api.model.payment

import oxygen.schema.JsonSchema

final case class GetPaymentMethodsResponse(
    paymentMethods: Seq[PaymentMethod],
) derives JsonSchema
