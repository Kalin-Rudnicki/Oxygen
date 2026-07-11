package oxygen.payments.stripe.model

final case class ListPaymentMethodsResponse(
    paymentMethods: List[PaymentMethodInfo],
)
