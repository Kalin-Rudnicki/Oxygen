package oxygen.example.webServer.api

import oxygen.example.api.PaymentApi
import oxygen.example.api.model.error.*
import oxygen.example.api.model.payment.*
import oxygen.example.api.model.user.*
import zio.*

final case class PaymentApiImpl() extends PaymentApi {

  override def initPaymentMethod(
      authorization: UserToken,
  ): IO[ApiError, InitPaymentMethodResponse] =
    ??? // FIX-PRE-MERGE (KR) :

  override def completePaymentMethod(
      req: CompletePaymentMethodRequest,
      authorization: UserToken,
  ): IO[ApiError, CompletePaymentMethodResponse] =
    ??? // FIX-PRE-MERGE (KR) :

  override def getPaymentMethods(
      authorization: UserToken,
  ): IO[ApiError, GetPaymentMethodsResponse] =
    ??? // FIX-PRE-MERGE (KR) :

}
object PaymentApiImpl {

  val layer: ULayer[PaymentApi] =
    ZLayer.succeed { PaymentApiImpl() }

}
