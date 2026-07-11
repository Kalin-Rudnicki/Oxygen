package oxygen.example.api

import oxygen.example.api.model.error.*
import oxygen.example.api.model.payment.*
import oxygen.example.api.model.user.*
import oxygen.http.client.DeriveClient
import oxygen.http.core.*
import zio.*

trait PaymentApi derives DeriveClient {

  @route.post("/payment/method/init")
  def initPaymentMethod(
      @param.header authorization: UserToken,
  ): IO[ApiError, InitPaymentMethodResponse]

  @route.post("/payment/method/complete")
  def completePaymentMethod(
      @param.body req: CompletePaymentMethodRequest,
      @param.header authorization: UserToken,
  ): IO[ApiError, PaymentMethod]

  @route.get("/payment/method")
  def getPaymentMethods(
      @param.header authorization: UserToken,
  ): IO[ApiError, GetPaymentMethodsResponse]

}
