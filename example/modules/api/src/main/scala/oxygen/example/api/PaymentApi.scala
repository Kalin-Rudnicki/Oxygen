package oxygen.example.api

import oxygen.example.api.model.error.*
import oxygen.example.api.model.user.*
import oxygen.http.client.DeriveClient
import oxygen.http.core.*
import zio.*

trait PaymentApi derives DeriveClient {

  // FIX-PRE-MERGE (KR) :
  @route.post("/payment/method/init")
  def initPaymentMethod(
      @param.header authorization: UserToken,
  ): IO[ApiError, String]

  // FIX-PRE-MERGE (KR) :
  @route.post("/payment/method/complete")
  def completePaymentMethod(
      @param.header authorization: UserToken,
  ): IO[ApiError, String]

  // FIX-PRE-MERGE (KR) :
  @route.get("/payment/method")
  def getPaymentMethods(
      @param.header authorization: UserToken,
  ): IO[ApiError, String]

}
