package oxygen.example.webServer.api

import oxygen.example.api.PaymentApi
import oxygen.example.api.model.error.*
import oxygen.example.api.model.payment.*
import oxygen.example.api.model.user.*
import oxygen.example.api.service.TokenService
import oxygen.example.domain.model.error.DomainError
import oxygen.example.domain.service.*
import oxygen.http.server.CurrentRequest
import zio.*

final case class PaymentApiImpl(
    tokenService: TokenService,
    userService: UserService,
    paymentService: PaymentService,
) extends PaymentApi {

  override def initPaymentMethod(
      authorization: UserToken,
  ): IO[ApiError, InitPaymentMethodResponse] =
    CurrentRequest.handle[DomainError, ApiError] {
      for {
        tokenUser <- tokenService.validateToken(authorization)
        user <- userService.getFullUser(tokenUser.id)
        (user, customerId) <- paymentService.ensureStripeCustomer(user)
        initRes <- paymentService.initPaymentMethod(user.id, customerId)
      } yield InitPaymentMethodResponse(
        id = initRes.init.id,
        publishableKey = initRes.key,
        clientSecret = initRes.init.clientSecret,
      )
    }

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

  val layer: URLayer[TokenService & UserService & PaymentService, PaymentApi] =
    ZLayer.fromFunction { PaymentApiImpl.apply }

}
