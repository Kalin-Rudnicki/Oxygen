package oxygen.example.webServer.api

import oxygen.example.api.PaymentApi
import oxygen.example.api.model.error.*
import oxygen.example.api.model.payment.*
import oxygen.example.api.model.user.*
import oxygen.example.api.service.TokenService
import oxygen.example.domain.model.error.DomainError
import oxygen.example.domain.model as Domain
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
        user <- paymentService.ensureStripeCustomer(user)
        initRes <- paymentService.initPaymentMethod(user)
      } yield InitPaymentMethodResponse(
        id = initRes.init.id,
        publishableKey = initRes.key,
        clientSecret = initRes.init.clientSecret,
      )
    }

  override def completePaymentMethod(
      req: CompletePaymentMethodRequest,
      authorization: UserToken,
  ): IO[ApiError, PaymentMethod] =
    CurrentRequest.handle[DomainError, ApiError] {
      for {
        tokenUser <- tokenService.validateToken(authorization)
        paymentMethod <- paymentService.completePaymentMethod(tokenUser.id, req.id)

        // FIX-PRE-MERGE (KR) : remove - just for testing...

      } yield paymentMethod.toApi
    }

  override def getPaymentMethods(
      authorization: UserToken,
  ): IO[ApiError, GetPaymentMethodsResponse] =
    CurrentRequest.handle[DomainError, ApiError] {
      for {
        tokenUser <- tokenService.validateToken(authorization)
        paymentMethod <- paymentService.getPaymentMethods(tokenUser.id)
      } yield GetPaymentMethodsResponse(paymentMethod.map(_.toApi))
    }

  extension (self: Domain.payment.PaymentMethod)
    private def toApi: PaymentMethod =
      PaymentMethod(
        id = self.id,
        name = self.name,
        repr = self.repr,
        createdAt = self.createdAt,
      )

}
object PaymentApiImpl {

  val layer: URLayer[TokenService & UserService & PaymentService, PaymentApi] =
    ZLayer.fromFunction { PaymentApiImpl.apply }

}
