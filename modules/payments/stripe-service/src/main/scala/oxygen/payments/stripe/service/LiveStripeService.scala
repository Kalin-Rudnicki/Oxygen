package oxygen.payments.stripe.service

import com.stripe.StripeClient
import com.stripe.model as STRIPE
import oxygen.payments.stripe.model.*
import oxygen.predef.core.*
import oxygen.schema.JsonSchema
import oxygen.stripe.model.*
import zio.*

// FIX-PRE-MERGE (KR) :
final case class LiveStripeService(
    config: LiveStripeService.Config,
    client: StripeClient,
) extends StripeService {

  override def createCustomer(req: CreateCustomerRequest): IO[StripeError, CreateCustomerResponse] =
    ??? // FIX-PRE-MERGE (KR) :

  override def createSetupIntent(customerId: StripeCustomerId): IO[StripeError, StripeSetupIntentClientSecret] =
    ??? // FIX-PRE-MERGE (KR) :

  override def createPayment(req: CreatePaymentRequest): IO[StripeError, CreatePaymentResponse] =
    for {
      stripeReq <- LiveStripeService.buildPaymentIntent(req)
    } yield ???

}
object LiveStripeService {

  final case class Config(
      secretKey: StripeSecretKey,
      publishableKey: StripePublishableKey,
  ) derives JsonSchema

  val layer: RLayer[LiveStripeService.Config, LiveStripeService] =
    ZLayer {
      for {
        config <- ZIO.service[LiveStripeService.Config]
        client <- buildClient(config)
      } yield LiveStripeService(config, client)
    }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Builders
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def attemptBuild[A: TypeTag as tt](thunk: => A): IO[StripeError.BuildError, A] =
    ZIO.attempt { thunk }.mapError(StripeError.BuildError(tt, _))

  private def attemptSend[A]()(thunk: => A): IO[StripeError.ApiError, A] =
    ZIO.attempt { thunk }.orDie // FIX-PRE-MERGE (KR) :

  private def buildClient(config: LiveStripeService.Config): Task[StripeClient] =
    attemptBuild[StripeClient] {
      ??? // FIX-PRE-MERGE (KR) :
    }

  private def buildPaymentIntent(in: CreatePaymentRequest): IO[StripeError.BuildError, STRIPE.PaymentIntent] =
    attemptBuild[STRIPE.PaymentIntent] {
      ??? // FIX-PRE-MERGE (KR) :
    }

}
