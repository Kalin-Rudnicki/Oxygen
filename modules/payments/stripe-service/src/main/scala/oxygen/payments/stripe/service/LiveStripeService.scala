package oxygen.payments.stripe.service

import com.stripe.StripeClient
import com.stripe.model as M
import com.stripe.param as P
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
  import LiveStripeService.*

  override def createCustomer(req: CreateCustomerRequest): IO[StripeError, CreateCustomerResponse] =
    ??? // FIX-PRE-MERGE (KR) :

  override def createSetupIntent(customerId: StripeCustomerId): IO[StripeError, StripeSetupIntentClientSecret] =
    ??? // FIX-PRE-MERGE (KR) :

  override def createPayment(req: CreatePaymentRequest): IO[StripeError, CreatePaymentResponse] =
    (
      for {
        params: P.PaymentIntentCreateParams <- buildPaymentIntent(req)
        response: M.PaymentIntent <- attemptSend { client.v1().paymentIntents().create(params) }
      } yield ??? // FIX-PRE-MERGE (KR) :
    ).mapError(_.withTarget(StripeError.Target("payment", "create")))

}
object LiveStripeService {

  final case class Config(
      secretKey: StripeSecretKey,
      publishableKey: StripePublishableKey,
  ) derives JsonSchema

  val layer: ZLayer[LiveStripeService.Config, StripeError, LiveStripeService] =
    ZLayer {
      for {
        config <- ZIO.service[LiveStripeService.Config]
        client <- buildClient(config)
      } yield LiveStripeService(config, client)
    }.mapError(_.withTarget(StripeError.Target("payment", "create")))

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Builders
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  extension [A](self: A)
    private def setOpt[B](opt: Option[B])(mod: B => A => A): A =
      opt match
        case Some(value) => mod(value)(self)
        case None        => self

  private def attemptBuild[A: TypeTag as tt](thunk: => A): IO[StripeError.BuildError, A] =
    ZIO.attempt { thunk }.mapError(StripeError.BuildError(None, tt, _))

  private def attemptSend[A](thunk: => A): IO[StripeError.SendError, A] =
    ZIO.attemptBlocking { thunk }.mapError(StripeError.SendError(_))

  private def buildClient(config: LiveStripeService.Config): IO[StripeError, StripeClient] =
    attemptBuild[StripeClient] {
      ??? // FIX-PRE-MERGE (KR) :
    }

  private def buildPaymentIntent(req: CreatePaymentRequest): IO[StripeError.BuildError, P.PaymentIntentCreateParams] =
    ZIO.dieMessage("What are you doing trying to charge a negative amount....").unlessDiscard { req.amount.positive } *>
      attemptBuild[P.PaymentIntentCreateParams] {
        P.PaymentIntentCreateParams
          .builder()
          .setCurrency(req.amount.currencyCode.code)
          .setAmount(req.amount.unsignedFractionalUnits)
          .setCustomer(req.customerId.unwrap)
          .setPaymentMethod(req.paymentMethodId.unwrap)
          .setDescription(req.description)
          .setOpt(req.email) { email => _.setReceiptEmail(email.toString) }
          .setConfirm(true)
          .setOffSession(true)
          .build()
      }

}
