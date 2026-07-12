package oxygen.payments.stripe.service

import com.stripe.StripeClient
import com.stripe.model as M
import com.stripe.param as P
import oxygen.payments.stripe.model.*
import oxygen.predef.core.*
import oxygen.schema.JsonSchema
import oxygen.stripe.model.*
import zio.*

final case class LiveStripeService(
    config: LiveStripeService.Config,
    client: StripeClient,
) extends StripeService {
  import LiveStripeService.*

  override def createCustomer(req: CreateCustomerRequest): IO[StripeError, CreateCustomerResponse] =
    (
      for {
        params: P.CustomerCreateParams <- buildCreateCustomer(req)
        rawResponse: M.Customer <- attemptSend { client.v1().customers().create(params) }
        decodedResponse: CreateCustomerResponse <- attemptDecodeIO { decodeCreateCustomerResponse(rawResponse) }
      } yield decodedResponse
    ).mapError(_.withTarget(StripeError.Target("customer", "create")))

  override def createSetupIntent(req: CreateSetupIntentRequest): IO[StripeError, CreateSetupIntentResponse] =
    (
      for {
        params: P.SetupIntentCreateParams <- buildSetupIntent(req)
        rawResponse: M.SetupIntent <- attemptSend { client.v1().setupIntents().create(params) }
        decodedResponse: CreateSetupIntentResponse <- attemptDecodeIO { decodeSetupIntentResponse(rawResponse) }
      } yield decodedResponse
    ).mapError(_.withTarget(StripeError.Target("setupIntent", "create")))

  override def createPaymentIntent(req: CreatePaymentIntentRequest): IO[StripeError, CreatePaymentIntentResponse] =
    (
      for {
        params: P.PaymentIntentCreateParams <- buildPaymentIntent(req)
        rawResponse: M.PaymentIntent <- attemptSend { client.v1().paymentIntents().create(params) }
        decodedResponse: CreatePaymentIntentResponse <- attemptDecodeIO { decodePaymentResponse(rawResponse) }
      } yield decodedResponse
    ).mapError(_.withTarget(StripeError.Target("paymentIntent", "create")))

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
    }.mapError(_.withTarget(StripeError.Target("layer", "init")))

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  extension [A](self: A)
    private def setOpt[B](opt: Option[B])(mod: B => A => A): A =
      opt match
        case Some(value) => mod(value)(self)
        case None        => self

  private def attemptBuild[A: TypeTag as tt](thunk: => A): IO[StripeError.BuildError, A] =
    ZIO.attempt { thunk }.mapError(StripeError.BuildError(None, tt, _))

  private def attemptDecodeIO[E >: StripeError.DecodeError, A: TypeTag as tt](thunk: => IO[E, A]): IO[E, A] =
    ZIO.attempt { thunk }.mapError(StripeError.DecodeError(None, tt, _)).flatten.catchAllDefect { e => ZIO.fail(StripeError.DecodeError(None, tt, e)) }

  private def attemptDecode[A: TypeTag as tt](thunk: => A): IO[StripeError.DecodeError, A] =
    attemptDecodeIO { ZIO.succeed(thunk) }

  private def attemptSend[A](thunk: => A): IO[StripeError.SendError, A] =
    ZIO.attemptBlocking { thunk }.mapError(StripeError.SendError(_))

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Builders
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def buildClient(config: LiveStripeService.Config): IO[StripeError.BuildError, StripeClient] =
    attemptBuild[StripeClient] {
      StripeClient
        .builder()
        .setApiKey(config.secretKey.unwrap)
        .build()
    }

  private def buildCreateCustomer(req: CreateCustomerRequest): IO[StripeError.BuildError, P.CustomerCreateParams] =
    attemptBuild[P.CustomerCreateParams] {
      P.CustomerCreateParams
        .builder()
        .setOpt(req.name) { name => _.setName(name) }
        .setOpt(req.email) { email => _.setEmail(email.toString) }
        .build()
    }

  private def buildSetupIntent(req: CreateSetupIntentRequest): IO[StripeError.BuildError, P.SetupIntentCreateParams] =
    attemptBuild[P.SetupIntentCreateParams] {
      P.SetupIntentCreateParams
        .builder()
        .setCustomer(req.customerId.unwrap)
        .build()
    }

  private def buildPaymentIntent(req: CreatePaymentIntentRequest): IO[StripeError.ChargeNegativeAmount | StripeError.BuildError, P.PaymentIntentCreateParams] =
    ZIO.fail(StripeError.ChargeNegativeAmount(None, req.amount)).unlessDiscard { req.amount.positive } *>
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

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Decoders
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def decodeCreateCustomerResponse(response: M.Customer): IO[StripeError.DecodeError, CreateCustomerResponse] =
    attemptDecode[CreateCustomerResponse] {
      CreateCustomerResponse(
        id = StripeCustomerId.wrap(response.getId),
      )
    }

  private def decodeSetupIntentResponse(response: M.SetupIntent): IO[StripeError.DecodeError, CreateSetupIntentResponse] =
    ??? // FIX-PRE-MERGE (KR) :

  private def decodePaymentResponse(response: M.PaymentIntent): IO[StripeError.DecodeError, CreatePaymentIntentResponse] =
    ??? // FIX-PRE-MERGE (KR) :

}
