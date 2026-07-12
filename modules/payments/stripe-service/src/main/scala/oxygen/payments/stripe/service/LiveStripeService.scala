package oxygen.payments.stripe.service

import com.stripe.StripeClient
import com.stripe.model as M
import com.stripe.net.RequestOptions
import com.stripe.param as P
import java.time.YearMonth
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
        decodedResponse: CreateSetupIntentResponse <- attemptDecodeIO {
          decodeSetupIntentResponse(config.publishableKey, rawResponse)
        }
      } yield decodedResponse
    ).mapError(_.withTarget(StripeError.Target("setupIntent", "create")))

  override def retrieveSetupIntent(setupIntentId: StripeSetupIntentId): IO[StripeError, RetrieveSetupIntentResponse] =
    (
      for {
        rawResponse: M.SetupIntent <- attemptSend { client.v1().setupIntents().retrieve(setupIntentId.unwrap) }
        decodedResponse: RetrieveSetupIntentResponse <- attemptDecodeIO { decodeRetrieveSetupIntentResponse(rawResponse) }
      } yield decodedResponse
    ).mapError(_.withTarget(StripeError.Target("setupIntent", "retrieve")))

  override def listPaymentMethods(customerId: StripeCustomerId): IO[StripeError, ListPaymentMethodsResponse] =
    (
      for {
        params: P.PaymentMethodListParams <- buildListPaymentMethods(customerId)
        rawResponse <- attemptSend { client.v1().paymentMethods().list(params) }
        decodedResponse: ListPaymentMethodsResponse <- attemptDecodeIO { decodeListPaymentMethodsResponse(rawResponse) }
      } yield decodedResponse
    ).mapError(_.withTarget(StripeError.Target("paymentMethod", "list")))

  override def createPaymentIntent(req: CreatePaymentIntentRequest): IO[StripeError, CreatePaymentIntentResponse] =
    (
      for {
        params: P.PaymentIntentCreateParams <- buildPaymentIntent(req)
        rawResponse: M.PaymentIntent <- attemptSend {
          req.idempotencyKey match {
            case Some(key) =>
              client.v1().paymentIntents().create(params, RequestOptions.builder().setIdempotencyKey(key).build())
            case None =>
              client.v1().paymentIntents().create(params)
          }
        }
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
    ZIO.attempt { thunk }.mapError(StripeError.DecodeError(None, tt, _)).flatten.catchAllDefect { e =>
      ZIO.fail(StripeError.DecodeError(None, tt, e))
    }

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
        .setUsage(P.SetupIntentCreateParams.Usage.OFF_SESSION)
        .setAutomaticPaymentMethods(
          P.SetupIntentCreateParams.AutomaticPaymentMethods
            .builder()
            .setEnabled(true)
            .build(),
        )
        .build()
    }

  private def buildListPaymentMethods(customerId: StripeCustomerId): IO[StripeError.BuildError, P.PaymentMethodListParams] =
    attemptBuild[P.PaymentMethodListParams] {
      P.PaymentMethodListParams
        .builder()
        .setCustomer(customerId.unwrap)
        .setType(P.PaymentMethodListParams.Type.CARD)
        .build()
    }

  private def buildPaymentIntent(
      req: CreatePaymentIntentRequest,
  ): IO[StripeError.ChargeNegativeAmount | StripeError.BuildError, P.PaymentIntentCreateParams] =
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
        id = StripeCustomerId.wrap(requireNonNull("id", response.getId)),
      )
    }

  private def decodeSetupIntentResponse(
      publishableKey: StripePublishableKey,
      response: M.SetupIntent,
  ): IO[StripeError.DecodeError, CreateSetupIntentResponse] =
    attemptDecode[CreateSetupIntentResponse] {
      CreateSetupIntentResponse(
        id = StripeSetupIntentId.wrap(requireNonNull("id", response.getId)),
        publishableKey = publishableKey,
        clientSecret = StripeSetupIntentClientSecret.wrap(requireNonNull("client_secret", response.getClientSecret)),
      )
    }

  private def decodeRetrieveSetupIntentResponse(response: M.SetupIntent): IO[StripeError.DecodeError, RetrieveSetupIntentResponse] =
    attemptDecode[RetrieveSetupIntentResponse] {
      RetrieveSetupIntentResponse(
        id = StripeSetupIntentId.wrap(requireNonNull("id", response.getId)),
        status = requireNonNull("status", response.getStatus),
        paymentMethodId = Option(response.getPaymentMethod).map(StripePaymentMethodId.wrap),
      )
    }

  private def decodeListPaymentMethodsResponse(
      response: com.stripe.model.StripeCollection[M.PaymentMethod],
  ): IO[StripeError.DecodeError, ListPaymentMethodsResponse] =
    attemptDecode[ListPaymentMethodsResponse] {
      val methods =
        Option(response.getData).toList.flatMap { javaList =>
          import scala.jdk.CollectionConverters.*
          javaList.asScala.toList
        }.map(decodePaymentMethodInfo)

      ListPaymentMethodsResponse(paymentMethods = methods)
    }

  private def decodePaymentMethodInfo(pm: M.PaymentMethod): PaymentMethodInfo =
    PaymentMethodInfo(
      id = StripePaymentMethodId.wrap(requireNonNull("id", pm.getId)),
      card = Option(pm.getCard).map { card =>
        val year: Long = requireNonNullLong("card.exp_year", card.getExpYear)
        val month: Long = requireNonNullLong("card.exp_month", card.getExpMonth)
        val yearMonth: YearMonth = YearMonth.of(year.toInt, month.toInt)

        CardDisplay(
          brand = requireNonNull("card.brand", card.getBrand),
          last4 = requireNonNull("card.last4", card.getLast4),
          expiry = yearMonth,
        )
      },
    )

  private def decodePaymentResponse(response: M.PaymentIntent): IO[StripeError.DecodeError, CreatePaymentIntentResponse] =
    attemptDecode {
      val paymentIntentId = StripePaymentIntentId.wrap(requireNonNull("id", response.getId))
      val status = requireNonNull("status", response.getStatus)

      status match {
        case "succeeded" =>
          CreatePaymentIntentResponse.Succeeded(paymentIntentId)

        case "processing" =>
          CreatePaymentIntentResponse.Processing(paymentIntentId)

        case "requires_action" =>
          CreatePaymentIntentResponse.RequiresAction(
            paymentIntentId = paymentIntentId,
            clientSecret = StripePaymentIntentClientSecret.wrap(requireNonNull("client_secret", response.getClientSecret)),
          )

        case "requires_payment_method" =>
          val lastError = Option(response.getLastPaymentError)
          CreatePaymentIntentResponse.RequiresPaymentMethod(
            paymentIntentId = paymentIntentId,
            failureCode = lastError.flatMap(e => Option(e.getCode)),
            failureMessage = lastError.flatMap(e => Option(e.getMessage)),
          )

        case "canceled" =>
          CreatePaymentIntentResponse.Canceled(paymentIntentId)

        case other =>
          CreatePaymentIntentResponse.UnexpectedStatus(paymentIntentId, other)
      }
    }

  private def requireNonNull(field: String, value: String): String =
    Option(value).getOrElse {
      throw new IllegalArgumentException(s"missing required field: $field")
    }

  private def requireNonNullLong(field: String, value: java.lang.Long): Long =
    Option(value).map(_.longValue).getOrElse {
      throw new IllegalArgumentException(s"missing required field: $field")
    }

}
