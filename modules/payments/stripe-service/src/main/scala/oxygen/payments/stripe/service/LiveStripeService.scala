package oxygen.payments.stripe.service

import com.stripe.StripeClient
import com.stripe.model as M
import com.stripe.net.RequestOptions
import com.stripe.param as P
import java.time.YearMonth
import oxygen.payments.model.PaymentMethodType
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

  override def getPaymentMethodFromSetupIntent(setupIntentId: StripeSetupIntentId): IO[StripeError, PaymentMethodInfo] =
    (
      for {
        retrieveParams: P.SetupIntentRetrieveParams <- buildRetrieveSetupIntentExpandPaymentMethod
        setupIntent: M.SetupIntent <- attemptSend {
          client.v1().setupIntents().retrieve(setupIntentId.unwrap, retrieveParams)
        }
        paymentMethod: M.PaymentMethod <- resolvePaymentMethodFromSetupIntent(client, setupIntent)
        decoded: PaymentMethodInfo <- attemptDecode { decodePaymentMethodInfo(paymentMethod) }
      } yield decoded
    ).mapError(_.withTarget(StripeError.Target("setupIntent", "getPaymentMethod")))

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

  final case class LiveOrUnimplementedConfig(config: Option[LiveStripeService.Config])
  object LiveOrUnimplementedConfig {
    given JsonSchema[LiveOrUnimplementedConfig] = JsonSchema.deriveWrapped
  }

  private def make(config: LiveStripeService.Config): IO[StripeError, LiveStripeService] =
    buildClient(config).mapBoth(
      _.withTarget(StripeError.Target("layer", "init")),
      LiveStripeService(config, _),
    )

  val layer: ZLayer[LiveStripeService.Config, StripeError, LiveStripeService] =
    ZLayer { ZIO.serviceWithZIO[LiveStripeService.Config](make) }

  val optionallyUnimplementedLayer: ZLayer[LiveStripeService.LiveOrUnimplementedConfig, StripeError, StripeService] =
    ZLayer {
      ZIO.serviceWith[LiveStripeService.LiveOrUnimplementedConfig](_.config).flatMap {
        case Some(config) => make(config)
        case None         => ZIO.logWarning("No live stripe config - defaulting to `StripeService.Unimplemented`").as { StripeService.Unimplemented }
      }
    }

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
        .build()
    }

  private def buildRetrieveSetupIntentExpandPaymentMethod: IO[StripeError.BuildError, P.SetupIntentRetrieveParams] =
    attemptBuild[P.SetupIntentRetrieveParams] {
      P.SetupIntentRetrieveParams
        .builder()
        .addExpand("payment_method")
        .build()
    }

  private def resolvePaymentMethodFromSetupIntent(
      client: StripeClient,
      setupIntent: M.SetupIntent,
  ): IO[StripeError.SetupIntentMissingPaymentMethod | StripeError.SendError, M.PaymentMethod] = {
    val setupIntentId = StripeSetupIntentId.wrap(requireNonNull("id", setupIntent.getId))
    val status = Option(setupIntent.getStatus).getOrElse("unknown")

    Option(setupIntent.getPaymentMethodObject)
      .map(ZIO.succeed(_))
      .orElse {
        Option(setupIntent.getPaymentMethod).map { paymentMethodId =>
          attemptSend { client.v1().paymentMethods().retrieve(paymentMethodId) }
        }
      }
      .getOrElse(ZIO.fail(StripeError.SetupIntentMissingPaymentMethod(None, setupIntentId, status)))
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

  private def decodePaymentMethodInfo(pm: M.PaymentMethod): PaymentMethodInfo = {
    val id = StripePaymentMethodId.wrap(requireNonNull("id", pm.getId))
    val methodType: PaymentMethodType =
      requireNonNull("type", pm.getType) match {
        case "card" =>
          val card = Option(pm.getCard).getOrElse {
            throw new IllegalArgumentException("payment method type=card but card object is missing")
          }
          val brand = requireNonNull("card.brand", card.getBrand)
          val last4 = requireNonNull("card.last4", card.getLast4)
          val expiry = YearMonth.of(
            requireNonNullLong("card.exp_year", card.getExpYear).toInt,
            requireNonNullLong("card.exp_month", card.getExpMonth).toInt,
          )
          val funding = PaymentMethodInfo.parseCardFunding(card.getFunding)
          val wallet =
            Option(card.getWallet).flatMap(w => Option(w.getType)).map(PaymentMethodInfo.parseCardWallet)

          wallet match {
            case Some(w) => PaymentMethodType.WalletCard(brand, last4, expiry, funding, w)
            case None    => PaymentMethodType.Card(brand, last4, expiry, funding)
          }

        case "paypal" =>
          val paypal = Option(pm.getPaypal)
          PaymentMethodType.PayPal(
            payerEmail = paypal.flatMap(p => Option(p.getPayerEmail)),
            payerId = paypal.flatMap(p => Option(p.getPayerId)),
            country = paypal.flatMap(p => Option(p.getCountry)),
          )

        case other =>
          PaymentMethodType.Other(other)
      }

    PaymentMethodInfo(id = id, methodType = methodType)
  }

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
