package oxygen.payments.stripe.service

import com.stripe.StripeClient
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

  override def createCustomer(req: CreateCustomer): IO[StripeError, StripeCustomerId] =
    ??? // FIX-PRE-MERGE (KR) :

  override def createSetupIntent(customerId: StripeCustomerId): IO[StripeError, StripeSetupIntentClientSecret] =
    ??? // FIX-PRE-MERGE (KR) :

}
object LiveStripeService {

  final case class Config(
      secretKey: StripeSecretKey,
      publishableKey: StripePublishableKey,
  ) derives JsonSchema

  private def buildClient(config: LiveStripeService.Config): Task[StripeClient] =
    ??? // FIX-PRE-MERGE (KR) :

  val layer: RLayer[LiveStripeService.Config, LiveStripeService] =
    ZLayer {
      for {
        config <- ZIO.service[LiveStripeService.Config]
        client <- buildClient(config)
      } yield LiveStripeService(config, client)
    }

}
