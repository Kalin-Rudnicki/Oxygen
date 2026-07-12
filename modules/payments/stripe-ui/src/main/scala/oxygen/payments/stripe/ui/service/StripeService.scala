package oxygen.payments.stripe.ui.service

import oxygen.core.model.currency.CurrencyCode
import oxygen.payments.stripe.ui.component.StripeForeignElement
import oxygen.payments.stripe.ui.facades as F
import oxygen.stripe.model.*
import oxygen.ui.web.UIError
import zio.*

trait StripeService {

  def elements(
      publishableKey: StripePublishableKey,
      clientSecret: StripeSetupIntentClientSecret,
      currency: CurrencyCode,
      appearance: F.StripeAppearance,
      elementOptions: F.StripeElementOptions,
  ): ZIO[Scope, UIError, StripeForeignElement]

}
object StripeService {

  final case class Live(loadedRef: Ref.Synchronized[Boolean]) extends StripeService {

    private def load: UIO[Unit] =
      ??? // FIX-PRE-MERGE (KR) : add stripe script

    private def ensureLoaded: UIO[Unit] =
      loadedRef.updateZIO {
        case true  => ZIO.succeed(true)
        case false => load.as(true)
      }

    private def makeElements(
        _publishableKey: StripePublishableKey,
        _clientSecret: StripeSetupIntentClientSecret,
        _currency: CurrencyCode,
        _appearance: F.StripeAppearance,
        _elementOptions: F.StripeElementOptions,
    ): Task[(F.Stripe, F.StripeElement)] = {
      val _elementsOptions: F.StripeElementsOptions =
        new F.StripeElementsOptions {
          this.mode = "setup"
          this.currency = _currency.code
          this.clientSecret = _clientSecret.unwrap
          this.appearance = _appearance
        }

      for {
        stripe <- ZIO.attempt { F.StripeGlobal.Stripe(_publishableKey.unwrap) }
        elements <- ZIO.attempt { stripe.elements(_elementsOptions) }
        element <- ZIO.attempt { elements.create("payment", _elementOptions) }
      } yield (stripe, element)
    }

    override def elements(
        publishableKey: StripePublishableKey,
        clientSecret: StripeSetupIntentClientSecret,
        currency: CurrencyCode,
        appearance: F.StripeAppearance,
        elementOptions: F.StripeElementOptions,
    ): ZIO[Scope, UIError, StripeForeignElement] =
      for {
        _ <- ensureLoaded
        (_, stripeElem) <- makeElements(
          _publishableKey = publishableKey,
          _clientSecret = clientSecret,
          _currency = currency,
          _appearance = appearance,
          _elementOptions = elementOptions,
        ).orDie // TODO (KR) :
        oxygenElem <- StripeForeignElement.register(stripeElem)
      } yield oxygenElem

  }

}
