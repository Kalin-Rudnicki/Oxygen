package oxygen.payments.stripe.ui.service

import org.scalajs.dom.{document, Event, HTMLScriptElement}
import oxygen.core.model.currency.CurrencyCode
import oxygen.payments.stripe.ui.component.StripeForeignElement
import oxygen.payments.stripe.ui.facades as F
import oxygen.stripe.model.*
import oxygen.ui.web.UIError
import zio.*

trait StripeService {

  def create(
      publishableKey: StripePublishableKey,
      clientSecret: StripeSetupIntentClientSecret,
      currency: CurrencyCode,
      appearance: F.StripeAppearance,
      elementOptions: F.StripeElementOptions,
  ): ZIO[Scope, UIError, StripeForeignElement]

}
object StripeService {

  def live: ULayer[StripeService] = ZLayer.succeed { Live }

  private object Live extends StripeService {

    private val StripeJsUrl = "https://js.stripe.com/v3/"

    private val loadedRef: Ref.Synchronized[Boolean] =
      Unsafe.unsafely { Ref.Synchronized.unsafe.make(false) }

    /** Inject Stripe.js from CDN. Only called when [[loadedRef]] is still false. */
    private def load: Task[Unit] =
      ZIO.async[Any, Throwable, Unit] { cb =>
        val script = document.createElement("script").asInstanceOf[HTMLScriptElement]
        script.src = StripeJsUrl
        script.async = true
        script.addEventListener("load", (_: Event) => cb(ZIO.unit))
        script.addEventListener(
          "error",
          (e: Event) => cb(ZIO.fail(new RuntimeException(s"Failed to load Stripe.js from $StripeJsUrl : $e"))),
        )
        document.head.appendChild(script)
      }

    private def ensureLoaded: Task[Unit] =
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
    ): Task[(F.Stripe, F.StripeElements, F.StripeElement)] = {
      val _elementsOptions: F.StripeElementsOptions =
        new F.StripeElementsOptions {
          // this.mode = "setup"
          this.currency = _currency.code.toLowerCase
          this.clientSecret = _clientSecret.unwrap
          this.appearance = _appearance
        }

      for {
        stripe <- ZIO.attempt { F.StripeGlobal.Stripe(_publishableKey.unwrap) }
        elements <- ZIO.attempt { stripe.elements(_elementsOptions) }
        element <- ZIO.attempt { elements.create("payment", _elementOptions) }
      } yield (stripe, elements, element)
    }

    override def create(
        publishableKey: StripePublishableKey,
        clientSecret: StripeSetupIntentClientSecret,
        currency: CurrencyCode,
        appearance: F.StripeAppearance,
        elementOptions: F.StripeElementOptions,
    ): ZIO[Scope, UIError, StripeForeignElement] =
      for {
        _ <- ensureLoaded.mapError { e =>
          UIError.ClientSide.UserActionable(
            Option(e.getMessage).filter(_.nonEmpty).getOrElse("Failed to load Stripe.js"),
          )
        }
        (stripe, stripeElems, stripeElem) <- makeElements(
          _publishableKey = publishableKey,
          _clientSecret = clientSecret,
          _currency = currency,
          _appearance = appearance,
          _elementOptions = elementOptions,
        ).orDie // TODO (KR) :
        oxygenElem <- StripeForeignElement.register(stripe, stripeElems, stripeElem, clientSecret)
      } yield oxygenElem

  }

}
