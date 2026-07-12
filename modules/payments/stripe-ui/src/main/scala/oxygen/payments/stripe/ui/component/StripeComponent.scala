package oxygen.payments.stripe.ui.component

import oxygen.core.model.currency.CurrencyCode
import oxygen.payments.stripe.ui.component.StripeForeignElement
import oxygen.payments.stripe.ui.facades as F
import oxygen.payments.stripe.ui.service.StripeService
import oxygen.stripe.model.*
import oxygen.ui.web.{PWidget, UIError}
import oxygen.ui.web.create.*
import scala.scalajs.js
import zio.*

final class StripeComponent private (
    private val element: StripeForeignElement,
) {

  def submit: IO[UIError, Any] =
    validateSubmit *>
      confirmSetup

  /////// Helpers ///////////////////////////////////////////////////////////////

  /**
    * Validate the Payment Element (and collect wallet data if needed).
    * Call before BE confirm / `confirmSetup`. Does not confirm the SetupIntent.
    */
  private def validateSubmit: IO[UIError, Unit] =
    ZIO.fromPromiseJS { element.elements.submit() }
      .flatMapError { t =>
        val message: String = Option(t.getMessage).filter(_.nonEmpty).getOrElse("Failed to submit payment details")
        val error: UIError = UIError.ClientSide.UserActionable(message)
        ZIO.logError(s"Stripe Error:\n$t").as(error)
      }
      .flatMap { result =>
        undefToOption(result.error) match {
          case None =>
            ZIO.unit
          case Some(err) =>
            val message: String = undefToOption(err.message).filter(_.nonEmpty).getOrElse("Invalid payment details")
            ZIO.fail(UIError.ClientSide.UserActionable(message))
        }
      }

  private def confirmSetup: IO[UIError, Any] =
    ??? // FIX-PRE-MERGE (KR) :

  private def undefToOption[A](value: js.UndefOr[A]): Option[A] =
    if js.isUndefined(value) then None
    else Some(value.asInstanceOf[A])

}
object StripeComponent {

  lazy val widget: WidgetS[StripeComponent] =
    Widget.state[StripeComponent].get { stripeState =>
      PWidget.ForeignHost.empty("div", "stripe-host", stripeState.element)
    }

  /** Meant to be called on page init */
  def create(
      publishableKey: StripePublishableKey,
      clientSecret: StripeSetupIntentClientSecret,
      currency: CurrencyCode,
      appearance: F.StripeAppearance,
      elementOptions: F.StripeElementOptions,
  ): ZIO[StripeService & Scope, UIError, StripeComponent] =
    for {
      stripe <- ZIO.service[StripeService]
      element <- stripe.create(
        publishableKey = publishableKey,
        clientSecret = clientSecret,
        currency = currency,
        appearance = appearance,
        elementOptions = elementOptions,
      )
    } yield new StripeComponent(element)

}
