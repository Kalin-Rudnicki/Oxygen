package oxygen.payments.stripe.ui.component

import org.scalajs.dom.Element
import oxygen.payments.stripe.ui.facades as F
import oxygen.stripe.model.StripeSetupIntentClientSecret
import oxygen.ui.web.internal.*
import zio.*

final class StripeForeignElement private (
    private[stripe] val stripe: F.Stripe,
    private[stripe] val elements: F.StripeElements,
    private[stripe] val element: F.StripeElement,
    private[stripe] val clientSecret: StripeSetupIntentClientSecret,
)(using ForeignElement.Register) extends ForeignElement {

  override val name: String = "Stripe Element"

  override protected def mountInternal(stableId: String, element: Element): Unit = this.element.mount(element)
  override protected def unmountInternal(): Unit = this.element.unmount()
  override protected def destroyInternal(): Unit = this.element.destroy()

}
object StripeForeignElement {

  def register(
      stripe: F.Stripe,
      elements: F.StripeElements,
      element: F.StripeElement,
      clientSecret: StripeSetupIntentClientSecret,
  ): URIO[Scope, StripeForeignElement] =
    ForeignElement.register { new StripeForeignElement(stripe, elements, element, clientSecret) }

}
