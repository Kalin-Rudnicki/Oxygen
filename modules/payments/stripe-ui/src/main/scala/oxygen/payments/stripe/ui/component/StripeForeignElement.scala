package oxygen.payments.stripe.ui.component

import org.scalajs.dom.Element
import oxygen.payments.stripe.ui.facades as F
import oxygen.ui.web.internal.*
import zio.*

final class StripeForeignElement private (
    private[stripe] val elements: F.StripeElements,
    private[stripe] val element: F.StripeElement,
)(using ForeignElement.Register) extends ForeignElement {

  override val name: String = "Stripe Element"

  override protected def mountInternal(stableId: String, element: Element): Unit = this.element.mount(element)
  override protected def unmountInternal(): Unit = this.element.unmount()
  override protected def destroyInternal(): Unit = this.element.destroy()

}
object StripeForeignElement {

  def register(elements: F.StripeElements, element: F.StripeElement): URIO[Scope, StripeForeignElement] =
    ForeignElement.register { new StripeForeignElement(elements, element) }

}
