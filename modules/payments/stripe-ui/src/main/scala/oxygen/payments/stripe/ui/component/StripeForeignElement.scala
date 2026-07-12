package oxygen.payments.stripe.ui.component

import org.scalajs.dom.Element
import oxygen.payments.stripe.ui.facades as F
import oxygen.ui.web.internal.*

final class StripeForeignElement private (stripe: F.StripeElement)(using ForeignElement.Register) extends ForeignElement {

  override val name: String = "Stripe Element"

  override protected def mountInternal(stableId: String, element: Element): Unit = stripe.mount(element)
  override protected def unmountInternal(): Unit = stripe.unmount()
  override protected def destroyInternal(): Unit = stripe.destroy()

}
object StripeForeignElement {
  
  // def make(): 
  
}
