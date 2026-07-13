package oxygen.payments.stripe.ui.facades

import org.scalajs.dom
import scala.scalajs.js

/** A single Element (e.g. Payment Element from `elements.create("payment")`). */
@js.native
trait StripeElement extends js.Object {

  def mount(domElement: String): Unit = js.native
  def mount(domElement: dom.Element): Unit = js.native

  def unmount(): Unit = js.native
  def destroy(): Unit = js.native

  def focus(): Unit = js.native
  def blur(): Unit = js.native
  def clear(): Unit = js.native

  def update(options: StripeElementOptions): Unit = js.native

  def on(event: String, handler: js.Function1[js.Object, Unit]): Unit = js.native
  def on(event: String, handler: js.Function0[Unit]): Unit = js.native

}

/** Options for `elements.create("payment" | ..., options)`. */
trait StripeElementOptions extends js.Object {
  var layout: js.UndefOr[String | js.Object] = js.undefined
  var defaultValues: js.UndefOr[js.Object] = js.undefined
  var business: js.UndefOr[js.Object] = js.undefined
  var paymentMethodOrder: js.UndefOr[js.Array[String]] = js.undefined
  var fields: js.UndefOr[js.Object] = js.undefined
  var terms: js.UndefOr[js.Object] = js.undefined
  var wallets: js.UndefOr[StripeWalletsOptions] = js.undefined
}

/**
  * Payment Element wallet visibility.
  * Values: `"auto"` | `"never"` (and for some wallets `"always"` where supported).
  */
trait StripeWalletsOptions extends js.Object {
  var applePay: js.UndefOr[String] = js.undefined
  var googlePay: js.UndefOr[String] = js.undefined
  var link: js.UndefOr[String] = js.undefined
}

object StripeWalletsOptions {

  /** Card wallets only — hide Link (and its “save for faster checkout” UI). */
  def cardWalletsOnly: StripeWalletsOptions =
    new StripeWalletsOptions {
      applePay = "auto"
      googlePay = "auto"
      link = "never"
    }

}
