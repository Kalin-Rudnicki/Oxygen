package oxygen.payments.stripe.ui.facades

import scala.scalajs.js

/**
  * Stripe Elements Appearance API.
  *
  * @see https://docs.stripe.com/elements/appearance-api
  */
trait StripeAppearance extends js.Object {

  /** `"stripe"` | `"night"` | `"flat"` */
  var theme: js.UndefOr[String] = js.undefined

  var variables: js.UndefOr[StripeAppearanceVariables] = js.undefined

  /**
    * CSS-like selector → property map, e.g. `.Tab:hover` → `{ color: "..." }`.
    * Values are CSS property bags as nested dictionaries.
    */
  var rules: js.UndefOr[js.Dictionary[js.Dictionary[String]]] = js.undefined

  /** `"above"` | `"floating"` */
  var labels: js.UndefOr[String] = js.undefined

  var disableAnimations: js.UndefOr[Boolean] = js.undefined

}

object StripeAppearance {

  def apply(
      theme: js.UndefOr[String] = js.undefined,
      variables: js.UndefOr[StripeAppearanceVariables] = js.undefined,
      rules: js.UndefOr[js.Dictionary[js.Dictionary[String]]] = js.undefined,
      labels: js.UndefOr[String] = js.undefined,
      disableAnimations: js.UndefOr[Boolean] = js.undefined,
  ): StripeAppearance = {
    val t = theme
    val v = variables
    val r = rules
    val l = labels
    val d = disableAnimations
    new StripeAppearance {
      this.theme = t
      this.variables = v
      this.rules = r
      this.labels = l
      this.disableAnimations = d
    }
  }

}

/** Common Appearance API variables (all optional). */
trait StripeAppearanceVariables extends js.Object {
  var fontFamily: js.UndefOr[String] = js.undefined
  var fontSizeBase: js.UndefOr[String] = js.undefined
  var fontSmooth: js.UndefOr[String] = js.undefined
  var fontVariantLigatures: js.UndefOr[String] = js.undefined
  var fontVariationSettings: js.UndefOr[String] = js.undefined
  var fontLineHeight: js.UndefOr[String] = js.undefined

  var colorPrimary: js.UndefOr[String] = js.undefined
  var colorBackground: js.UndefOr[String] = js.undefined
  var colorText: js.UndefOr[String] = js.undefined
  var colorTextSecondary: js.UndefOr[String] = js.undefined
  var colorTextPlaceholder: js.UndefOr[String] = js.undefined
  var colorDanger: js.UndefOr[String] = js.undefined
  var colorSuccess: js.UndefOr[String] = js.undefined
  var colorWarning: js.UndefOr[String] = js.undefined

  var borderRadius: js.UndefOr[String] = js.undefined
  var spacingUnit: js.UndefOr[String] = js.undefined

  var iconColor: js.UndefOr[String] = js.undefined
  var iconHoverColor: js.UndefOr[String] = js.undefined
  var iconCardErrorColor: js.UndefOr[String] = js.undefined
  var iconCardCvcColor: js.UndefOr[String] = js.undefined
  var iconCardCvcErrorColor: js.UndefOr[String] = js.undefined
  var iconCheckmarkColor: js.UndefOr[String] = js.undefined
  var iconChevronDownColor: js.UndefOr[String] = js.undefined
  var iconChevronDownHoverColor: js.UndefOr[String] = js.undefined
  var iconCloseColor: js.UndefOr[String] = js.undefined
  var iconCloseHoverColor: js.UndefOr[String] = js.undefined
  var iconLoadingIndicatorColor: js.UndefOr[String] = js.undefined
  var iconMenuColor: js.UndefOr[String] = js.undefined
  var iconMenuHoverColor: js.UndefOr[String] = js.undefined
  var iconPasscodeDeviceColor: js.UndefOr[String] = js.undefined
  var iconPasscodeDeviceHoverColor: js.UndefOr[String] = js.undefined
  var iconPasscodeDeviceNotificationColor: js.UndefOr[String] = js.undefined
  var iconRedirectColor: js.UndefOr[String] = js.undefined

  var tabIconSelectedColor: js.UndefOr[String] = js.undefined
  var tabIconHoverColor: js.UndefOr[String] = js.undefined
  var tabIconMoreColor: js.UndefOr[String] = js.undefined
  var tabIconMoreHoverColor: js.UndefOr[String] = js.undefined

  var logoColor: js.UndefOr[String] = js.undefined
  var tabLogoSelectedColor: js.UndefOr[String] = js.undefined
  var tabLogoHoverColor: js.UndefOr[String] = js.undefined

  var accessibleColorOnColorPrimary: js.UndefOr[String] = js.undefined
  var accessibleColorOnColorBackground: js.UndefOr[String] = js.undefined
  var accessibleColorOnColorSuccess: js.UndefOr[String] = js.undefined
  var accessibleColorOnColorDanger: js.UndefOr[String] = js.undefined
  var accessibleColorOnColorWarning: js.UndefOr[String] = js.undefined
}

object StripeAppearanceVariables {

  def apply(
      colorPrimary: js.UndefOr[String] = js.undefined,
      colorBackground: js.UndefOr[String] = js.undefined,
      colorText: js.UndefOr[String] = js.undefined,
      colorTextSecondary: js.UndefOr[String] = js.undefined,
      colorTextPlaceholder: js.UndefOr[String] = js.undefined,
      colorDanger: js.UndefOr[String] = js.undefined,
      fontFamily: js.UndefOr[String] = js.undefined,
      fontSizeBase: js.UndefOr[String] = js.undefined,
      borderRadius: js.UndefOr[String] = js.undefined,
      spacingUnit: js.UndefOr[String] = js.undefined,
  ): StripeAppearanceVariables = {
    val _colorPrimary = colorPrimary
    val _colorBackground = colorBackground
    val _colorText = colorText
    val _colorTextSecondary = colorTextSecondary
    val _colorTextPlaceholder = colorTextPlaceholder
    val _colorDanger = colorDanger
    val _fontFamily = fontFamily
    val _fontSizeBase = fontSizeBase
    val _borderRadius = borderRadius
    val _spacingUnit = spacingUnit
    new StripeAppearanceVariables {
      this.colorPrimary = _colorPrimary
      this.colorBackground = _colorBackground
      this.colorText = _colorText
      this.colorTextSecondary = _colorTextSecondary
      this.colorTextPlaceholder = _colorTextPlaceholder
      this.colorDanger = _colorDanger
      this.fontFamily = _fontFamily
      this.fontSizeBase = _fontSizeBase
      this.borderRadius = _borderRadius
      this.spacingUnit = _spacingUnit
    }
  }

}
