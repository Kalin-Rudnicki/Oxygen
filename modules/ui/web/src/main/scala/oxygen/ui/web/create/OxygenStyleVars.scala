package oxygen.ui.web.create

abstract class OxygenStyleVars[T] {

  val color: Colors
  val spacing: Spacing
  val borderWidth: BorderWidth
  val borderRadius: BorderRadius
  val fontSize: FontSize
  val fontStyle: FontStyle
  val fontWeight: FontWeight

  // TODO (KR) : add missing:
  //           : - font family
  //           : - font weight
  //           : - box shadow
  //           : - letter spacing
  //           : - line height

  ///////  ///////////////////////////////////////////////////////////////

  abstract class Colors {

    val primary: ColorWithStrength
    val fg: FG
    val bg: BG
    val highlight: Highlight
    val status: Status
    val brand: Brand

    abstract class FG {
      val default: T
      val inverse: T
      val moderate: T
      val subtle: T
      val minimal: T
      val globalBlack: T
      val globalWhite: T
      val focus: T
      val focusInverse: T
      val textLink: T
    }

    abstract class BG {
      val default: T
      val base: T
      val layerOne: T
      val layerTwo: T
      val layerThree: T
      val transparent: T
    }

    abstract class Highlight {
      val accent: ColorWithStrength
      val brand: T
      val _1: T
      val _2: T
      val _3: T
      val _4: T
      val _5: T
    }

    abstract class Status {
      val positive: ColorWithStrength
      val negative: ColorWithStrength
      val alert: ColorWithStrength
      val informational: ColorWithStrength
      val notification: T

      final def destructive: T = negative.standard // TODO (KR) : have a separate destructive color

    }

    // TODO (KR) : add more cases, and then add to css builder
    abstract class Brand {
      val primary1: ColorWithLightDark
      val primary2: ColorWithLightDark
    }

  }

  abstract class Spacing {
    val _1px: T
    val _2px: T

    val xxs: T
    val xs: T
    val s: T
    val m: T
    val l: T
    val xl: T
    val xxl: T

    val _0: T
    val _1: T
    val _2: T
    val _3: T
    val _4: T
    val _5: T
    val _6: T
    val _7: T
    val _8: T
    val _9: T
    val _10: T
    val _11: T
    val _12: T
    val _13: T
    val _14: T
    val _15: T
    val _16: T
    val _17: T
    val _18: T
    val _19: T
    val _20: T
    val _21: T
    val _22: T
    val _23: T
    val _24: T
    val _25: T
  }

  abstract class BorderWidth {
    val _0: T
    val _1: T
    val _2: T
    val _3: T
    val _4: T
    val _5: T
    val _6: T
  }

  abstract class BorderRadius {
    val _1px: T
    val _2px: T

    val s: T
    val m: T
    val l: T

    val _0: T
    val _1: T
    val _2: T
    val _3: T
    val _4: T
    val _5: T
    val _6: T
    val _7: T
    val _8: T
  }

  abstract class FontSize {
    val _1: T
    val _2: T
    val _3: T
    val _4: T
    val _5: T
    val _6: T
    val _7: T
    val _8: T
    val _9: T
    val _10: T
    val _11: T
    val _12: T
    val _13: T
    val _14: T
    val _15: T
  }

  abstract class FontStyle {
    val default: T
  }

  abstract class FontWeight {
    val thin: T
    val light: T
    val regular: T
    val medium: T
    val semiBold: T
    val bold: T
    val extraBold: T
    val black: T
  }

  ///////  ///////////////////////////////////////////////////////////////

  abstract class ColorWithStrength {
    val standard: T
    val strong: T
    val subtle: T
    val minimal: T
  }

  abstract class ColorWithLightDark {
    val standard: T
    val light: T
    val dark: T
  }

}
object OxygenStyleVars extends OxygenStyleVars[CSSVar] {

  def toCSS(vars: OxygenStyleVars[String]): StyleSheet =
    StyleSheet.variables("default styles")(
      // primary
      OxygenStyleVars.color.primary.standard -> vars.color.primary.standard,
      OxygenStyleVars.color.primary.strong -> vars.color.primary.strong,
      OxygenStyleVars.color.primary.subtle -> vars.color.primary.subtle,
      OxygenStyleVars.color.primary.minimal -> vars.color.primary.minimal,
      // fg
      OxygenStyleVars.color.fg.default -> vars.color.fg.default,
      OxygenStyleVars.color.fg.moderate -> vars.color.fg.moderate,
      OxygenStyleVars.color.fg.subtle -> vars.color.fg.subtle,
      OxygenStyleVars.color.fg.minimal -> vars.color.fg.minimal,
      OxygenStyleVars.color.fg.inverse -> vars.color.fg.inverse,
      OxygenStyleVars.color.fg.globalBlack -> vars.color.fg.globalBlack,
      OxygenStyleVars.color.fg.globalWhite -> vars.color.fg.globalWhite,
      OxygenStyleVars.color.fg.focus -> vars.color.fg.focus,
      OxygenStyleVars.color.fg.focusInverse -> vars.color.fg.focusInverse,
      OxygenStyleVars.color.fg.textLink -> vars.color.fg.textLink,
      // bg
      OxygenStyleVars.color.bg.base -> vars.color.bg.base,
      OxygenStyleVars.color.bg.default -> vars.color.bg.default,
      OxygenStyleVars.color.bg.layerOne -> vars.color.bg.layerOne,
      OxygenStyleVars.color.bg.layerTwo -> vars.color.bg.layerTwo,
      OxygenStyleVars.color.bg.layerThree -> vars.color.bg.layerThree,
      OxygenStyleVars.color.bg.transparent -> vars.color.bg.transparent,
      // highlight
      OxygenStyleVars.color.highlight.accent.standard -> vars.color.highlight.accent.standard,
      OxygenStyleVars.color.highlight.accent.strong -> vars.color.highlight.accent.strong,
      OxygenStyleVars.color.highlight.accent.subtle -> vars.color.highlight.accent.subtle,
      OxygenStyleVars.color.highlight.accent.minimal -> vars.color.highlight.accent.minimal,
      OxygenStyleVars.color.highlight.brand -> vars.color.highlight.brand,
      OxygenStyleVars.color.highlight._1 -> vars.color.highlight._1,
      OxygenStyleVars.color.highlight._2 -> vars.color.highlight._2,
      OxygenStyleVars.color.highlight._3 -> vars.color.highlight._3,
      OxygenStyleVars.color.highlight._4 -> vars.color.highlight._4,
      OxygenStyleVars.color.highlight._5 -> vars.color.highlight._5,
      // status
      OxygenStyleVars.color.status.positive.standard -> vars.color.status.positive.standard,
      OxygenStyleVars.color.status.positive.strong -> vars.color.status.positive.strong,
      OxygenStyleVars.color.status.positive.subtle -> vars.color.status.positive.subtle,
      OxygenStyleVars.color.status.positive.minimal -> vars.color.status.positive.minimal,
      OxygenStyleVars.color.status.negative.standard -> vars.color.status.negative.standard,
      OxygenStyleVars.color.status.negative.strong -> vars.color.status.negative.strong,
      OxygenStyleVars.color.status.negative.subtle -> vars.color.status.negative.subtle,
      OxygenStyleVars.color.status.negative.minimal -> vars.color.status.negative.minimal,
      OxygenStyleVars.color.status.alert.standard -> vars.color.status.alert.standard,
      OxygenStyleVars.color.status.alert.strong -> vars.color.status.alert.strong,
      OxygenStyleVars.color.status.alert.subtle -> vars.color.status.alert.subtle,
      OxygenStyleVars.color.status.alert.minimal -> vars.color.status.alert.minimal,
      OxygenStyleVars.color.status.informational.standard -> vars.color.status.informational.standard,
      OxygenStyleVars.color.status.informational.strong -> vars.color.status.informational.strong,
      OxygenStyleVars.color.status.informational.subtle -> vars.color.status.informational.subtle,
      OxygenStyleVars.color.status.informational.minimal -> vars.color.status.informational.minimal,
      OxygenStyleVars.color.status.notification -> vars.color.status.notification,
      // brand
      OxygenStyleVars.color.brand.primary1.standard -> vars.color.brand.primary1.standard,
      OxygenStyleVars.color.brand.primary1.light -> vars.color.brand.primary1.light,
      OxygenStyleVars.color.brand.primary1.dark -> vars.color.brand.primary1.dark,
      OxygenStyleVars.color.brand.primary2.standard -> vars.color.brand.primary2.standard,
      OxygenStyleVars.color.brand.primary2.light -> vars.color.brand.primary2.light,
      OxygenStyleVars.color.brand.primary2.dark -> vars.color.brand.primary2.dark,
      // spacing
      OxygenStyleVars.spacing._1px -> vars.spacing._1px,
      OxygenStyleVars.spacing._2px -> vars.spacing._2px,
      OxygenStyleVars.spacing.xxs -> vars.spacing.xxs,
      OxygenStyleVars.spacing.xs -> vars.spacing.xs,
      OxygenStyleVars.spacing.s -> vars.spacing.s,
      OxygenStyleVars.spacing.m -> vars.spacing.m,
      OxygenStyleVars.spacing.l -> vars.spacing.l,
      OxygenStyleVars.spacing.xl -> vars.spacing.xl,
      OxygenStyleVars.spacing.xxl -> vars.spacing.xxl,
      OxygenStyleVars.spacing._0 -> vars.spacing._0,
      OxygenStyleVars.spacing._1 -> vars.spacing._1,
      OxygenStyleVars.spacing._2 -> vars.spacing._2,
      OxygenStyleVars.spacing._3 -> vars.spacing._3,
      OxygenStyleVars.spacing._4 -> vars.spacing._4,
      OxygenStyleVars.spacing._5 -> vars.spacing._5,
      OxygenStyleVars.spacing._6 -> vars.spacing._6,
      OxygenStyleVars.spacing._7 -> vars.spacing._7,
      OxygenStyleVars.spacing._8 -> vars.spacing._8,
      OxygenStyleVars.spacing._9 -> vars.spacing._9,
      OxygenStyleVars.spacing._10 -> vars.spacing._10,
      OxygenStyleVars.spacing._11 -> vars.spacing._11,
      OxygenStyleVars.spacing._12 -> vars.spacing._12,
      OxygenStyleVars.spacing._13 -> vars.spacing._13,
      OxygenStyleVars.spacing._14 -> vars.spacing._14,
      OxygenStyleVars.spacing._15 -> vars.spacing._15,
      OxygenStyleVars.spacing._16 -> vars.spacing._16,
      OxygenStyleVars.spacing._17 -> vars.spacing._17,
      OxygenStyleVars.spacing._18 -> vars.spacing._18,
      OxygenStyleVars.spacing._19 -> vars.spacing._19,
      OxygenStyleVars.spacing._20 -> vars.spacing._20,
      OxygenStyleVars.spacing._21 -> vars.spacing._21,
      OxygenStyleVars.spacing._22 -> vars.spacing._22,
      OxygenStyleVars.spacing._23 -> vars.spacing._23,
      OxygenStyleVars.spacing._24 -> vars.spacing._24,
      OxygenStyleVars.spacing._25 -> vars.spacing._25,
      // border width
      OxygenStyleVars.borderWidth._0 -> vars.borderWidth._0,
      OxygenStyleVars.borderWidth._1 -> vars.borderWidth._1,
      OxygenStyleVars.borderWidth._2 -> vars.borderWidth._2,
      OxygenStyleVars.borderWidth._3 -> vars.borderWidth._3,
      OxygenStyleVars.borderWidth._4 -> vars.borderWidth._4,
      OxygenStyleVars.borderWidth._5 -> vars.borderWidth._5,
      OxygenStyleVars.borderWidth._6 -> vars.borderWidth._6,
      // border radius
      OxygenStyleVars.borderRadius._1px -> vars.borderRadius._1px,
      OxygenStyleVars.borderRadius._2px -> vars.borderRadius._2px,
      OxygenStyleVars.borderRadius.s -> vars.borderRadius.s,
      OxygenStyleVars.borderRadius.m -> vars.borderRadius.m,
      OxygenStyleVars.borderRadius.l -> vars.borderRadius.l,
      OxygenStyleVars.borderRadius._0 -> vars.borderRadius._0,
      OxygenStyleVars.borderRadius._1 -> vars.borderRadius._1,
      OxygenStyleVars.borderRadius._2 -> vars.borderRadius._2,
      OxygenStyleVars.borderRadius._3 -> vars.borderRadius._3,
      OxygenStyleVars.borderRadius._4 -> vars.borderRadius._4,
      OxygenStyleVars.borderRadius._5 -> vars.borderRadius._5,
      OxygenStyleVars.borderRadius._6 -> vars.borderRadius._6,
      OxygenStyleVars.borderRadius._7 -> vars.borderRadius._7,
      OxygenStyleVars.borderRadius._8 -> vars.borderRadius._8,
      // font size
      OxygenStyleVars.fontSize._1 -> vars.fontSize._1,
      OxygenStyleVars.fontSize._2 -> vars.fontSize._2,
      OxygenStyleVars.fontSize._3 -> vars.fontSize._3,
      OxygenStyleVars.fontSize._4 -> vars.fontSize._4,
      OxygenStyleVars.fontSize._5 -> vars.fontSize._5,
      OxygenStyleVars.fontSize._6 -> vars.fontSize._6,
      OxygenStyleVars.fontSize._7 -> vars.fontSize._7,
      OxygenStyleVars.fontSize._8 -> vars.fontSize._8,
      OxygenStyleVars.fontSize._9 -> vars.fontSize._9,
      OxygenStyleVars.fontSize._10 -> vars.fontSize._10,
      OxygenStyleVars.fontSize._11 -> vars.fontSize._11,
      OxygenStyleVars.fontSize._12 -> vars.fontSize._12,
      OxygenStyleVars.fontSize._13 -> vars.fontSize._13,
      OxygenStyleVars.fontSize._14 -> vars.fontSize._14,
      OxygenStyleVars.fontSize._15 -> vars.fontSize._15,
      // font style
      OxygenStyleVars.fontStyle.default -> vars.fontStyle.default,
      // font weight
      OxygenStyleVars.fontWeight.thin -> vars.fontWeight.thin,
      OxygenStyleVars.fontWeight.light -> vars.fontWeight.light,
      OxygenStyleVars.fontWeight.regular -> vars.fontWeight.regular,
      OxygenStyleVars.fontWeight.medium -> vars.fontWeight.medium,
      OxygenStyleVars.fontWeight.semiBold -> vars.fontWeight.semiBold,
      OxygenStyleVars.fontWeight.bold -> vars.fontWeight.bold,
      OxygenStyleVars.fontWeight.extraBold -> vars.fontWeight.extraBold,
      OxygenStyleVars.fontWeight.black -> vars.fontWeight.black,
    )

  object color extends Colors {

    object primary extends ColorWithStrength {
      val standard: CSSVar = CSSVar("style--color--primary")
      val strong: CSSVar = CSSVar("style--color--primary-strong")
      val minimal: CSSVar = CSSVar("style--color--primary-minimal")
      val subtle: CSSVar = CSSVar("style--color--primary-subtle")
    }

    object fg extends FG {
      val default: CSSVar = CSSVar("style--color--fg--default")
      val inverse: CSSVar = CSSVar("style--color--fg--inverse")
      val moderate: CSSVar = CSSVar("style--color--fg--moderate")
      val subtle: CSSVar = CSSVar("style--color--fg--subtle")
      val minimal: CSSVar = CSSVar("style--color--fg--minimal")
      val globalBlack: CSSVar = CSSVar("style--color--fg--global-black")
      val globalWhite: CSSVar = CSSVar("style--color--fg--global-white")
      val focus: CSSVar = CSSVar("style--color--fg--focus")
      val focusInverse: CSSVar = CSSVar("style--color--fg--focus-inverse")
      val textLink: CSSVar = CSSVar("style--color--fg--text-link")
    }

    object bg extends BG {
      val default: CSSVar = CSSVar("style--color--bg--default")
      val base: CSSVar = CSSVar("style--color--bg--base")
      val layerOne: CSSVar = CSSVar("style--color--bg--layer-one")
      val layerTwo: CSSVar = CSSVar("style--color--bg--layer-two")
      val layerThree: CSSVar = CSSVar("style--color--bg--layer-three")
      val transparent: CSSVar = CSSVar("style--color--bg--transparent")
    }

    object highlight extends Highlight {
      object accent extends ColorWithStrength {
        val standard: CSSVar = CSSVar("style--color--highlight--accent")
        val strong: CSSVar = CSSVar("style--color--highlight--accent-strong")
        val subtle: CSSVar = CSSVar("style--color--highlight--accent-subtle")
        val minimal: CSSVar = CSSVar("style--color--highlight--accent-minimal")
      }
      val brand: CSSVar = CSSVar("style--color--highlight--brand")
      val _1: CSSVar = CSSVar("style--color--highlight--one")
      val _2: CSSVar = CSSVar("style--color--highlight--two")
      val _3: CSSVar = CSSVar("style--color--highlight--three")
      val _4: CSSVar = CSSVar("style--color--highlight--four")
      val _5: CSSVar = CSSVar("style--color--highlight--five")
    }

    object status extends Status {
      object positive extends ColorWithStrength {
        val standard: CSSVar = CSSVar("style--color--status--positive")
        val strong: CSSVar = CSSVar("style--color--status--positive-strong")
        val subtle: CSSVar = CSSVar("style--color--status--positive-subtle")
        val minimal: CSSVar = CSSVar("style--color--status--positive-minimal")
      }
      object negative extends ColorWithStrength {
        val standard: CSSVar = CSSVar("style--color--status--negative")
        val strong: CSSVar = CSSVar("style--color--status--negative-strong")
        val subtle: CSSVar = CSSVar("style--color--status--negative-subtle")
        val minimal: CSSVar = CSSVar("style--color--status--negative-minimal")
      }
      object alert extends ColorWithStrength {
        val standard: CSSVar = CSSVar("style--color--status--alert")
        val strong: CSSVar = CSSVar("style--color--status--alert-strong")
        val subtle: CSSVar = CSSVar("style--color--status--alert-subtle")
        val minimal: CSSVar = CSSVar("style--color--status--alert-minimal")
      }
      object informational extends ColorWithStrength {
        val standard: CSSVar = CSSVar("style--color--status--informational")
        val strong: CSSVar = CSSVar("style--color--status--informational-strong")
        val subtle: CSSVar = CSSVar("style--color--status--informational-subtle")
        val minimal: CSSVar = CSSVar("style--color--status--informational-minimal")
      }
      val notification: CSSVar = CSSVar("style--color--status--notification")
    }

    object brand extends Brand {
      object primary1 extends ColorWithLightDark {
        val standard: CSSVar = CSSVar("style--color--brand--primary-1")
        val light: CSSVar = CSSVar("style--color--brand--primary-1-light")
        val dark: CSSVar = CSSVar("style--color--brand--primary-1-dark")
      }
      object primary2 extends ColorWithLightDark {
        val standard: CSSVar = CSSVar("style--color--brand--primary-2")
        val light: CSSVar = CSSVar("style--color--brand--primary-2-light")
        val dark: CSSVar = CSSVar("style--color--brand--primary-2-dark")
      }
    }

  }

  object spacing extends Spacing {
    val _1px: CSSVar = CSSVar("style--spacing--1px")
    val _2px: CSSVar = CSSVar("style--spacing--2px")

    val xxs: CSSVar = CSSVar("style--spacing--xxs")
    val xs: CSSVar = CSSVar("style--spacing--xs")
    val s: CSSVar = CSSVar("style--spacing--s")
    val m: CSSVar = CSSVar("style--spacing--m")
    val l: CSSVar = CSSVar("style--spacing--l")
    val xl: CSSVar = CSSVar("style--spacing--xl")
    val xxl: CSSVar = CSSVar("style--spacing--xxl")

    val _0: CSSVar = CSSVar("style--spacing--0")
    val _1: CSSVar = CSSVar("style--spacing--1")
    val _2: CSSVar = CSSVar("style--spacing--2")
    val _3: CSSVar = CSSVar("style--spacing--3")
    val _4: CSSVar = CSSVar("style--spacing--4")
    val _5: CSSVar = CSSVar("style--spacing--5")
    val _6: CSSVar = CSSVar("style--spacing--6")
    val _7: CSSVar = CSSVar("style--spacing--7")
    val _8: CSSVar = CSSVar("style--spacing--8")
    val _9: CSSVar = CSSVar("style--spacing--9")
    val _10: CSSVar = CSSVar("style--spacing--10")
    val _11: CSSVar = CSSVar("style--spacing--11")
    val _12: CSSVar = CSSVar("style--spacing--12")
    val _13: CSSVar = CSSVar("style--spacing--13")
    val _14: CSSVar = CSSVar("style--spacing--14")
    val _15: CSSVar = CSSVar("style--spacing--15")
    val _16: CSSVar = CSSVar("style--spacing--16")
    val _17: CSSVar = CSSVar("style--spacing--17")
    val _18: CSSVar = CSSVar("style--spacing--18")
    val _19: CSSVar = CSSVar("style--spacing--19")
    val _20: CSSVar = CSSVar("style--spacing--20")
    val _21: CSSVar = CSSVar("style--spacing--21")
    val _22: CSSVar = CSSVar("style--spacing--22")
    val _23: CSSVar = CSSVar("style--spacing--23")
    val _24: CSSVar = CSSVar("style--spacing--24")
    val _25: CSSVar = CSSVar("style--spacing--25")
  }

  object borderWidth extends BorderWidth {
    val _0: CSSVar = CSSVar("style--border-width--0")
    val _1: CSSVar = CSSVar("style--border-width--1")
    val _2: CSSVar = CSSVar("style--border-width--2")
    val _3: CSSVar = CSSVar("style--border-width--3")
    val _4: CSSVar = CSSVar("style--border-width--4")
    val _5: CSSVar = CSSVar("style--border-width--5")
    val _6: CSSVar = CSSVar("style--border-width--6")
  }

  object borderRadius extends BorderRadius {
    val _1px: CSSVar = CSSVar("style--border-radius--1px")
    val _2px: CSSVar = CSSVar("style--border-radius--2px")

    val s: CSSVar = CSSVar("style--border-radius--s")
    val m: CSSVar = CSSVar("style--border-radius--m")
    val l: CSSVar = CSSVar("style--border-radius--l")

    val _0: CSSVar = CSSVar("style--border-radius--0")
    val _1: CSSVar = CSSVar("style--border-radius--1")
    val _2: CSSVar = CSSVar("style--border-radius--2")
    val _3: CSSVar = CSSVar("style--border-radius--3")
    val _4: CSSVar = CSSVar("style--border-radius--4")
    val _5: CSSVar = CSSVar("style--border-radius--5")
    val _6: CSSVar = CSSVar("style--border-radius--6")
    val _7: CSSVar = CSSVar("style--border-radius--7")
    val _8: CSSVar = CSSVar("style--border-radius--8")
  }

  object fontSize extends FontSize {
    val _1: CSSVar = CSSVar("style--font-size--1")
    val _2: CSSVar = CSSVar("style--font-size--2")
    val _3: CSSVar = CSSVar("style--font-size--3")
    val _4: CSSVar = CSSVar("style--font-size--4")
    val _5: CSSVar = CSSVar("style--font-size--5")
    val _6: CSSVar = CSSVar("style--font-size--6")
    val _7: CSSVar = CSSVar("style--font-size--7")
    val _8: CSSVar = CSSVar("style--font-size--8")
    val _9: CSSVar = CSSVar("style--font-size--9")
    val _10: CSSVar = CSSVar("style--font-size--10")
    val _11: CSSVar = CSSVar("style--font-size--11")
    val _12: CSSVar = CSSVar("style--font-size--12")
    val _13: CSSVar = CSSVar("style--font-size--13")
    val _14: CSSVar = CSSVar("style--font-size--14")
    val _15: CSSVar = CSSVar("style--font-size--15")
  }

  object fontStyle extends FontStyle {
    val default: CSSVar = CSSVar("style--font-style--default")
  }

  object fontWeight extends FontWeight {
    val thin: CSSVar = CSSVar("style--font-weight--thin")
    val light: CSSVar = CSSVar("style--font-weight--light")
    val regular: CSSVar = CSSVar("style--font-weight--regular")
    val medium: CSSVar = CSSVar("style--font-weight--medium")
    val semiBold: CSSVar = CSSVar("style--font-weight--semi-bold")
    val bold: CSSVar = CSSVar("style--font-weight--bold")
    val extraBold: CSSVar = CSSVar("style--font-weight--extra-bold")
    val black: CSSVar = CSSVar("style--font-weight--black")
  }

}

val S: OxygenStyleVars.type = OxygenStyleVars
