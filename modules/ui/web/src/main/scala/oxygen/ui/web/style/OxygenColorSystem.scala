package oxygen.ui.web.style

import oxygen.ui.web.create.*

/**
  * Seed → generate → override Oxygen color system (authoring-time only).
  * Runtime widgets must consume CSS variables, not resolved colors.
  */
object OxygenColorSystem {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Seeds
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Seeds(
      background: String,
      foreground: String,
      primary: String,
      danger: String,
      success: String,
      warning: String,
      focus: String,
      accent: String,
      brand1: String,
      brand2: String,
      info: Option[String] = None,
      notification: Option[String] = None,
  )

  object Seeds {

    /**
      * Built-in dark default: **Graphite neutrals + pop accent** (research: neutrals first, one chroma).
      * brand1 mirrors primary so shell chrome (TopBar.brand historically) stays high-contrast.
      * Packs live in [[oxygen.ui.web.style.OxygenThemes]].
      */
    val oxygenDark: Seeds =
      Seeds(
        background = "#09090b", // zinc-950-ish canvas
        foreground = "#fafafa", // near-white ink
        primary = "#3b82f6", // blue that pops on graphite
        danger = "#f87171",
        success = "#4ade80",
        warning = "#fbbf24",
        focus = "#60a5fa",
        accent = "#818cf8", // indigo accent wash / links
        brand1 = "#2563eb", // shell = solid action (not muddy gray)
        brand2 = "#a1a1aa",
      )

    /** Light companion: paper neutrals + saturated primary for CTAs. */
    val oxygenLight: Seeds =
      Seeds(
        background = "#fafafa",
        foreground = "#09090b",
        primary = "#2563eb",
        danger = "#dc2626",
        success = "#16a34a",
        warning = "#d97706",
        focus = "#2563eb",
        accent = "#4f46e5",
        brand1 = "#1d4ed8",
        brand2 = "#71717a",
      )

  }

  enum Mode {
    case Light, Dark
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Generated pieces
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class RoleScale(
      standard: String,
      strong: String,
      subtle: String,
      minimal: String,
      hover: String,
      active: String,
      /** Ink on solid `standard` fill (luminance-picked). */
      on: String,
  )

  final case class BrandScale(
      standard: String,
      light: String,
      dark: String,
  )

  final case class GeneratedPalette(
      mode: Mode,
      primary: RoleScale,
      accent: RoleScale,
      positive: RoleScale,
      negative: RoleScale,
      alert: RoleScale,
      informational: RoleScale,
      notification: String,
      fgDefault: String,
      fgInverse: String,
      fgModerate: String,
      fgSubtle: String,
      fgMinimal: String,
      fgFocus: String,
      fgFocusInverse: String,
      fgTextLink: String,
      fgGlobalBlack: String,
      fgGlobalWhite: String,
      bgBase: String,
      bgDefault: String,
      bgLayerOne: String,
      bgLayerTwo: String,
      bgLayerThree: String,
      bgTransparent: String,
      brand1: BrandScale,
      brand2: BrandScale,
      highlightBrand: String,
      highlight1: String,
      highlight2: String,
      highlight3: String,
      highlight4: String,
      highlight5: String,
  ) {

    def withOverrides(overrides: Map[String, String]): GeneratedPalette = {
      def o(key: String, current: String): String = overrides.getOrElse(key, current)
      def role(prefix: String, r: RoleScale): RoleScale =
        RoleScale(
          standard = o(s"$prefix.standard", r.standard),
          strong = o(s"$prefix.strong", r.strong),
          subtle = o(s"$prefix.subtle", r.subtle),
          minimal = o(s"$prefix.minimal", r.minimal),
          hover = o(s"$prefix.hover", r.hover),
          active = o(s"$prefix.active", r.active),
          on = o(s"$prefix.on", r.on),
        )
      def brand(prefix: String, b: BrandScale): BrandScale =
        BrandScale(
          standard = o(s"$prefix.standard", b.standard),
          light = o(s"$prefix.light", b.light),
          dark = o(s"$prefix.dark", b.dark),
        )

      copy(
        primary = role("color.primary", primary),
        accent = role("color.highlight.accent", accent),
        positive = role("color.status.positive", positive),
        negative = role("color.status.negative", negative),
        alert = role("color.status.alert", alert),
        informational = role("color.status.informational", informational),
        notification = o("color.status.notification", notification),
        fgDefault = o("color.fg.default", fgDefault),
        fgInverse = o("color.fg.inverse", fgInverse),
        fgModerate = o("color.fg.moderate", fgModerate),
        fgSubtle = o("color.fg.subtle", fgSubtle),
        fgMinimal = o("color.fg.minimal", fgMinimal),
        fgFocus = o("color.fg.focus", fgFocus),
        fgFocusInverse = o("color.fg.focusInverse", fgFocusInverse),
        fgTextLink = o("color.fg.textLink", fgTextLink),
        fgGlobalBlack = o("color.fg.globalBlack", fgGlobalBlack),
        fgGlobalWhite = o("color.fg.globalWhite", fgGlobalWhite),
        bgBase = o("color.bg.base", bgBase),
        bgDefault = o("color.bg.default", bgDefault),
        bgLayerOne = o("color.bg.layerOne", bgLayerOne),
        bgLayerTwo = o("color.bg.layerTwo", bgLayerTwo),
        bgLayerThree = o("color.bg.layerThree", bgLayerThree),
        bgTransparent = o("color.bg.transparent", bgTransparent),
        brand1 = brand("color.brand.primary1", brand1),
        brand2 = brand("color.brand.primary2", brand2),
        highlightBrand = o("color.highlight.brand", highlightBrand),
        highlight1 = o("color.highlight._1", highlight1),
        highlight2 = o("color.highlight._2", highlight2),
        highlight3 = o("color.highlight._3", highlight3),
        highlight4 = o("color.highlight._4", highlight4),
        highlight5 = o("color.highlight._5", highlight5),
      )
    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Generation
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def generate(seeds: Seeds, mode: Mode): GeneratedPalette = {
    val bg = color(seeds.background)
    val fg = color(seeds.foreground)
    val primaryC = color(seeds.primary)
    val dangerC = color(seeds.danger)
    val successC = color(seeds.success)
    val warningC = color(seeds.warning)
    val focusC = color(seeds.focus)
    val accentC = color(seeds.accent)
    val brand1C = color(seeds.brand1)
    val brand2C = color(seeds.brand2)
    val infoC = color(seeds.info.getOrElse(seeds.focus))
    val notifC = color(seeds.notification.getOrElse(seeds.danger))

    val bgBase = bg
    val bgDefault = surfaceStep(bgBase, mode, 1)
    val bgLayerOne = surfaceStep(bgBase, mode, 2)
    val bgLayerTwo = surfaceStep(bgBase, mode, 3)
    val bgLayerThree = surfaceStep(bgBase, mode, 4)

    GeneratedPalette(
      mode = mode,
      primary = roleScale(primaryC, bgBase, mode),
      accent = roleScale(accentC, bgBase, mode),
      positive = roleScale(successC, bgBase, mode),
      negative = roleScale(dangerC, bgBase, mode),
      alert = roleScale(warningC, bgBase, mode),
      informational = roleScale(infoC, bgBase, mode),
      notification = show(notifC),
      fgDefault = show(fg),
      fgInverse = show(bg),
      fgModerate = show(mixToward(fg, bg, 0.35)),
      fgSubtle = show(mixToward(fg, bg, 0.55)),
      fgMinimal = show(mixToward(fg, bg, 0.75)),
      fgFocus = show(focusC),
      fgFocusInverse = show(focusC.darken(40.0)),
      fgTextLink = show(if mode == Mode.Light then primaryC.darken(10.0) else primaryC),
      fgGlobalBlack = "#000000",
      fgGlobalWhite = "#ffffff",
      bgBase = show(bgBase),
      bgDefault = show(bgDefault),
      bgLayerOne = show(bgLayerOne),
      bgLayerTwo = show(bgLayerTwo),
      bgLayerThree = show(bgLayerThree),
      bgTransparent = show(CSSColor("#000000").setOpacity(70.0)),
      brand1 = brandScale(brand1C),
      brand2 = brandScale(brand2C),
      highlightBrand = show(brand2C),
      highlight1 = "#ffda55",
      highlight2 = "#f79064",
      highlight3 = "#0982fc",
      highlight4 = "#cd1007",
      highlight5 = "#ff3465",
    )
  }

  def withOverrides(palette: GeneratedPalette, overrides: Map[String, String]): GeneratedPalette =
    palette.withOverrides(overrides)

  /**
    * Project generated colors onto the existing OxygenStyleVars[String] surface
    * (including hover/active on ColorWithStrength).
    */
  def toStyleVars(palette: GeneratedPalette): OxygenStyleVars[String] =
    new OxygenStyleVars[String] {
      object color extends Colors {
        object primary extends ColorWithStrength {
          val standard: String = palette.primary.standard
          val strong: String = palette.primary.strong
          val subtle: String = palette.primary.subtle
          val minimal: String = palette.primary.minimal
          val hover: String = palette.primary.hover
          val active: String = palette.primary.active
          val on: String = palette.primary.on
        }
        object fg extends FG {
          val default: String = palette.fgDefault
          val inverse: String = palette.fgInverse
          val moderate: String = palette.fgModerate
          val subtle: String = palette.fgSubtle
          val minimal: String = palette.fgMinimal
          val focus: String = palette.fgFocus
          val focusInverse: String = palette.fgFocusInverse
          val textLink: String = palette.fgTextLink
          val globalBlack: String = palette.fgGlobalBlack
          val globalWhite: String = palette.fgGlobalWhite
        }
        object bg extends BG {
          val default: String = palette.bgDefault
          val base: String = palette.bgBase
          val layerOne: String = palette.bgLayerOne
          val layerTwo: String = palette.bgLayerTwo
          val layerThree: String = palette.bgLayerThree
          val transparent: String = palette.bgTransparent
        }
        object highlight extends Highlight {
          object accent extends ColorWithStrength {
            val standard: String = palette.accent.standard
            val strong: String = palette.accent.strong
            val subtle: String = palette.accent.subtle
            val minimal: String = palette.accent.minimal
            val hover: String = palette.accent.hover
            val active: String = palette.accent.active
            val on: String = palette.accent.on
          }
          val brand: String = palette.highlightBrand
          val _1: String = palette.highlight1
          val _2: String = palette.highlight2
          val _3: String = palette.highlight3
          val _4: String = palette.highlight4
          val _5: String = palette.highlight5
        }
        object status extends Status {
          object positive extends ColorWithStrength {
            val standard: String = palette.positive.standard
            val strong: String = palette.positive.strong
            val subtle: String = palette.positive.subtle
            val minimal: String = palette.positive.minimal
            val hover: String = palette.positive.hover
            val active: String = palette.positive.active
            val on: String = palette.positive.on
          }
          object negative extends ColorWithStrength {
            val standard: String = palette.negative.standard
            val strong: String = palette.negative.strong
            val subtle: String = palette.negative.subtle
            val minimal: String = palette.negative.minimal
            val hover: String = palette.negative.hover
            val active: String = palette.negative.active
            val on: String = palette.negative.on
          }
          object alert extends ColorWithStrength {
            val standard: String = palette.alert.standard
            val strong: String = palette.alert.strong
            val subtle: String = palette.alert.subtle
            val minimal: String = palette.alert.minimal
            val hover: String = palette.alert.hover
            val active: String = palette.alert.active
            val on: String = palette.alert.on
          }
          object informational extends ColorWithStrength {
            val standard: String = palette.informational.standard
            val strong: String = palette.informational.strong
            val subtle: String = palette.informational.subtle
            val minimal: String = palette.informational.minimal
            val hover: String = palette.informational.hover
            val active: String = palette.informational.active
            val on: String = palette.informational.on
          }
          val notification: String = palette.notification
        }
        object brand extends Brand {
          object primary1 extends ColorWithLightDark {
            val standard: String = palette.brand1.standard
            val light: String = palette.brand1.light
            val dark: String = palette.brand1.dark
          }
          object primary2 extends ColorWithLightDark {
            val standard: String = palette.brand2.standard
            val light: String = palette.brand2.light
            val dark: String = palette.brand2.dark
          }
        }
      }

      object spacing extends Spacing {
        val _1px: String = 1.px
        val _2px: String = 2.px
        val xxs: String = 4.px
        val xs: String = 8.px
        val s: String = 12.px
        val m: String = 16.px
        val l: String = 24.px
        val xl: String = 32.px
        val xxl: String = 64.px
        private val mult: Int = 4
        val _0: String = (0 * mult).px
        val _1: String = (1 * mult).px
        val _2: String = (2 * mult).px
        val _3: String = (3 * mult).px
        val _4: String = (4 * mult).px
        val _5: String = (5 * mult).px
        val _6: String = (6 * mult).px
        val _7: String = (7 * mult).px
        val _8: String = (8 * mult).px
        val _9: String = (9 * mult).px
        val _10: String = (10 * mult).px
        val _11: String = (11 * mult).px
        val _12: String = (12 * mult).px
        val _13: String = (13 * mult).px
        val _14: String = (14 * mult).px
        val _15: String = (15 * mult).px
        val _16: String = (16 * mult).px
        val _17: String = (17 * mult).px
        val _18: String = (18 * mult).px
        val _19: String = (19 * mult).px
        val _20: String = (20 * mult).px
        val _21: String = (21 * mult).px
        val _22: String = (22 * mult).px
        val _23: String = (23 * mult).px
        val _24: String = (24 * mult).px
        val _25: String = (25 * mult).px
      }

      object borderWidth extends BorderWidth {
        val _0: String = 0.px
        val _1: String = 1.px
        val _2: String = 2.px
        val _3: String = 3.px
        val _4: String = 4.px
        val _5: String = 5.px
        val _6: String = 6.px
      }

      object borderRadius extends BorderRadius {
        val _1px: String = 1.px
        val _2px: String = 2.px
        val s: String = 4.px
        val m: String = 8.px
        val l: String = 32.px
        private val mult: Int = 4
        val _0: String = (mult * 0).px
        val _1: String = (mult * 1).px
        val _2: String = (mult * 2).px
        val _3: String = (mult * 3).px
        val _4: String = (mult * 4).px
        val _5: String = (mult * 5).px
        val _6: String = (mult * 6).px
        val _7: String = (mult * 7).px
        val _8: String = (mult * 8).px
      }

      object fontSize extends FontSize {
        val _1: String = 0.75.rem
        val _2: String = 0.875.rem
        val _3: String = 1.rem
        val _4: String = 1.125.rem
        val _5: String = 1.25.rem
        val _6: String = 1.375.rem
        val _7: String = 1.5.rem
        val _8: String = 1.75.rem
        val _9: String = 2.rem
        val _10: String = 2.25.rem
        val _11: String = 2.625.rem
        val _12: String = 3.rem
        val _13: String = 3.25.rem
        val _14: String = 3.625.rem
        val _15: String = 4.rem
      }

      object fontStyle extends FontStyle {
        val default: String = "Roboto Flex, Roboto Condensed"
      }

      object fontWeight extends FontWeight {
        val thin: String = "100"
        val light: String = "300"
        val regular: String = "400"
        val medium: String = "500"
        val semiBold: String = "600"
        val bold: String = "700"
        val extraBold: String = "800"
        val black: String = "900"
      }
    }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Internals
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def color(hex: String): CSSColor = CSSColor.unsafeParse(hex)

  private def show(c: CSSColor): String = c.show

  private def lightness(c: CSSColor): Double =
    c match
      case rgb: CSSColor.RGB     => rgb.toHSL.lightness
      case CSSColor.RGBA(rgb, _) => rgb.toHSL.lightness

  private def roleScale(seed: CSSColor, bg: CSSColor, mode: Mode): RoleScale = {
    val standard = seed
    val strong =
      mode match
        case Mode.Dark  => seed.lighten(12.0)
        case Mode.Light => seed.darken(12.0)
    val subtle = mixToward(seed, bg, 0.55)
    val minimal = mixToward(seed, bg, 0.75)
    val (hover, active) =
      if lightness(seed) < 0.22 then (seed.lighten(12.0), seed.lighten(22.0))
      else (seed.darken(15.0), seed.darken(30.0))
    RoleScale(
      standard = show(standard),
      strong = show(strong),
      subtle = show(subtle),
      minimal = show(minimal),
      hover = show(hover),
      active = show(active),
      on = contrastInk(seed),
    )
  }

  /**
    * On-fill ink for solid chromatics (site audits: always pick for *container*, not page mode).
    * Uses relative luminance; threshold ~0.45 → black ink, else white.
    */
  def contrastInk(fill: CSSColor): String = {
    val l = relativeLuminance(fill)
    if l > 0.45 then "#000000" else "#ffffff"
  }

  private def relativeLuminance(c: CSSColor): Double = {
    val rgb = c match
      case r: CSSColor.RGB     => r
      case CSSColor.RGBA(r, _) => r
    def lin(channel: Int): Double = {
      val s = channel / 255.0
      if s <= 0.03928 then s / 12.92 else math.pow((s + 0.055) / 1.055, 2.4)
    }
    0.2126 * lin(rgb.r.value) + 0.7152 * lin(rgb.g.value) + 0.0722 * lin(rgb.b.value)
  }

  private def brandScale(seed: CSSColor): BrandScale =
    BrandScale(
      standard = show(seed),
      light = show(seed.lighten(15.0)),
      dark = show(seed.darken(15.0)),
    )

  /** amount=0 keep a; amount=1 fully b. Implemented via integer weights. */
  private def mixToward(a: CSSColor, b: CSSColor, amount: Double): CSSColor = {
    val toward = math.max(0.0, math.min(1.0, amount))
    val wB = math.round(toward * 100).toInt
    val wA = 100 - wB
    if wB <= 0 then a
    else if wA <= 0 then b
    else a.mix(wA)(b, wB)
  }

  /** Surface ladder — larger steps so layers read clearly (Carbon-style stacking). */
  private def surfaceStep(base: CSSColor, mode: Mode, step: Int): CSSColor = {
    val pct = step match
      case 1 => 10.0
      case 2 => 18.0
      case 3 => 28.0
      case _ => 40.0
    mode match
      case Mode.Dark  => base.lighten(pct)
      case Mode.Light => base.darken(pct * 0.55)
  }

}
