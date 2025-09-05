package oxygen.ui.web.defaults

import oxygen.ui.web.create.*

object OxygenStyleVarDefaults {

  object CZR extends OxygenStyleVars[String] {

    object color extends Colors {

      object primary extends ColorWithStrength {
        val standard: String = "#00e285"
        val strong: String = "#00ac5f"
        val subtle: String = "#365737"
        val minimal: String = "#364137"
      }

      object fg extends FG {
        val default: String = "#f3f3f3"
        val inverse: String = "#030303"

        val moderate: String = "#a8acab"
        val subtle: String = "#646766"
        val minimal: String = "#292a2a"

        val focus: String = "#259cff"
        val focusInverse: String = "#1b3665"

        val textLink: String = "#009e57"

        val globalBlack: String = "#000000"
        val globalWhite: String = "#ffffff"
      }

      object bg extends BG {
        val default: String = "#1d1d1d"
        val base: String = "#101010"
        val layerOne: String = "#292a2a"
        val layerTwo: String = "#363737"
        val layerThree: String = "#646766"
        val transparent: String = "#000000b3"
      }

      object highlight extends Highlight {
        object accent extends ColorWithStrength {
          val standard: String = "#af7fff"
          val strong: String = "#6f35ff"
          val subtle: String = "#311869"
          val minimal: String = "#1f1238"
        }
        val brand: String = "#c3a77c"
        val _1: String = "#ffda55"
        val _2: String = "#f79064"
        val _3: String = "#0982fc"
        val _4: String = "#cd1007"
        val _5: String = "#ff3465"
      }

      object status extends Status {
        object positive extends ColorWithStrength {
          val standard: String = "#00ac5f"
          val strong: String = "#00c46e"
          val subtle: String = "#007642"
          val minimal: String = "#0a140d"
        }
        object negative extends ColorWithStrength {
          val standard: String = "#f9315b"
          val strong: String = "#ff5e72"
          val subtle: String = "#c81844"
          val minimal: String = "#1f0d0d"
        }
        object alert extends ColorWithStrength {
          val standard: String = "#feba31"
          val strong: String = "#ffe6aa"
          val subtle: String = "#875f00"
          val minimal: String = "#171005"
        }
        object informational extends ColorWithStrength {
          val standard: String = "#259cff"
          val strong: String = "#63afff"
          val subtle: String = "#1564c5"
          val minimal: String = "#0e101a"
        }
        val notification: String = "#f9315b"
      }

      object brand extends Brand {
        object primary1 extends ColorWithLightDark {
          val standard: String = "#173432"
          val dark: String = "#173432" // TODO (KR) :
          val light: String = "#173432" // TODO (KR) :
        }
        object primary2 extends ColorWithLightDark {
          val standard: String = "#a0875b" // TODO (KR) :
          val dark: String = "#a0875b"
          val light: String = "#c5a459"
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

  }

}
