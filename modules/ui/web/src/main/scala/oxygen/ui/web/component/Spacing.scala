package oxygen.ui.web.component

import oxygen.ui.web.create.{*, given}

object Spacing {

  object horizontal {

    def apply(w: String): Widget.Const = span(display.inlineBlock, width := w)
    def opt(w: Option[String]): Widget.Const = Widget.foreach(w)(apply)

    val xxs: Widget.Const = horizontal(S.spacing.xxs)
    val xs: Widget.Const = horizontal(S.spacing.xs)
    val s: Widget.Const = horizontal(S.spacing.s)
    val m: Widget.Const = horizontal(S.spacing.m)
    val l: Widget.Const = horizontal(S.spacing.l)
    val xl: Widget.Const = horizontal(S.spacing.xl)
    val xxl: Widget.Const = horizontal(S.spacing.xxl)

    val _0: Widget.Const = horizontal(S.spacing._0)
    val _1: Widget.Const = horizontal(S.spacing._1)
    val _2: Widget.Const = horizontal(S.spacing._2)
    val _3: Widget.Const = horizontal(S.spacing._3)
    val _4: Widget.Const = horizontal(S.spacing._4)
    val _5: Widget.Const = horizontal(S.spacing._5)
    val _6: Widget.Const = horizontal(S.spacing._6)
    val _7: Widget.Const = horizontal(S.spacing._7)
    val _8: Widget.Const = horizontal(S.spacing._8)
    val _9: Widget.Const = horizontal(S.spacing._9)
    val _10: Widget.Const = horizontal(S.spacing._10)
    val _11: Widget.Const = horizontal(S.spacing._11)
    val _12: Widget.Const = horizontal(S.spacing._12)
    val _13: Widget.Const = horizontal(S.spacing._13)
    val _14: Widget.Const = horizontal(S.spacing._14)
    val _15: Widget.Const = horizontal(S.spacing._15)
    val _16: Widget.Const = horizontal(S.spacing._16)
    val _17: Widget.Const = horizontal(S.spacing._17)
    val _18: Widget.Const = horizontal(S.spacing._18)
    val _19: Widget.Const = horizontal(S.spacing._19)
    val _20: Widget.Const = horizontal(S.spacing._20)
    val _21: Widget.Const = horizontal(S.spacing._21)
    val _22: Widget.Const = horizontal(S.spacing._22)
    val _23: Widget.Const = horizontal(S.spacing._23)
    val _24: Widget.Const = horizontal(S.spacing._24)
    val _25: Widget.Const = horizontal(S.spacing._25)

  }

  object vertical {

    def apply(h: String): Widget.Const = div(height := h)
    def opt(h: Option[String]): Widget.Const = Widget.foreach(h)(apply)

    val xxs: Widget.Const = vertical(S.spacing.xxs)
    val xs: Widget.Const = vertical(S.spacing.xs)
    val s: Widget.Const = vertical(S.spacing.s)
    val m: Widget.Const = vertical(S.spacing.m)
    val l: Widget.Const = vertical(S.spacing.l)
    val xl: Widget.Const = vertical(S.spacing.xl)
    val xxl: Widget.Const = vertical(S.spacing.xxl)

    val _0: Widget.Const = vertical(S.spacing._0)
    val _1: Widget.Const = vertical(S.spacing._1)
    val _2: Widget.Const = vertical(S.spacing._2)
    val _3: Widget.Const = vertical(S.spacing._3)
    val _4: Widget.Const = vertical(S.spacing._4)
    val _5: Widget.Const = vertical(S.spacing._5)
    val _6: Widget.Const = vertical(S.spacing._6)
    val _7: Widget.Const = vertical(S.spacing._7)
    val _8: Widget.Const = vertical(S.spacing._8)
    val _9: Widget.Const = vertical(S.spacing._9)
    val _10: Widget.Const = vertical(S.spacing._10)
    val _11: Widget.Const = vertical(S.spacing._11)
    val _12: Widget.Const = vertical(S.spacing._12)
    val _13: Widget.Const = vertical(S.spacing._13)
    val _14: Widget.Const = vertical(S.spacing._14)
    val _15: Widget.Const = vertical(S.spacing._15)
    val _16: Widget.Const = vertical(S.spacing._16)
    val _17: Widget.Const = vertical(S.spacing._17)
    val _18: Widget.Const = vertical(S.spacing._18)
    val _19: Widget.Const = vertical(S.spacing._19)
    val _20: Widget.Const = vertical(S.spacing._20)
    val _21: Widget.Const = vertical(S.spacing._21)
    val _22: Widget.Const = vertical(S.spacing._22)
    val _23: Widget.Const = vertical(S.spacing._23)
    val _24: Widget.Const = vertical(S.spacing._24)
    val _25: Widget.Const = vertical(S.spacing._25)

  }

}
