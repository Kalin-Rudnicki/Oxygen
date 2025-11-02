package oxygen.ui.web.defaults

import oxygen.meta.K0
import oxygen.ui.web.create.*

object InlinePseudoClassStyles {

  private val all: Seq[CssBuilder] =
    K0.SumGeneric.EnumGeneric.deriveEnum.lax.values[CssBuilder]

  private def single(b: CssBuilder): String = {
    val mods: Seq[b.InlinePseudoAttr] =
      Seq(b.dynamic, b.dynamic.hover, b.dynamic.active, b.dynamic.hoverActive, b.dynamic.focus, b.dynamic.hoverFocus)

    s"/* --- ${b.key} --- */\n\n" +
      mods
        .map { m => s"${m.cssClassName} {\n  ${b.key}: var(${m.varName});\n}" }
        .mkString("\n\n")
  }

  val compiled: StyleSheet =
    StyleSheet.makeLazy("Inline Pseudo Classes") {
      all.sortBy(_.key).map(single).mkString("\n\n\n")
    }

}
