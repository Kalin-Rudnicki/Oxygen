package oxygen.ui.web.defaults

import oxygen.ui.web.component.{ColumnsStyle, SortableList, Tooltip, TopBar}
import oxygen.ui.web.create.{Motion, OxygenStyleSheet, StyleSheet}
import oxygen.ui.web.layout.HolyGrail
import scala.collection.immutable.ArraySeq

/**
  * Stylesheets required for Oxygen UI to behave correctly in a typical [[oxygen.ui.web.PageApp]].
  *
  * Includes: CSS reset, theme color vars (light + dark), inline pseudo-class support,
  * core component classes, columns, HolyGrail responsive shell, motion tokens, tooltips,
  * sortable-list drag chrome.
  *
  * Append app-specific sheets after this:
  * {{{
  * override val styleSheets: ArraySeq[StyleSheet] =
  *   coreOxygenStyleSheets ++ ArraySeq(MyApp.sheet)
  * }}}
  */
val coreOxygenStyleSheets: ArraySeq[StyleSheet] =
  ArraySeq(normalizeCssReset) ++
    OxygenStyleVarDefaults.oxygenColorSheets.to(ArraySeq) ++
    ArraySeq(
      InlinePseudoClassStyles.compiled,
      OxygenStyleSheet.compiled,
      ColumnsStyle.sheet,
      HolyGrail.responsiveSheet,
      TopBar.responsiveSheet,
      Motion.sheet,
      Tooltip.sheet,
      SortableList.sheet,
    )
