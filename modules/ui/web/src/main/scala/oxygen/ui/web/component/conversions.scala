package oxygen.ui.web.component

import oxygen.ui.web.Widget

given textToWidget: Conversion[String, Widget.Const] = Widget.text(_)
