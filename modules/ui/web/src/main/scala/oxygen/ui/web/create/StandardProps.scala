package oxygen.ui.web.create

import oxygen.predef.core.*

/**
  * Shared pure data helpers for builders (Decorator/DerivedColors removed in W2).
  *
  * Still used (Dropdown / Table / HorizontalRadio padding). Not dead code.
  */
object StandardProps {

  final case class Padding(topBottom: String, leftRight: String) derives Show {
    val show: String = s"$topBottom $leftRight"
  }
  object Padding {
    val none: Padding = Padding("0px", "0px")
  }

}
