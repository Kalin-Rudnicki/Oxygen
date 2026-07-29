package oxygen.ui.web.component

import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}

/**
  * W11-T02: drag presentation modes (visual only; pair with DnD handlers).
  */
object DragPresentation {

  enum Mode {
    case Ghost // browser default ghost
    case Highlight // elevate row while dragging
    case Opaque // solid drag surface styling
  }

  def styles(mode: Mode): Widget =
    mode match {
      case Mode.Ghost =>
        Widget.empty
      case Mode.Highlight =>
        fragment(
          boxShadow := "0 4px 12px rgba(0,0,0,0.2)",
          outline := s"2px solid ${S.color.primary.standard}",
          opacity := "0.95",
        )
      case Mode.Opaque =>
        fragment(
          backgroundColor := S.color.bg.layerOne,
          opacity := "1",
          border(1.px, "solid", S.color.primary.standard),
        )
    }

  /** Apply presentation when `dragging` is true. */
  def whenDragging(mode: Mode, dragging: Boolean): Widget =
    if dragging then styles(mode) else Widget.empty

}
