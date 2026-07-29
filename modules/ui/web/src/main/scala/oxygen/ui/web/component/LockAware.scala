package oxygen.ui.web.component

import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}

/**
  * W4-T04 / W4-T05: bind widgets to [[PageLock]] and show region overlays.
  *
  * TODO (KR): revisit after remaining component cleanup settles.
  *
  * Pattern:
  * {{{
  * PageLock.bindPage { locked =>
  *   Button("Save").disabled(locked).progress(locked)
  * }
  * // or
  * LockAware.pageButton("Save")(_.medium)
  * }}}
  */
object LockAware {

  /** Page-lock-aware button (disabled + progress cursor while page locked). */
  def pageButton(
      text: String,
      configure: Button.Const => Button.Const = identity,
  ): Widget =
    PageLock.bindPage { locked =>
      configure(Button(text)).disabled(locked).progress(locked)
    }

  /** Region-lock-aware button. */
  def regionButton(
      regionId: String,
      text: String,
      configure: Button.Const => Button.Const = identity,
  ): Widget =
    PageLock.bindRegion(regionId) { locked =>
      configure(Button(text)).disabled(locked).progress(locked)
    }

  /**
    * W4-T05: wrap a subtree; when page or region is locked, a translucent overlay
    * blocks pointer events (visual busy affordance).
    */
  def region(
      regionId: String,
      content: Widget*,
  ): Widget =
    PageLock.bind { lock =>
      val locked = lock.regionLocked(regionId)
      div(
        position.relative,
        Widget.fragment(content),
        Widget.when(locked) {
          div(
            position.absolute,
            top := 0.px,
            left := 0.px,
            right := 0.px,
            bottom := 0.px,
            backgroundColor := S.color.bg.transparent,
            cursor := "progress",
            zIndex := ZIndices.pageMessages, // above content; messages stay higher if needed
            Widget.raw.css("pointer-events", "all"),
          )
        },
      )
    }

  /** Whole-page overlay when page lock is held. */
  def page(
      content: Widget*,
  ): Widget =
    PageLock.bindPage { locked =>
      div(
        position.relative,
        Widget.fragment(content),
        Widget.when(locked) {
          div(
            position.absolute,
            top := 0.px,
            left := 0.px,
            right := 0.px,
            bottom := 0.px,
            backgroundColor := S.color.bg.transparent,
            cursor := "progress",
            zIndex := ZIndices.pageMessages,
            Widget.raw.css("pointer-events", "all"),
          )
        },
      )
    }

}
