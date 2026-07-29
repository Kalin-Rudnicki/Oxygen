package oxygen.ui.web.component

import oxygen.meta.typing.UnionRemoving
import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}
import zio.*

/**
  * HolyGrail-style modal (W2-T06). Decorator for size / padding / chrome.
  *
  * Defaults are tuned for a typical confirm/dialog. Use size presets or explicit
  * `width` / `height` / `padding` for forms, wide tables, and full-screen flows.
  * R2: focus trap TODO.
  */
final case class Modal[-Env, +Action, -StateGet, +StateSet <: StateGet](
    private val _width: String,
    private val _height: String,
    private val _maxWidth: String,
    private val _maxHeight: String,
    private val _minWidth: String,
    private val _padding: String,
    private val _opacityPercent: Double,
    private val _backgroundColor: String,
    private val _borderRadius: String,
    private val _boxShadow: String,
    private val _content: Widget.Polymorphic[Env, Action, StateGet, StateSet],
) extends PWidget.Deferred[Env, Action, StateGet, StateSet] {

  def width(w: String): Modal[Env, Action, StateGet, StateSet] = copy(_width = w)
  def height(h: String): Modal[Env, Action, StateGet, StateSet] = copy(_height = h)
  def maxWidth(w: String): Modal[Env, Action, StateGet, StateSet] = copy(_maxWidth = w)
  def maxHeight(h: String): Modal[Env, Action, StateGet, StateSet] = copy(_maxHeight = h)
  def minWidth(w: String): Modal[Env, Action, StateGet, StateSet] = copy(_minWidth = w)
  def padding(p: String): Modal[Env, Action, StateGet, StateSet] = copy(_padding = p)
  def pad(p: String): Modal[Env, Action, StateGet, StateSet] = padding(p)
  def opacityPercent(p: Double): Modal[Env, Action, StateGet, StateSet] = copy(_opacityPercent = p)
  def backgroundColor(c: String): Modal[Env, Action, StateGet, StateSet] = copy(_backgroundColor = c)
  def borderRadius(r: String): Modal[Env, Action, StateGet, StateSet] = copy(_borderRadius = r)
  def boxShadow(s: String): Modal[Env, Action, StateGet, StateSet] = copy(_boxShadow = s)

  /** Compact confirm / alert (~24rem, auto height). */
  def sm: Modal[Env, Action, StateGet, StateSet] =
    copy(_width = 24.rem, _height = "auto", _maxWidth = 92.vw, _maxHeight = 80.vh, _minWidth = 16.rem, _padding = S.spacing._4)

  /** Default dialog (~32rem). */
  def md: Modal[Env, Action, StateGet, StateSet] =
    copy(_width = 32.rem, _height = "auto", _maxWidth = 92.vw, _maxHeight = 85.vh, _minWidth = 18.rem, _padding = S.spacing._6)

  /** Form / multi-field (~40rem). */
  def lg: Modal[Env, Action, StateGet, StateSet] =
    copy(_width = 40.rem, _height = "auto", _maxWidth = 94.vw, _maxHeight = 90.vh, _minWidth = 20.rem, _padding = S.spacing._6)

  /** Wide content / tables (~56rem). */
  def xl: Modal[Env, Action, StateGet, StateSet] =
    copy(_width = 56.rem, _height = "auto", _maxWidth = 96.vw, _maxHeight = 90.vh, _minWidth = 24.rem, _padding = S.spacing._6)

  /** Near full-viewport sheet. */
  def full: Modal[Env, Action, StateGet, StateSet] =
    copy(_width = 96.vw, _height = 92.vh, _maxWidth = 96.vw, _maxHeight = 92.vh, _minWidth = 0.px, _padding = S.spacing._6)

  /** Tight inner padding (toasts-in-modal, dense chrome). */
  def compact: Modal[Env, Action, StateGet, StateSet] = padding(S.spacing._3)

  /** Roomier body padding. */
  def comfortable: Modal[Env, Action, StateGet, StateSet] = padding(S.spacing._8)

  def apply[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      children: Widget.Polymorphic[Env2, Action2, StateGet2, StateSet2]*,
  ): Modal[Env2, Action2, StateGet2, StateSet2] =
    copy(_content = fragment(_content, Widget.fragment(children)))

  override protected def build: PWidget[Env, Action, StateGet, StateSet] = {
    import oxygen.ui.web.create.{
      backgroundColor as backgroundColorAttr,
      borderRadius as borderRadiusAttr,
      boxShadow as boxShadowAttr,
      height as heightAttr,
      maxHeight as maxHeightAttr,
      maxWidth as maxWidthAttr,
      minWidth as minWidthAttr,
      padding as paddingAttr,
      width as widthAttr,
    }
    div(
      O.ModalOverlay,
      // TODO (R2): focus trap
      backgroundColorAttr := s"rgba(0, 0, 0, ${_opacityPercent / 100.0})",
      div(
        O.ModalOverlay.Modal,
        widthAttr := _width,
        heightAttr := _height,
        maxWidthAttr := _maxWidth,
        maxHeightAttr := _maxHeight,
        minWidthAttr := _minWidth,
        paddingAttr := _padding,
        backgroundColorAttr := _backgroundColor,
        borderRadiusAttr := _borderRadius,
        boxShadowAttr := _boxShadow,
        boxSizing.borderBox,
        overflow.auto,
        onClick.e.handle { e =>
          e.stopPropagation(); ZIO.unit
        },
        _content,
      ),
    )
  }

  def option[Env2 <: Env: HasNoScope, Action2 >: Action | Modal.Close, State](
      contents: WidgetEAS[Env2, Action2, State]*,
  )(using ev: UnionRemoving[Action2, Modal.Close]): WidgetEAS[Env2, ev.Remaining, Option[State]] =
    Modal.renderOption(this, contents*)

}
object Modal extends WidgetTypes[Modal] {

  type Close = Close.type
  case object Close

  /**
    * Default: medium dialog, auto height, comfortable padding, soft elevation.
    * Scrim is CSS-controlled via [[O.ModalOverlay]]; panel uses theme layerOne.
    */
  val empty: Modal.Const =
    Modal(
      _width = 32.rem,
      _height = "auto",
      _maxWidth = 92.vw,
      _maxHeight = 85.vh,
      _minWidth = 18.rem,
      _padding = S.spacing._6,
      _opacityPercent = 70,
      _backgroundColor = S.color.bg.layerOne,
      _borderRadius = S.borderRadius.l,
      _boxShadow = "0 16px 48px rgba(0,0,0,0.28)",
      _content = Widget.empty,
    )

  def apply(): Modal.Const = empty

  def apply(configure: Modal.Const => Modal.Const): Modal.Const = configure(empty)

  def option[Env: HasNoScope, Action >: Close, State](
      configure: Modal.Const => Modal.Const = identity,
  )(contents: WidgetEAS[Env, Action, State]*)(using
      ev: UnionRemoving[Action, Close],
  ): WidgetEAS[Env, ev.Remaining, Option[State]] =
    configure(empty).option(contents*)

  def option[Env: HasNoScope, Action >: Close, State](
  )(contents: WidgetEAS[Env, Action, State]*)(using
      ev: UnionRemoving[Action, Close],
  ): WidgetEAS[Env, ev.Remaining, Option[State]] =
    empty.option(contents*)

  private def renderOption[Env: HasNoScope, Action >: Close, State](
      modal: Modal[?, ?, ?, ?],
      contents: WidgetEAS[Env, Action, State]*,
  )(using ev: UnionRemoving[Action, Close]): WidgetEAS[Env, ev.Remaining, Option[State]] = {
    import oxygen.ui.web.create.{
      backgroundColor as backgroundColorAttr,
      borderRadius as borderRadiusAttr,
      boxShadow as boxShadowAttr,
      height as heightAttr,
      maxHeight as maxHeightAttr,
      maxWidth as maxWidthAttr,
      minWidth as minWidthAttr,
      padding as paddingAttr,
      width as widthAttr,
    }
    val tmp1: WidgetEAS[Env, Action, State] =
      div(
        O.ModalOverlay,
        // TODO (R2): focus trap
        backgroundColorAttr := s"rgba(0, 0, 0, ${modal._opacityPercent / 100.0})",
        onClick.action(Close),
        div(
          O.ModalOverlay.Modal,
          widthAttr := modal._width,
          heightAttr := modal._height,
          maxWidthAttr := modal._maxWidth,
          maxHeightAttr := modal._maxHeight,
          minWidthAttr := modal._minWidth,
          paddingAttr := modal._padding,
          backgroundColorAttr := modal._backgroundColor,
          borderRadiusAttr := modal._borderRadius,
          boxShadowAttr := modal._boxShadow,
          boxSizing.borderBox,
          overflow.auto,
          onClick.e.handle { e =>
            e.stopPropagation(); ZIO.unit
          },
        )(contents*),
      )

    val tmp2: WidgetEAS[Env, Action, Option[State]] =
      div(Widget.sum.option(tmp1))

    tmp2.handleActionStateful.ps[Close] { case (s, Close) => s.set(None) }
  }

}
