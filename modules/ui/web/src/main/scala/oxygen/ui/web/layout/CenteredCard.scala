package oxygen.ui.web.layout

import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}

// TODO (KR) : this probably needs some `mobile` awareness...
/**
  * Centered card page body (auth / marketing). Inline styles only — no stylesheet class.
  */
final case class CenteredCard[-Env, +Action, -StateGet, +StateSet <: StateGet](
    private val _pageBg: String,
    private val _cardBg: String,
    private val _maxWidth: String,
    private val _minWidth: String,
    private val _width: String,
    private val _maxHeight: String,
    private val _padding: String,
    private val _borderRadius: String,
    private val _boxShadow: Option[String],
    private val _content: Widget.Polymorphic[Env, Action, StateGet, StateSet],
) extends PWidget.Deferred[Env, Action, StateGet, StateSet] {

  def pageBg(c: String): CenteredCard[Env, Action, StateGet, StateSet] = copy(_pageBg = c)
  def cardBg(c: String): CenteredCard[Env, Action, StateGet, StateSet] = copy(_cardBg = c)
  def maxWidth(w: String): CenteredCard[Env, Action, StateGet, StateSet] = copy(_maxWidth = w)
  def minWidth(w: String): CenteredCard[Env, Action, StateGet, StateSet] = copy(_minWidth = w)
  def width(w: String): CenteredCard[Env, Action, StateGet, StateSet] = copy(_width = w)
  def maxHeight(h: String): CenteredCard[Env, Action, StateGet, StateSet] = copy(_maxHeight = h)
  def padding(p: String): CenteredCard[Env, Action, StateGet, StateSet] = copy(_padding = p)
  def borderRadius(r: String): CenteredCard[Env, Action, StateGet, StateSet] = copy(_borderRadius = r)
  def boxShadow(s: String): CenteredCard[Env, Action, StateGet, StateSet] = copy(_boxShadow = Some(s))
  def noBoxShadow: CenteredCard[Env, Action, StateGet, StateSet] = copy(_boxShadow = None)

  def apply[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      children: Widget.Polymorphic[Env2, Action2, StateGet2, StateSet2]*,
  ): CenteredCard[Env2, Action2, StateGet2, StateSet2] =
    copy(_content = fragment(_content, Widget.fragment(children)))

  override protected def build: PWidget[Env, Action, StateGet, StateSet] = {
    import oxygen.ui.web.create.{
      backgroundColor as backgroundColorAttr,
      borderRadius as borderRadiusAttr,
      boxShadow as boxShadowAttr,
      maxHeight as maxHeightAttr,
      maxWidth as maxWidthAttr,
      minWidth as minWidthAttr,
      padding as paddingAttr,
      width as widthAttr,
    }
    div(
      display.flex,
      flexDirection.column,
      justifyContent.center,
      alignItems.center,
      backgroundColorAttr := _pageBg,
      minHeight := 100.vh,
      widthAttr := 100.pct,
      div(
        backgroundColorAttr := _cardBg,
        display.flex,
        flexDirection.column,
        alignItems.center,
        justifyContent.center,
        maxHeightAttr := _maxHeight,
        minWidthAttr := _minWidth,
        maxWidthAttr := _maxWidth,
        widthAttr := _width,
        borderRadiusAttr := _borderRadius,
        paddingAttr := _padding,
        _boxShadow.fold[Widget](Widget.empty)(s => boxShadowAttr := s),
        _content,
      ),
    )
  }

}
object CenteredCard extends WidgetTypes[CenteredCard] {

  val empty: CenteredCard.Const =
    CenteredCard(
      _pageBg = S.color.bg.default,
      _cardBg = S.color.bg.layerOne,
      _maxWidth = 600.px,
      _minWidth = "min(300px, 100%)",
      _width = 50.pct,
      _maxHeight = 70.pct,
      _padding = 40.px,
      _borderRadius = 40.px,
      _boxShadow = None,
      _content = Widget.empty,
    )

  def apply(): CenteredCard.Const = empty

  def apply(configure: CenteredCard.Const => CenteredCard.Const): CenteredCard.Const =
    configure(empty)

  def apply[Env, Action, StateGet, StateSet <: StateGet](
      content: Widget.Polymorphic[Env, Action, StateGet, StateSet]*,
  ): CenteredCard[Env, Action, StateGet, StateSet] =
    empty(content*)

}
