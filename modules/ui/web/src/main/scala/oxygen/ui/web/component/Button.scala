package oxygen.ui.web.component

import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}

/**
  * HolyGrail-style button builder (W2-T02). Pure CSS vars for colors — no Decorator / getColorValue.
  * W9-T05: optional leading/trailing [[Icon]] slots.
  */
final case class Button[-Env, +Action, -StateGet, +StateSet <: StateGet](
    private val size: Size,
    private val intent: Intent,
    private val variant: ControlVariant,
    private val disabled: Boolean,
    private val progress: Boolean,
    private val _content: Widget.Polymorphic[Env, Action, StateGet, StateSet],
    private val leadingIcon: Option[Icon] = None,
    private val trailingIcon: Option[Icon] = None,
) extends PWidget.Deferred[Env, Action, StateGet, StateSet] {

  def size(s: Size): Button[Env, Action, StateGet, StateSet] = copy(size = s)
  def intent(i: Intent): Button[Env, Action, StateGet, StateSet] = copy(intent = i)
  def variant(v: ControlVariant): Button[Env, Action, StateGet, StateSet] = copy(variant = v)
  def disabled(d: Boolean): Button[Env, Action, StateGet, StateSet] = copy(disabled = d)
  def progress(p: Boolean): Button[Env, Action, StateGet, StateSet] = copy(progress = p)

  def extraSmall: Button[Env, Action, StateGet, StateSet] = size(Size.ExtraSmall)
  def small: Button[Env, Action, StateGet, StateSet] = size(Size.Small)
  def medium: Button[Env, Action, StateGet, StateSet] = size(Size.Medium)
  def large: Button[Env, Action, StateGet, StateSet] = size(Size.Large)
  def extraLarge: Button[Env, Action, StateGet, StateSet] = size(Size.ExtraLarge)

  def primary: Button[Env, Action, StateGet, StateSet] = intent(Intent.Primary)
  def positive: Button[Env, Action, StateGet, StateSet] = intent(Intent.Success)
  def success: Button[Env, Action, StateGet, StateSet] = intent(Intent.Success)
  def negative: Button[Env, Action, StateGet, StateSet] = intent(Intent.Danger)
  def danger: Button[Env, Action, StateGet, StateSet] = intent(Intent.Danger)
  def destructive: Button[Env, Action, StateGet, StateSet] = intent(Intent.Danger)
  def alert: Button[Env, Action, StateGet, StateSet] = intent(Intent.Warning)
  def warning: Button[Env, Action, StateGet, StateSet] = intent(Intent.Warning)
  def informational: Button[Env, Action, StateGet, StateSet] = intent(Intent.Info)
  def info: Button[Env, Action, StateGet, StateSet] = intent(Intent.Info)
  def accent: Button[Env, Action, StateGet, StateSet] = intent(Intent.Accent)
  def neutral: Button[Env, Action, StateGet, StateSet] = intent(Intent.Neutral)

  def solid: Button[Env, Action, StateGet, StateSet] = variant(ControlVariant.Solid)
  def standard: Button[Env, Action, StateGet, StateSet] = variant(ControlVariant.Solid)
  def subtle: Button[Env, Action, StateGet, StateSet] = variant(ControlVariant.Subtle)
  def minimal: Button[Env, Action, StateGet, StateSet] = variant(ControlVariant.Minimal)

  def leading(icon: Icon): Button[Env, Action, StateGet, StateSet] = copy(leadingIcon = Some(icon))
  def trailing(icon: Icon): Button[Env, Action, StateGet, StateSet] = copy(trailingIcon = Some(icon))
  def iconOnly(icon: Icon): Button[Env, Action, StateGet, StateSet] =
    copy(leadingIcon = Some(icon), trailingIcon = None, _content = Widget.empty)

  def modContent[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      f: Widget.Polymorphic[Env, Action, StateGet, StateSet] => Widget.Polymorphic[Env2, Action2, StateGet2, StateSet2],
  ): Button[Env2, Action2, StateGet2, StateSet2] =
    copy(_content = f(_content))

  def content[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      children: Widget.Polymorphic[Env2, Action2, StateGet2, StateSet2]*,
  ): Button[Env2, Action2, StateGet2, StateSet2] =
    modContent { fragment(_, Widget.fragment(children)) }

  def apply[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      children: Widget.Polymorphic[Env2, Action2, StateGet2, StateSet2]*,
  ): Button[Env2, Action2, StateGet2, StateSet2] =
    modContent { fragment(_, Widget.fragment(children)) }

  override protected def build: PWidget[Env, Action, StateGet, StateSet] = {
    val (pad, radius, fontSz, fontWt) = Button.sizeTokens(size)
    val role = Button.roleVars(intent)
    val iconPx = Button.iconSizePx(size)
    val colorStyles: Widget = (disabled, progress, variant) match {
      case (true, _, _) | (_, true, _) =>
        fragment(
          color.dynamic := S.color.fg.subtle,
          backgroundColor.dynamic := S.color.bg.layerTwo,
          borderColor.dynamic := S.color.bg.transparent,
          cursor := (if progress then "progress" else "not-allowed"),
        )
      case (_, _, ControlVariant.Solid) =>
        fragment(
          // On-fill ink (site audits): pairs with solid role, not page fg.inverse
          color.dynamic := role.on,
          backgroundColor.dynamic := role.standard,
          backgroundColor.dynamic.hover := role.hover,
          backgroundColor.dynamic.active := role.active,
          borderColor.dynamic := role.hover,
        )
      case (_, _, ControlVariant.Subtle) =>
        fragment(
          color.dynamic := role.standard,
          backgroundColor.dynamic := role.subtle,
          backgroundColor.dynamic.hover := role.minimal,
          backgroundColor.dynamic.active := role.hover,
          borderColor.dynamic := role.standard,
        )
      case (_, _, ControlVariant.Minimal) =>
        fragment(
          color.dynamic := role.standard,
          backgroundColor.dynamic := S.color.bg.transparent,
          backgroundColor.dynamic.hover := role.subtle,
          backgroundColor.dynamic.active := role.minimal,
          borderColor.dynamic := S.color.bg.transparent,
          borderColor.dynamic.hover := role.standard,
        )
    }

    val body: Widget.Polymorphic[Env, Action, StateGet, StateSet] =
      fragment(
        display.inlineFlex,
        alignItems.center,
        justifyContent.center,
        gap := S.spacing._2,
        leadingIcon.map(_.size(iconPx)).getOrElse(Widget.empty),
        _content,
        trailingIcon.map(_.size(iconPx)).getOrElse(Widget.empty),
      )

    button(
      O.Button,
      borderStyle := "solid",
      borderWidth := 1.px,
      padding := pad,
      borderRadius := radius,
      fontSize := fontSz,
      fontWeight := fontWt,
      cursor := "pointer",
      colorStyles,
      body,
    )
  }

}
object Button extends WidgetTypes[Button] {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Public API
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  val empty: Button.Const =
    Button(Size.Medium, Intent.Primary, ControlVariant.Solid, false, false, Widget.empty)

  def apply(): Button.Const = empty

  def apply(text: String): Button.Const =
    empty.content(text)

  def apply[Env, Action, StateGet, StateSet <: StateGet](
      children: Widget.Polymorphic[Env, Action, StateGet, StateSet]*,
  ): Button[Env, Action, StateGet, StateSet] =
    empty.content(children*)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Form (submit control, composable Deferred builder)
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /**
    * Submit button form builder. Raises [[Form.Submit]] on click.
    * When [[lockAware]] is true (default), disables/progresses under [[PageLock]] (W4-T04).
    *
    * TODO (KR): `_button: Button.Const` is lazy — should carry Env/Action/State type params
    * instead of hardcoding Const (same as other form builders).
    *
    * {{{
    * Button.form("Login").medium.primary
    * Button.form("Save").modButton(_.positive.subtle)
    * }}}
    */
  final case class form private (
      private val _text: String,
      private val _button: Button.Const,
      private val _surroundingPadding: String,
      private val _lockAware: Boolean,
  ) extends PForm.Deferred.Stateless[Any, Form.Submit, Unit] {

    override protected lazy val build: PForm[Any, Form.Submit, Any, Nothing, Unit] = {
      def submitBtn(locked: Boolean): Button[Any, Form.Submit, Any, Nothing] =
        _button
          .disabled(locked)
          .progress(locked)
          .content(_text, onClick.action(Form.Submit))
      val body: WidgetA[Form.Submit] =
        if _lockAware then PageLock.bindPage[Any, Form.Submit, Any, Nothing](submitBtn)
        else submitBtn(false)
      Form.unit(
        div(
          padding := _surroundingPadding,
          body,
        ),
      )
    }

    /** Drill into the underlying [[Button]] builder. */
    def modButton(f: Button.Const => Button.Const): form = copy(_button = f(_button))
    def button: Button.Const = _button

    def size(s: Size): form = modButton(_.size(s))
    def intent(i: Intent): form = modButton(_.intent(i))
    def variant(v: ControlVariant): form = modButton(_.variant(v))
    def extraSmall: form = modButton(_.extraSmall)
    def small: form = modButton(_.small)
    def medium: form = modButton(_.medium)
    def large: form = modButton(_.large)
    def extraLarge: form = modButton(_.extraLarge)
    def primary: form = modButton(_.primary)
    def positive: form = modButton(_.positive)
    def negative: form = modButton(_.negative)
    def alert: form = modButton(_.alert)
    def informational: form = modButton(_.informational)
    def solid: form = modButton(_.solid)
    def subtle: form = modButton(_.subtle)
    def minimal: form = modButton(_.minimal)
    def leading(icon: Icon): form = modButton(_.leading(icon))
    def trailing(icon: Icon): form = modButton(_.trailing(icon))

    def surroundingPadding(p: String): form = copy(_surroundingPadding = p)
    def lockAware(v: Boolean): form = copy(_lockAware = v)
    def noLockAware: form = lockAware(false)

  }
  object form {

    def apply(buttonMainText: String): Button.form =
      new Button.form(
        _text = buttonMainText,
        _button = Button.empty.medium.primary.solid,
        _surroundingPadding = css(10.px, 35.px),
        _lockAware = true,
      )

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Private helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private final case class RoleVars(
      standard: CSSVar,
      hover: CSSVar,
      active: CSSVar,
      subtle: CSSVar,
      minimal: CSSVar,
      on: CSSVar,
  )

  private def roleVars(intent: Intent): RoleVars =
    intent match {
      case Intent.Primary => RoleVars(S.color.primary.standard, S.color.primary.hover, S.color.primary.active, S.color.primary.subtle, S.color.primary.minimal, S.color.primary.on)
      case Intent.Accent  => RoleVars(
          S.color.highlight.accent.standard,
          S.color.highlight.accent.hover,
          S.color.highlight.accent.active,
          S.color.highlight.accent.subtle,
          S.color.highlight.accent.minimal,
          S.color.highlight.accent.on,
        )
      // Neutral solid: mid gray fill → use global white as best-effort on-fill (rarely solid)
      case Intent.Neutral => RoleVars(S.color.fg.moderate, S.color.fg.default, S.color.fg.subtle, S.color.bg.layerOne, S.color.bg.layerTwo, S.color.fg.globalWhite)
      case Intent.Success => RoleVars(
          S.color.status.positive.standard,
          S.color.status.positive.hover,
          S.color.status.positive.active,
          S.color.status.positive.subtle,
          S.color.status.positive.minimal,
          S.color.status.positive.on,
        )
      case Intent.Warning => RoleVars(
          S.color.status.alert.standard,
          S.color.status.alert.hover,
          S.color.status.alert.active,
          S.color.status.alert.subtle,
          S.color.status.alert.minimal,
          S.color.status.alert.on,
        )
      case Intent.Danger => RoleVars(
          S.color.status.negative.standard,
          S.color.status.negative.hover,
          S.color.status.negative.active,
          S.color.status.negative.subtle,
          S.color.status.negative.minimal,
          S.color.status.negative.on,
        )
      case Intent.Info => RoleVars(
          S.color.status.informational.standard,
          S.color.status.informational.hover,
          S.color.status.informational.active,
          S.color.status.informational.subtle,
          S.color.status.informational.minimal,
          S.color.status.informational.on,
        )
    }

  private def sizeTokens(size: Size): (String, String, String, String) =
    size match {
      case Size.ExtraSmall => (css(S.spacing._2px, S.spacing._3), S.borderRadius._2, S.fontSize._2, S.fontWeight.semiBold)
      case Size.Small      => (css(S.spacing._1, S.spacing._4), S.borderRadius._4, S.fontSize._3, S.fontWeight.semiBold)
      case Size.Medium     => (css(S.spacing._2, S.spacing._8), S.borderRadius._5, S.fontSize._4, S.fontWeight.bold)
      case Size.Large      => (css(S.spacing._3, S.spacing._10), S.borderRadius._5, S.fontSize._4, S.fontWeight.bold)
      case Size.ExtraLarge => (css(S.spacing._4, S.spacing._14), S.borderRadius._7, S.fontSize._5, S.fontWeight.bold)
    }

  private def iconSizePx(size: Size): Int =
    size match {
      case Size.ExtraSmall => Icon.Size.xs
      case Size.Small      => Icon.Size.sm
      case Size.Medium     => Icon.Size.md
      case Size.Large      => Icon.Size.lg
      case Size.ExtraLarge => Icon.Size.lg
    }

}
