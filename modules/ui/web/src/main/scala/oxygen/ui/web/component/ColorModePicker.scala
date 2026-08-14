package oxygen.ui.web.component

import org.scalajs.dom.{document, window, HTMLElement, KeyboardEvent}
import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}
import oxygen.ui.web.service.{Broadcast, ColorMode}
import zio.*

/**
  * Reusable, drop-in color-mode picker (Light / Dark / System).
  *
  * A first-class, standalone widget for switching color mode. It is fully independent of the
  * theme-pack machinery ([[ThemePicker]] / `OxygenThemes` / `Theme`) — it depends only on the
  * [[ColorMode]] service and stands on its own.
  *
  * Self-contained: backed by a shared [[GlobalState]] seeded from the stored [[ColorMode]]
  * preference, so it reflects the current mode and can be dropped into any page without wiring
  * page state. Selecting an option calls [[ColorMode.setAndPersist]] (persists + rebinds
  * `data-color-mode` + notifies other tabs) and updates the highlight.
  *
  * Variants (config builder, à la [[ToggleThumb]] / [[HorizontalRadio]]):
  *   - [[segmented]] (default) — a `role=radiogroup` Light / Dark / System segmented control with
  *     full keyboard support (arrows / Home / End / Space / Enter) and roving `tabindex`.
  *   - [[compact]] — a single icon button that cycles modes; ideal for a top bar.
  * (A form/dropdown variant is intentionally omitted — `HorizontalRadio.form[ColorMode.Mode]`
  * already covers labeled form contexts.)
  *
  * a11y:
  *   - segmented: `role=radiogroup`, `role=radio` + `aria-checked` per option, roving `tabindex`,
  *     keyboard navigation that moves selection + DOM focus together.
  *   - compact: a native `<button>` with a descriptive `aria-label` / `title` announcing the
  *     current mode and the next action.
  *
  * Live updates: by default the highlight reflects the mode at render time. To make it live-update
  * on cross-tab / programmatic `ColorMode` changes, wire [[ColorModePicker.syncAcrossTabs]] once
  * (e.g. from `prePageLoad`); it pushes broadcast changes into the shared state and re-renders.
  *
  * {{{
  * ColorModePicker()                              // segmented control
  * ColorModePicker(label = "Color mode".some)     // with a leading caption
  * ColorModePicker.compact.small                  // icon cycle button for a top bar
  * ColorModePicker().lightDarkOnly                 // drop the System option
  * }}}
  */
final case class ColorModePicker(
    private val _variant: ColorModePicker.Variant,
    private val _size: ColorModePicker.Size,
    private val _label: Option[String],
    private val _includeSystem: Boolean,
    private val _showIcons: Boolean,
    private val _idPrefix: String,
) extends PWidget.Deferred.Stateless[Any, Nothing] {

  import ColorModePicker.*

  // ////////////////////////////////////////////////////////////////////////////////////////////
  //      Config
  // ////////////////////////////////////////////////////////////////////////////////////////////

  def variant(v: Variant): ColorModePicker = copy(_variant = v)
  def segmented: ColorModePicker = variant(Variant.Segmented)
  def compact: ColorModePicker = variant(Variant.Compact)

  def size(s: Size): ColorModePicker = copy(_size = s)
  def small: ColorModePicker = size(Size.small)
  def medium: ColorModePicker = size(Size.medium)
  def large: ColorModePicker = size(Size.large)

  def label(text: String): ColorModePicker = copy(_label = Some(text))
  def label(text: Option[String]): ColorModePicker = copy(_label = text)
  def noLabel: ColorModePicker = copy(_label = None)

  def includeSystem(b: Boolean): ColorModePicker = copy(_includeSystem = b)
  def lightDarkOnly: ColorModePicker = includeSystem(false)

  def showIcons(b: Boolean): ColorModePicker = copy(_showIcons = b)
  def withIcons: ColorModePicker = showIcons(true)
  def noIcons: ColorModePicker = showIcons(false)

  def idPrefix(p: String): ColorModePicker = copy(_idPrefix = p)

  private def modes: List[ColorMode.Mode] = Logic.options(_includeSystem)

  // ////////////////////////////////////////////////////////////////////////////////////////////
  //      Render
  // ////////////////////////////////////////////////////////////////////////////////////////////

  override protected def build: PWidget[Any, Nothing, Any, Nothing] =
    _variant match {
      case Variant.Segmented => renderSegmented
      case Variant.Compact   => renderCompact
    }

  private def select(st: WidgetState[ColorMode.Mode], mode: ColorMode.Mode): UIO[Unit] =
    ColorMode.setAndPersist(mode) *> st.set(mode)

  private def optionId(mode: ColorMode.Mode): String =
    s"${_idPrefix}-${mode.lower}"

  private def focusOption(mode: ColorMode.Mode): UIO[Unit] =
    ZIO.succeed {
      Option(document.getElementById(optionId(mode))).foreach {
        case el: HTMLElement => el.focus()
        case _               => ()
      }
    }

  private def iconWidget(mode: ColorMode.Mode): Widget =
    Logic.iconFor(mode).size(_size.iconPx).decorativeIcon

  private def leadingLabel: Widget =
    _label match {
      case Some(text) => span(text, color := S.color.fg.moderate, fontSize := _size.fontSize)
      case None       => Widget.empty
    }

  // --- Segmented -------------------------------------------------------------------------------

  private def segmentedOption(
      st: WidgetState[ColorMode.Mode],
      mode: ColorMode.Mode,
      current: ColorMode.Mode,
  ): Widget = {
    val selected: Boolean = mode == current
    span(
      id := optionId(mode),
      Widget.raw.htmlAttr("role", "radio"),
      Widget.raw.htmlAttr("aria-checked", if selected then "true" else "false"),
      Widget.raw.htmlAttr("aria-label", mode.pretty),
      Widget.raw.htmlAttr("tabindex", if selected then "0" else "-1"),
      display.inlineFlex,
      alignItems.center,
      gap := S.spacing._2,
      cursor.pointer,
      userSelect.none,
      outline := "none",
      padding(_size.padV, _size.padH),
      fontSize := _size.fontSize,
      fontWeight := S.fontWeight.semiBold,
      borderStyle.solid,
      borderWidth := 1.px,
      if selected then
        fragment(
          color.dynamic := S.color.primary.on,
          backgroundColor.dynamic := S.color.primary.standard,
          borderColor.dynamic := S.color.primary.standard,
        )
      else
        fragment(
          color.dynamic := S.color.fg.default,
          backgroundColor.dynamic := S.color.bg.layerTwo,
          backgroundColor.dynamic.hover := S.color.bg.layerThree,
          borderColor.dynamic := S.color.bg.layerThree,
        )
      ,
      if _showIcons then iconWidget(mode) else Widget.empty,
      mode.pretty,
      onClick := select(st, mode),
    )
  }

  private def renderSegmented: Widget =
    Widget.state[ColorMode.Mode].detach(ModeState) { st =>
      val current: ColorMode.Mode = Logic.effectiveCurrent(modes, st.renderTimeValue)
      div(
        display.inlineFlex,
        alignItems.center,
        gap := S.spacing._2,
        flexWrap.wrap,
        leadingLabel,
        div(
          Widget.raw.htmlAttr("role", "radiogroup"),
          Widget.raw.htmlAttr("aria-label", _label.getOrElse("Color mode")),
          display.inlineFlex,
          borderRadius := S.borderRadius._3,
          overflow.hidden,
          onKeyDown.e.handle { (e: KeyboardEvent) =>
            Logic.keyToNav(e.key) match {
              case Some(nav) =>
                val next = Logic.resolveNav(modes, current, nav)
                e.preventDefault()
                if next == current then focusOption(next)
                else select(st, next) *> focusOption(next)
              case None => ZIO.unit
            }
          },
          Widget.foreach(modes)(segmentedOption(st, _, current)),
        ),
      )
    }

  // --- Compact ---------------------------------------------------------------------------------

  private def renderCompact: Widget =
    Widget.state[ColorMode.Mode].detach(ModeState) { st =>
      val current: ColorMode.Mode = Logic.effectiveCurrent(modes, st.renderTimeValue)
      val next: ColorMode.Mode = Logic.cycle(modes, current)
      val describe: String = s"Color mode: ${current.pretty}. Activate to switch to ${next.pretty}."
      val btn =
        if _showIcons then Button().iconOnly(Logic.iconFor(current)).subtle
        else Button(current.pretty).subtle
      val sized = _size match {
        case Size.small => btn.small
        case Size.large => btn.large
        case _          => btn.medium
      }
      div(
        display.inlineFlex,
        alignItems.center,
        gap := S.spacing._2,
        flexWrap.wrap,
        leadingLabel,
        sized.content(
          Widget.raw.htmlAttr("aria-label", describe),
          Widget.raw.htmlAttr("title", describe),
          onClick := select(st, next),
        ),
      )
    }

}
object ColorModePicker {

  // ////////////////////////////////////////////////////////////////////////////////////////////
  //      Public entry points (backward compatible)
  // ////////////////////////////////////////////////////////////////////////////////////////////

  /**
    * Per-instance id counter (scala.js is single-threaded, so a plain `var` is safe).
    * Guarantees every default-configured picker gets a document-unique `idPrefix`, so multiple
    * pickers on one page don't emit colliding option ids (which would break roving-tabindex focus
    * and produce invalid duplicate-id HTML). An explicit `.idPrefix(...)` still overrides this.
    */
  private var instanceCounter: Long = 0L
  private def freshIdPrefix(): String = {
    val n = instanceCounter
    instanceCounter += 1L
    s"oxygen-color-mode-$n"
  }

  /** Default segmented Light / Dark / System control. Pass `label` to render a leading caption. */
  def apply(label: Option[String] = None): ColorModePicker =
    ColorModePicker(
      _variant = Variant.Segmented,
      _size = Size.small,
      _label = label,
      _includeSystem = true,
      _showIcons = false,
      _idPrefix = freshIdPrefix(),
    )

  /** Compact single-button cycle control (icon by default), ideal for a top bar. */
  def compact: ColorModePicker = apply().compact.withIcons

  /** Segmented control with a leading Light/Dark/System icon on each option. */
  def segmentedWithIcons: ColorModePicker = apply().withIcons

  // ////////////////////////////////////////////////////////////////////////////////////////////
  //      Shared state
  // ////////////////////////////////////////////////////////////////////////////////////////////

  private def readStored: ColorMode.Mode =
    Option(window.localStorage.getItem(ColorMode.storageKey)).flatMap(ColorMode.parse).getOrElse(ColorMode.Mode.System)

  private object ModeState extends GlobalState[ColorMode.Mode]("ColorModePicker")(readStored)

  /**
    * Opt-in: keep every mounted [[ColorModePicker]] highlight in sync with cross-tab /
    * programmatic [[ColorMode]] changes for the lifetime of the provided [[Scope]].
    *
    * Subscribes to the [[Broadcast]] color-mode channel and pushes changes into the shared state
    * (which re-renders the current page). Cross-tab *application* of the mode already happens via
    * `ColorMode.subscribeCrossTab` / `ColorTheme.install`; this only updates the picker's
    * highlight. Wire once (e.g. from `prePageLoad`).
    *
    * {{{
    * override protected def prePageLoad: RIO[Env & Scope, Unit] =
    *   ColorTheme.install *> ColorModePicker.syncAcrossTabs
    * }}}
    */
  def syncAcrossTabs: URIO[Scope, Unit] =
    Broadcast.subscribeThemeMode.foreach(ModeState.set).forkScoped.unit

  // ////////////////////////////////////////////////////////////////////////////////////////////
  //      Config types
  // ////////////////////////////////////////////////////////////////////////////////////////////

  enum Variant { case Segmented, Compact }

  final case class Size(fontSize: String, padV: String, padH: String, iconPx: Int)
  object Size {
    val small: Size = Size(S.fontSize._2, S.spacing._1, S.spacing._3, Icon.Size.sm)
    val medium: Size = Size(S.fontSize._4, s"calc(${S.spacing._1} * 1.5)", S.spacing._4, Icon.Size.md)
    val large: Size = Size(S.fontSize._6, S.spacing._2, S.spacing._5, Icon.Size.lg)
  }

  // ////////////////////////////////////////////////////////////////////////////////////////////
  //      Pure decision logic (DOM-free, unit-tested)
  // ////////////////////////////////////////////////////////////////////////////////////////////

  /** Keyboard-navigation intents within a segmented radiogroup. */
  enum Nav { case Prev, Next, First, Last, Select }

  object Logic {

    /** Ordered options; `System` is included only when `includeSystem`. */
    def options(includeSystem: Boolean): List[ColorMode.Mode] =
      if includeSystem then List(ColorMode.Mode.Light, ColorMode.Mode.Dark, ColorMode.Mode.System)
      else List(ColorMode.Mode.Light, ColorMode.Mode.Dark)

    def iconFor(mode: ColorMode.Mode): Icon =
      mode match {
        case ColorMode.Mode.Light  => Icon.sun
        case ColorMode.Mode.Dark   => Icon.moon
        case ColorMode.Mode.System => Icon.monitor
      }

    /** The stored value, clamped into `options` (e.g. `System` stored but excluded => first). */
    def effectiveCurrent(options: List[ColorMode.Mode], stored: ColorMode.Mode): ColorMode.Mode =
      if options.contains(stored) then stored else options.headOption.getOrElse(stored)

    /** Map a `KeyboardEvent.key` to a navigation intent, if any. */
    def keyToNav(key: String): Option[Nav] =
      key match {
        case "ArrowLeft" | "Left" | "ArrowUp" | "Up"       => Some(Nav.Prev)
        case "ArrowRight" | "Right" | "ArrowDown" | "Down" => Some(Nav.Next)
        case "Home"                                        => Some(Nav.First)
        case "End"                                         => Some(Nav.Last)
        case " " | "Spacebar" | "Enter"                    => Some(Nav.Select)
        case _                                             => None
      }

    /** Resolve a navigation intent to the target mode (Prev/Next wrap around). */
    def resolveNav(options: List[ColorMode.Mode], current: ColorMode.Mode, nav: Nav): ColorMode.Mode = {
      val n = options.size
      if n == 0 then current
      else {
        val idx = math.max(options.indexOf(current), 0)
        nav match {
          case Nav.Prev   => options((idx - 1 + n) % n)
          case Nav.Next   => options((idx + 1) % n)
          case Nav.First  => options.head
          case Nav.Last   => options.last
          case Nav.Select => options(idx)
        }
      }
    }

    /** Next mode in cycle order (wraps) — used by the compact button. */
    def cycle(options: List[ColorMode.Mode], current: ColorMode.Mode): ColorMode.Mode =
      resolveNav(options, current, Nav.Next)

  }

}
