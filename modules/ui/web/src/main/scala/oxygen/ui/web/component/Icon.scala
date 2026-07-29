package oxygen.ui.web.component

import oxygen.ui.web.*
import oxygen.ui.web.create.*

/**
  * W9: typed stroke icons (24×24 viewBox, `currentColor`).
  *
  * Paths: Lucide-compatible MIT geometry (see `agent-docs/ui-rework/specs/icon-source-strategy.md`).
  * Escape hatch: [[Icon.custom]] / [[Icon.paths]].
  */
final case class Icon(
    name: String,
    private val pathDs: Seq[String],
    sizePx: Int,
    private val strokeWidth: Double,
    private val decorative: Boolean,
    private val label: Option[String],
) extends PWidget.Deferred.Stateless[Any, Nothing] {

  def size(px: Int): Icon = copy(sizePx = px.max(8))
  def xs: Icon = size(12)
  def sm: Icon = size(16)
  def md: Icon = size(20)
  def lg: Icon = size(24)
  def xl: Icon = size(32)

  def stroke(w: Double): Icon = copy(strokeWidth = w)
  def ariaLabel(l: String): Icon = copy(decorative = false, label = Some(l))
  def decorativeIcon: Icon = copy(decorative = true, label = None)

  def pathCount: Int = pathDs.size

  override protected def build: PWidget[Any, Nothing, Any, Nothing] = Icon.render(this)
}

object Icon {

  /** Pixel sizes used by chrome (buttons, nav). */
  object Size {
    val xs: Int = 12
    val sm: Int = 16
    val md: Int = 20
    val lg: Int = 24
    val xl: Int = 32
  }

  private def stroke(name: String, ds: String*): Icon =
    Icon(name, ds.toSeq, Size.lg, 2.0, decorative = true, label = None)

  def render(icon: Icon): Node = {
    val paths = Widget.foreach(icon.pathDs) { d =>
      svgPath(svgD := d)
    }
    // Wrapper (not only the <svg>): `user-select:none` on SVG alone does not stop
    // double-click from selecting sibling labels (checkbox text, icon catalog names, etc.).
    // Multi-click mousedown preventDefault blocks the browser word-selection gesture.
    span(
      display.inlineFlex,
      alignItems.center,
      justifyContent.center,
      flexShrink := "0",
      verticalAlign.middle,
      userSelect.none,
      onMouseDown.e.handle { e =>
        if e.detail > 1 then e.preventDefault()
        zio.ZIO.unit
      },
      svg(
        htmlWidth := icon.sizePx,
        htmlHeight := icon.sizePx,
        svgViewBox := "0 0 24 24",
        svgFill.none,
        svgStroke.currentColor,
        svgStrokeWidth := math.max(1, icon.strokeWidth.round.toInt),
        svgStrokeLineCap.round,
        svgStrokeLineJoin.round,
        Widget.raw.htmlAttr("aria-hidden", if icon.decorative then "true" else "false"),
        Widget.raw.htmlAttr("data-icon", icon.name),
        icon.label.map(l => Widget.raw.htmlAttr("aria-label", l)).getOrElse(Widget.empty),
        display.block,
        userSelect.none,
        paths,
      ),
    )
  }

  /** Escape hatch: one or more path `d` strings. */
  def paths(name: String, ds: String*): Icon = stroke(name, ds*)

  /** Escape hatch: raw path data (single path). */
  def custom(d: String): Icon = stroke("custom", d)

  def custom(name: String, d: String): Icon = stroke(name, d)

  /////// Catalog (generic UI only — no sports/betting) ///////////////////////////////////////////////////////////////

  val x: Icon = stroke("x", "M18 6 6 18", "M6 6l12 12")
  val menu: Icon = stroke("menu", "M4 5h16", "M4 12h16", "M4 19h16")
  val home: Icon = stroke("home", "M3 10.5 12 3l9 7.5", "M5 10v10h14V10")
  val search: Icon = stroke("search", "M11 19a8 8 0 1 0 0-16 8 8 0 0 0 0 16z", "M21 21l-4.3-4.3")
  val settings: Icon = stroke(
    "settings",
    "M12 15.5a3.5 3.5 0 1 0 0-7 3.5 3.5 0 0 0 0 7z",
    "M19.4 15a1.7 1.7 0 0 0 .3 1.8l.1.1a2 2 0 1 1-2.8 2.8l-.1-.1a1.7 1.7 0 0 0-1.8-.3 1.7 1.7 0 0 0-1 1.5V21a2 2 0 1 1-4 0v-.1a1.7 1.7 0 0 0-1-1.5 1.7 1.7 0 0 0-1.8.3l-.1.1a2 2 0 1 1-2.8-2.8l.1-.1a1.7 1.7 0 0 0 .3-1.8 1.7 1.7 0 0 0-1.5-1H3a2 2 0 1 1 0-4h.1a1.7 1.7 0 0 0 1.5-1 1.7 1.7 0 0 0-.3-1.8l-.1-.1a2 2 0 1 1 2.8-2.8l.1.1a1.7 1.7 0 0 0 1.8.3H9a1.7 1.7 0 0 0 1-1.5V3a2 2 0 1 1 4 0v.1a1.7 1.7 0 0 0 1 1.5 1.7 1.7 0 0 0 1.8-.3l.1-.1a2 2 0 1 1 2.8 2.8l-.1.1a1.7 1.7 0 0 0-.3 1.8V9c.3.6.9 1 1.5 1H21a2 2 0 1 1 0 4h-.1a1.7 1.7 0 0 0-1.5 1z",
  )
  val gear: Icon = settings
  val user: Icon = stroke("user", "M20 21a8 8 0 0 0-16 0", "M12 13a4 4 0 1 0 0-8 4 4 0 0 0 0 8z")
  val users: Icon = stroke("users", "M16 21v-2a4 4 0 0 0-4-4H6a4 4 0 0 0-4 4v2", "M9 11a4 4 0 1 0 0-8 4 4 0 0 0 0 8z", "M22 21v-2a4 4 0 0 0-3-3.87", "M16 3.13a4 4 0 0 1 0 7.75")
  val plus: Icon = stroke("plus", "M12 5v14", "M5 12h14")
  val minus: Icon = stroke("minus", "M5 12h14")
  val check: Icon = stroke("check", "M20 6 9 17l-5-5")
  val checkCircle: Icon = stroke("check-circle", "M22 11.08V12a10 10 0 1 1-5.93-9.14", "M22 4 12 14.01l-3-3")
  val info: Icon = stroke("info", "M12 22a10 10 0 1 0 0-20 10 10 0 0 0 0 20z", "M12 16v-4", "M12 8h.01")
  val warning: Icon = stroke("warning", "M10.29 3.86 1.82 18a2 2 0 0 0 1.71 3h16.94a2 2 0 0 0 1.71-3L13.71 3.86a2 2 0 0 0-3.42 0z", "M12 9v4", "M12 17h.01")
  val error: Icon = stroke("error", "M12 22a10 10 0 1 0 0-20 10 10 0 0 0 0 20z", "M15 9l-6 6", "M9 9l6 6")
  val ban: Icon = stroke("ban", "M12 22a10 10 0 1 0 0-20 10 10 0 0 0 0 20z", "M4.93 4.93l14.14 14.14")
  val trash: Icon = stroke("trash", "M3 6h18", "M8 6V4h8v2", "M19 6v14a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V6")
  val edit: Icon = stroke("edit", "M12 20h9", "M16.5 3.5a2.1 2.1 0 0 1 3 3L7 19l-4 1 1-4 12.5-12.5z")
  val pencil: Icon = edit
  val copy: Icon = stroke("copy", "M9 9h11v11H9z", "M5 15H4a1 1 0 0 1-1-1V4a1 1 0 0 1 1-1h10a1 1 0 0 1 1 1v1")
  val clipboard: Icon = stroke(
    "clipboard",
    "M9 5H7a2 2 0 0 0-2 2v12a2 2 0 0 0 2 2h10a2 2 0 0 0 2-2V7a2 2 0 0 0-2-2h-2",
    "M9 5a2 2 0 0 0 2 2h2a2 2 0 0 0 2-2",
    "M9 5a2 2 0 0 1 2-2h2a2 2 0 0 1 2 2",
  )
  val download: Icon = stroke("download", "M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4", "M7 10l5 5 5-5", "M12 15V3")
  val upload: Icon = stroke("upload", "M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4", "M17 8l-5-5-5 5", "M12 3v12")
  val link: Icon = stroke("link", "M10 13a5 5 0 0 0 7.54.54l3-3a5 5 0 0 0-7.07-7.07l-1.72 1.71", "M14 11a5 5 0 0 0-7.54-.54l-3 3a5 5 0 0 0 7.07 7.07l1.71-1.71")
  val externalLink: Icon = stroke("external-link", "M18 13v6a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V8a2 2 0 0 1 2-2h6", "M15 3h6v6", "M10 14 21 3")
  val launch: Icon = externalLink
  val eye: Icon = stroke("eye", "M1 12s4-8 11-8 11 8 11 8-4 8-11 8S1 12 1 12z", "M12 15a3 3 0 1 0 0-6 3 3 0 0 0 0 6z")
  val eyeOff: Icon = stroke(
    "eye-off",
    "M17.94 17.94A10.07 10.07 0 0 1 12 20c-7 0-11-8-11-8a18.45 18.45 0 0 1 5.06-5.94",
    "M9.9 4.24A9.12 9.12 0 0 1 12 4c7 0 11 8 11 8a18.5 18.5 0 0 1-2.16 3.19",
    "M1 1l22 22",
    "M14.12 14.12a3 3 0 1 1-4.24-4.24",
  )
  val show: Icon = eye
  val hide: Icon = eyeOff
  val lock: Icon = stroke("lock", "M19 11H5a2 2 0 0 0-2 2v7a2 2 0 0 0 2 2h14a2 2 0 0 0 2-2v-7a2 2 0 0 0-2-2z", "M7 11V7a5 5 0 0 1 10 0v4")
  val unlock: Icon = stroke("unlock", "M19 11H5a2 2 0 0 0-2 2v7a2 2 0 0 0 2 2h14a2 2 0 0 0 2-2v-7a2 2 0 0 0-2-2z", "M7 11V7a5 5 0 0 1 9.9-1")
  val key: Icon = stroke("key", "M21 2l-2 2m-7.61 7.61a5.5 5.5 0 1 1-7.778 7.778 5.5 5.5 0 0 1 7.777-7.777zm0 0L15.5 7.5m0 0l3 3L22 7l-3-3m-3.5 3.5L19 4")
  val bell: Icon = stroke("bell", "M18 8A6 6 0 0 0 6 8c0 7-3 9-3 9h18s-3-2-3-9", "M13.73 21a2 2 0 0 1-3.46 0")
  val mail: Icon = stroke("mail", "M4 4h16c1.1 0 2 .9 2 2v12c0 1.1-.9 2-2 2H4c-1.1 0-2-.9-2-2V6c0-1.1.9-2 2-2z", "M22 6l-10 7L2 6")
  val envelope: Icon = mail
  val phone: Icon = stroke(
    "phone",
    "M22 16.92v3a2 2 0 0 1-2.18 2 19.79 19.79 0 0 1-8.63-3.07 19.5 19.5 0 0 1-6-6 19.79 19.79 0 0 1-3.07-8.67A2 2 0 0 1 4.11 2h3a2 2 0 0 1 2 1.72c.12.9.36 1.77.7 2.61a2 2 0 0 1-.45 2.11L8.09 9.91a16 16 0 0 0 6 6l1.27-1.27a2 2 0 0 1 2.11-.45c.84.34 1.71.58 2.61.7A2 2 0 0 1 22 16.92z",
  )
  val message: Icon = stroke("message", "M21 15a2 2 0 0 1-2 2H7l-4 4V5a2 2 0 0 1 2-2h14a2 2 0 0 1 2 2z")
  val chat: Icon = message
  val calendar: Icon = stroke("calendar", "M3 4h18v18H3z", "M16 2v4", "M8 2v4", "M3 10h18")
  val clock: Icon = stroke("clock", "M12 22a10 10 0 1 0 0-20 10 10 0 0 0 0 20z", "M12 6v6l4 2")
  val star: Icon = stroke("star", "M12 2l3.09 6.26L22 9.27l-5 4.87 1.18 6.88L12 17.77l-6.18 3.25L7 14.14 2 9.27l6.91-1.01L12 2z")
  val heart: Icon = stroke("heart", "M20.84 4.61a5.5 5.5 0 0 0-7.78 0L12 5.67l-1.06-1.06a5.5 5.5 0 0 0-7.78 7.78l1.06 1.06L12 21.23l7.78-7.78 1.06-1.06a5.5 5.5 0 0 0 0-7.78z")
  val bookmark: Icon = stroke("bookmark", "M19 21l-7-5-7 5V5a2 2 0 0 1 2-2h10a2 2 0 0 1 2 2z")
  val flag: Icon = stroke("flag", "M4 15s1-1 4-1 5 2 8 2 4-1 4-1V3s-1 1-4 1-5-2-8-2-4 1-4 1z", "M4 22v-7")
  val pin: Icon = stroke(
    "pin",
    "M12 17v5",
    "M9 10.76a2 2 0 0 1-1.11 1.79l-1.78.9A2 2 0 0 0 5 15.24V16h14v-.76a2 2 0 0 0-1.11-1.79l-1.78-.9A2 2 0 0 1 15 10.76V7a1 1 0 0 1 1-1 2 2 0 0 0 0-4H8a2 2 0 0 0 0 4 1 1 0 0 1 1 1z",
  )
  val filter: Icon = stroke("filter", "M22 3H2l8 9.46V19l4 2v-8.54L22 3z")
  val funnel: Icon = filter
  val grid: Icon = stroke("grid", "M3 3h7v7H3z", "M14 3h7v7h-7z", "M14 14h7v7h-7z", "M3 14h7v7H3z")
  val list: Icon = stroke("list", "M8 6h13", "M8 12h13", "M8 18h13", "M3 6h.01", "M3 12h.01", "M3 18h.01")
  val table: Icon = stroke("table", "M3 3h18v18H3z", "M3 9h18", "M3 15h18", "M9 3v18", "M15 3v18")
  val image: Icon = stroke("image", "M3 5h18v14H3z", "M3 15l5-5 4 4 3-3 6 6", "M8.5 10a1.5 1.5 0 1 0 0-3 1.5 1.5 0 0 0 0 3z")
  val camera: Icon = stroke("camera", "M23 19a2 2 0 0 1-2 2H3a2 2 0 0 1-2-2V8a2 2 0 0 1 2-2h4l2-3h6l2 3h4a2 2 0 0 1 2 2z", "M12 17a4 4 0 1 0 0-8 4 4 0 0 0 0 8z")
  val video: Icon = stroke("video", "M23 7l-7 5 7 5V7z", "M1 7h14v10H1z")
  val play: Icon = stroke("play", "M5 3l14 9-14 9V3z")
  val pause: Icon = stroke("pause", "M6 4h4v16H6z", "M14 4h4v16h-4z")
  val stop: Icon = stroke("stop", "M6 6h12v12H6z")
  val volume: Icon = stroke("volume", "M11 5 6 9H2v6h4l5 4V5z", "M19.07 4.93a10 10 0 0 1 0 14.14", "M15.54 8.46a5 5 0 0 1 0 7.07")
  val mute: Icon = stroke("mute", "M11 5 6 9H2v6h4l5 4V5z", "M23 9l-6 6", "M17 9l6 6")
  val mic: Icon = stroke("mic", "M12 1a3 3 0 0 0-3 3v8a3 3 0 0 0 6 0V4a3 3 0 0 0-3-3z", "M19 10v2a7 7 0 0 1-14 0v-2", "M12 19v4", "M8 23h8")
  val wifi: Icon = stroke("wifi", "M5 12.55a11 11 0 0 1 14.08 0", "M1.42 9a16 16 0 0 1 21.16 0", "M8.53 16.11a6 6 0 0 1 6.95 0", "M12 20h.01")
  val wifiOff: Icon = stroke(
    "wifi-off",
    "M1 1l22 22",
    "M16.72 11.06A10.94 10.94 0 0 1 19 12.55",
    "M5 12.55a10.94 10.94 0 0 1 5.17-2.39",
    "M10.71 5.05A16 16 0 0 1 22.58 9",
    "M1.42 9a15.91 15.91 0 0 1 4.7-2.88",
    "M8.53 16.11a6 6 0 0 1 6.95 0",
    "M12 20h.01",
  )
  val bluetooth: Icon = stroke("bluetooth", "M6.5 6.5 17.5 17.5 12 23V1l5.5 5.5L6.5 17.5")
  val globe: Icon = stroke(
    "globe",
    "M12 22a10 10 0 1 0 0-20 10 10 0 0 0 0 20z",
    "M2 12h20",
    "M12 2a15.3 15.3 0 0 1 4 10 15.3 15.3 0 0 1-4 10 15.3 15.3 0 0 1-4-10 15.3 15.3 0 0 1 4-10z",
  )
  val mapPin: Icon = stroke("map-pin", "M21 10c0 7-9 13-9 13s-9-6-9-13a9 9 0 0 1 18 0z", "M12 13a3 3 0 1 0 0-6 3 3 0 0 0 0 6z")
  val gps: Icon = mapPin
  val folder: Icon = stroke("folder", "M22 19a2 2 0 0 1-2 2H4a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h5l2 3h9a2 2 0 0 1 2 2z")
  val file: Icon = stroke("file", "M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z", "M14 2v6h6")
  val document: Icon = file
  val archive: Icon = stroke("archive", "M21 8v13H3V8", "M1 3h22v5H1z", "M10 12h4")
  val save: Icon = stroke("save", "M19 21H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h11l5 5v11a2 2 0 0 1-2 2z", "M17 21v-8H7v8", "M7 3v5h8")
  val disk: Icon = save
  val printer: Icon = stroke("printer", "M6 9V2h12v7", "M6 18H4a2 2 0 0 1-2-2v-5a2 2 0 0 1 2-2h16a2 2 0 0 1 2 2v5a2 2 0 0 1-2 2h-2", "M6 14h12v8H6z")
  val share: Icon = stroke("share", "M4 12v8a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2v-8", "M16 6l-4-4-4 4", "M12 2v13")
  val logout: Icon = stroke("logout", "M9 21H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h4", "M16 17l5-5-5-5", "M21 12H9")
  val login: Icon = stroke("login", "M15 3h4a2 2 0 0 1 2 2v14a2 2 0 0 1-2 2h-4", "M10 17l5-5-5-5", "M15 12H3")
  val refresh: Icon = stroke("refresh", "M23 4v6h-6", "M1 20v-6h6", "M3.51 9a9 9 0 0 1 14.85-3.36L23 10", "M1 14l4.64 4.36A9 9 0 0 0 20.49 15")
  // Lucide undo / redo (curved arrow + corner) — previous arcs were malformed.
  val undo: Icon = stroke("undo", "M3 7v6h6", "M21 17a9 9 0 0 0-9-9 9 9 0 0 0-6 2.3L3 13")
  val redo: Icon = stroke("redo", "M21 7v6h-6", "M3 17a9 9 0 0 1 9-9 9 9 0 0 1 6 2.3l3 2.7")
  val chevronUp: Icon = stroke("chevron-up", "M18 15l-6-6-6 6")
  val chevronDown: Icon = stroke("chevron-down", "M6 9l6 6 6-6")
  val chevronLeft: Icon = stroke("chevron-left", "M15 18l-6-6 6-6")
  val chevronRight: Icon = stroke("chevron-right", "M9 18l6-6-6-6")
  val chevronsLeft: Icon = stroke("chevrons-left", "M11 17l-5-5 5-5", "M18 17l-5-5 5-5")
  val chevronsRight: Icon = stroke("chevrons-right", "M13 17l5-5-5-5", "M6 17l5-5-5-5")
  val arrowUp: Icon = stroke("arrow-up", "M12 19V5", "M5 12l7-7 7 7")
  val arrowDown: Icon = stroke("arrow-down", "M12 5v14", "M19 12l-7 7-7-7")
  val arrowLeft: Icon = stroke("arrow-left", "M19 12H5", "M12 19l-7-7 7-7")
  val arrowRight: Icon = stroke("arrow-right", "M5 12h14", "M12 5l7 7-7 7")
  val moreHorizontal: Icon = stroke("more-horizontal", "M12 13a1 1 0 1 0 0-2 1 1 0 0 0 0 2z", "M19 13a1 1 0 1 0 0-2 1 1 0 0 0 0 2z", "M5 13a1 1 0 1 0 0-2 1 1 0 0 0 0 2z")
  val moreVertical: Icon = stroke("more-vertical", "M12 13a1 1 0 1 0 0-2 1 1 0 0 0 0 2z", "M12 6a1 1 0 1 0 0-2 1 1 0 0 0 0 2z", "M12 20a1 1 0 1 0 0-2 1 1 0 0 0 0 2z")
  val dots: Icon = moreHorizontal
  val drag: Icon = stroke("drag", "M9 5h.01", "M9 12h.01", "M9 19h.01", "M15 5h.01", "M15 12h.01", "M15 19h.01")
  val sortAsc: Icon = stroke("sort-asc", "M11 5h10", "M11 9h7", "M11 13h4", "M3 17l4 4 4-4", "M7 3v18")
  val sortDesc: Icon = stroke("sort-desc", "M11 5h4", "M11 9h7", "M11 13h10", "M3 7l4-4 4 4", "M7 3v18")
  val trendUp: Icon = stroke("trend-up", "M23 6l-9.5 9.5-5-5L1 18", "M17 6h6v6")
  val trendDown: Icon = stroke("trend-down", "M23 18l-9.5-9.5-5 5L1 6", "M17 18h6v-6")
  val activity: Icon = stroke("activity", "M22 12h-4l-3 9L9 3l-3 9H2")
  val pulse: Icon = activity
  val zap: Icon = stroke("zap", "M13 2 3 14h9l-1 8 10-12h-9l1-8z")
  val lightning: Icon = zap
  val sun: Icon = stroke(
    "sun",
    "M12 1v2",
    "M12 21v2",
    "M4.22 4.22l1.42 1.42",
    "M18.36 18.36l1.42 1.42",
    "M1 12h2",
    "M21 12h2",
    "M4.22 19.78l1.42-1.42",
    "M18.36 5.64l1.42-1.42",
    "M12 17a5 5 0 1 0 0-10 5 5 0 0 0 0 10z",
  )
  val moon: Icon = stroke("moon", "M21 12.79A9 9 0 1 1 11.21 3 7 7 0 0 0 21 12.79z")
  val bulb: Icon = stroke("bulb", "M9 18h6", "M10 22h4", "M12 2a7 7 0 0 0-4 12.7V17h8v-2.3A7 7 0 0 0 12 2z")
  val gift: Icon = stroke("gift", "M20 12v10H4V12", "M2 7h20v5H2z", "M12 22V7", "M12 7H7.5a2.5 2.5 0 1 1 0-5C11 2 12 7 12 7z", "M12 7h4.5a2.5 2.5 0 1 0 0-5C13 2 12 7 12 7z")
  val tag: Icon = stroke("tag", "M20.59 13.41l-7.17 7.17a2 2 0 0 1-2.83 0L2 12V2h10l8.59 8.59a2 2 0 0 1 0 2.82z", "M7 7h.01")
  // Lucide tags — dual tag shapes + hole (stroke-friendly hole as tiny segment).
  val tags: Icon = stroke(
    "tags",
    "M13.172 2a2 2 0 0 1 1.414.586l6.71 6.71a2.4 2.4 0 0 1 0 3.408l-4.592 4.592a2.4 2.4 0 0 1-3.408 0l-6.71-6.71A2 2 0 0 1 6 9.172V3a1 1 0 0 1 1-1z",
    "M2 7v6.172a2 2 0 0 0 .586 1.414l6.71 6.71a2.4 2.4 0 0 0 3.191.193",
    "M10.5 6.5h.01",
  )
  val creditCard: Icon = stroke("credit-card", "M21 4H3a2 2 0 0 0-2 2v12a2 2 0 0 0 2 2h18a2 2 0 0 0 2-2V6a2 2 0 0 0-2-2z", "M1 10h22")
  val wallet: Icon = stroke("wallet", "M21 12V7H5a2 2 0 0 1 0-4h14v4", "M3 5v14a2 2 0 0 0 2 2h16v-5", "M18 12a2 2 0 0 0 0 4h4v-4h-4z")
  val dollar: Icon = stroke("dollar", "M12 1v22", "M17 5H9.5a3.5 3.5 0 0 0 0 7h5a3.5 3.5 0 0 1 0 7H6")
  val bank: Icon = stroke("bank", "M3 21h18", "M3 10h18", "M5 6l7-3 7 3", "M4 10v11", "M20 10v11", "M8 14v3", "M12 14v3", "M16 14v3")
  val receipt: Icon = stroke("receipt", "M4 2v20l3-2 3 2 3-2 3 2 3-2 3 2V2l-3 2-3-2-3 2-3-2-3 2-3-2z", "M8 10h8", "M8 14h8", "M8 6h8")
  val cart: Icon = stroke("cart", "M6 6h15l-1.5 9h-12z", "M6 6 5 3H2", "M9 20a1 1 0 1 0 0-2 1 1 0 0 0 0 2z", "M18 20a1 1 0 1 0 0-2 1 1 0 0 0 0 2z")
  val bed: Icon = stroke("bed", "M2 4v16", "M2 8h18a2 2 0 0 1 2 2v10", "M2 17h20", "M6 8v9")
  val hotel: Icon = bed
  val question: Icon = stroke("question", "M12 22a10 10 0 1 0 0-20 10 10 0 0 0 0 20z", "M9.09 9a3 3 0 0 1 5.83 1c0 2-3 3-3 3", "M12 17h.01")
  val help: Icon = question
  val accessibility: Icon = stroke("accessibility", "M12 5a1 1 0 1 0 0-2 1 1 0 0 0 0 2z", "M6 9h12", "M12 9v4", "M9 21l3-8 3 8", "M7 13l-2 2", "M17 13l2 2")
  val thumbUp: Icon = stroke("thumb-up", "M14 9V5a3 3 0 0 0-3-3l-4 9v11h11.28a2 2 0 0 0 2-1.7l1.38-9a2 2 0 0 0-2-2.3H14z", "M7 22H4a2 2 0 0 1-2-2v-7a2 2 0 0 1 2-2h3")
  val thumbDown: Icon = stroke(
    "thumb-down",
    "M10 15v4a3 3 0 0 0 3 3l4-9V2H5.72a2 2 0 0 0-2 1.7l-1.38 9a2 2 0 0 0 2 2.3H10z",
    "M17 2h2.67A2.31 2.31 0 0 1 22 4v7a2.31 2.31 0 0 1-2.33 2H17",
  )
  val alignLeft: Icon = stroke("align-left", "M17 10H3", "M21 6H3", "M21 14H3", "M17 18H3")
  val alignCenter: Icon = stroke("align-center", "M18 10H6", "M21 6H3", "M21 14H3", "M18 18H6")
  val alignRight: Icon = stroke("align-right", "M21 10H7", "M21 6H3", "M21 14H3", "M21 18H7")
  val bold: Icon = stroke("bold", "M6 4h8a4 4 0 0 1 4 4 4 4 0 0 1-4 4H6z", "M6 12h9a4 4 0 0 1 4 4 4 4 0 0 1-4 4H6z")
  val italic: Icon = stroke("italic", "M19 4h-9", "M14 20H5", "M15 4 9 20")
  val code: Icon = stroke("code", "M16 18l6-6-6-6", "M8 6l-6 6 6 6")
  val terminal: Icon = stroke("terminal", "M4 17l6-6-6-6", "M12 19h8")
  val database: Icon = stroke(
    "database",
    "M12 2C6.48 2 2 3.79 2 6s4.48 4 10 4 10-1.79 10-4-4.48-4-10-4z",
    "M2 6v6c0 2.21 4.48 4 10 4s10-1.79 10-4V6",
    "M2 12v6c0 2.21 4.48 4 10 4s10-1.79 10-4v-6",
  )
  val server: Icon = stroke("server", "M2 4h20v6H2z", "M2 14h20v6H2z", "M6 7h.01", "M6 17h.01")
  val cloud: Icon = stroke("cloud", "M18 10h-1.26A8 8 0 1 0 9 20h9a5 5 0 0 0 0-10z")
  val inbox: Icon = stroke("inbox", "M22 12h-6l-2 3h-4l-2-3H2", "M5.45 5.11 2 12v6a2 2 0 0 0 2 2h16a2 2 0 0 0 2-2v-6l-3.45-6.89A2 2 0 0 0 16.76 4H7.24a2 2 0 0 0-1.79 1.11z")
  val paperclip: Icon = stroke("paperclip", "M21.44 11.05l-9.19 9.19a6 6 0 0 1-8.49-8.49l9.19-9.19a4 4 0 0 1 5.66 5.66l-9.2 9.19a2 2 0 0 1-2.83-2.83l8.49-8.48")
  val scissors: Icon = stroke("scissors", "M6 9a3 3 0 1 0 0-6 3 3 0 0 0 0 6z", "M6 21a3 3 0 1 0 0-6 3 3 0 0 0 0 6z", "M20 4 8.12 15.88", "M14.47 14.48 20 20", "M8.12 8.12 12 12")
  val history: Icon = stroke("history", "M3 3v5h5", "M3.05 13A9 9 0 1 0 6 5.3L3 8", "M12 7v5l4 2")
  val timeline: Icon = stroke("timeline", "M12 2v20", "M17 5H9.5a3.5 3.5 0 0 0 0 7h5a3.5 3.5 0 0 1 0 7H6")
  val layers: Icon = stroke("layers", "M12 2 2 7l10 5 10-5-10-5z", "M2 17l10 5 10-5", "M2 12l10 5 10-5")
  val layout: Icon = stroke("layout", "M3 3h18v18H3z", "M3 9h18", "M9 21V9")
  val sidebar: Icon = stroke("sidebar", "M3 3h18v18H3z", "M9 3v18")
  val maximize: Icon = stroke("maximize", "M8 3H5a2 2 0 0 0-2 2v3", "M21 8V5a2 2 0 0 0-2-2h-3", "M3 16v3a2 2 0 0 0 2 2h3", "M16 21h3a2 2 0 0 0 2-2v-3")
  val minimize: Icon = stroke("minimize", "M8 3v3a2 2 0 0 1-2 2H3", "M21 8h-3a2 2 0 0 1-2-2V3", "M3 16h3a2 2 0 0 1 2 2v3", "M16 21v-3a2 2 0 0 1 2-2h3")
  val expand: Icon = maximize
  val collapse: Icon = minimize
  val loader: Icon = stroke("loader", "M12 2v4", "M12 18v4", "M4.93 4.93l2.83 2.83", "M16.24 16.24l2.83 2.83", "M2 12h4", "M18 12h4", "M4.93 19.07l2.83-2.83", "M16.24 7.76l2.83-2.83")
  val spinner: Icon = loader
  val circle: Icon = stroke("circle", "M12 22a10 10 0 1 0 0-20 10 10 0 0 0 0 20z")
  val square: Icon = stroke("square", "M3 3h18v18H3z")
  val checkbox: Icon = stroke("checkbox", "M9 11l3 3L22 4", "M21 12v7a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h11")
  val checkboxEmpty: Icon = stroke("checkbox-empty", "M5 3h14a2 2 0 0 1 2 2v14a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2z")
  val radioOn: Icon = stroke("radio-on", "M12 22a10 10 0 1 0 0-20 10 10 0 0 0 0 20z", "M12 16a4 4 0 1 0 0-8 4 4 0 0 0 0 8z")
  val radioOff: Icon = circle
  val id: Icon = stroke("id", "M4 4h16v16H4z", "M8 10h.01", "M12 10h4", "M8 14h8")
  val shield: Icon = stroke("shield", "M12 22s8-4 8-10V5l-8-3-8 3v7c0 6 8 10 8 10z")
  val alertCircle: Icon = stroke("alert-circle", "M12 22a10 10 0 1 0 0-20 10 10 0 0 0 0 20z", "M12 8v4", "M12 16h.01")
  val power: Icon = stroke("power", "M18.36 6.64a9 9 0 1 1-12.73 0", "M12 2v10")
  val battery: Icon = stroke("battery", "M1 6h18v12H1z", "M23 10v4", "M6 10v4")
  val watch: Icon = stroke("watch", "M12 22a7 7 0 1 0 0-14 7 7 0 0 0 0 14z", "M12 9v4l2 1", "M9 2h6", "M9 22h6")
  val alarm: Icon = stroke("alarm", "M12 22a8 8 0 1 0 0-16 8 8 0 0 0 0 16z", "M12 10v4l2 1", "M5 3 2 6", "M22 6l-3-3")
  val leaf: Icon = stroke("leaf", "M11 20A7 7 0 0 1 9.8 6.1C15.5 5 17 4.48 19 2c1 2 2 4.18 2 8 0 5.5-4.78 10-10 10z", "M2 21c0-3 1.85-5.36 5.08-6C9.5 14.52 12 13 13 12")
  val plant: Icon = leaf
  val broom: Icon = stroke("broom", "M3 21h4", "M5 21V10", "M9 6l5 5", "M14 3l7 7-8.5 8.5a3 3 0 0 1-4.24 0L5.5 15.26a3 3 0 0 1 0-4.24L14 3z")
  val tshirt: Icon = stroke(
    "tshirt",
    "M20.38 3.46 16 2a4 4 0 0 1-8 0L3.62 3.46a2 2 0 0 0-1.34 2.23l.58 3.47a1 1 0 0 0 .99.84H6v10c0 1.1.9 2 2 2h8a2 2 0 0 0 2-2V10h2.15a1 1 0 0 0 .99-.84l.58-3.47a2 2 0 0 0-1.34-2.23z",
  )
  // Lucide party-popper — full path set (prior version dropped half the strokes).
  val party: Icon = stroke(
    "party",
    "M5.8 11.3 2 22l10.7-3.79",
    "M4 3h.01",
    "M22 8h.01",
    "M15 2h.01",
    "M22 20h.01",
    "m22 2-2.24.75a2.9 2.9 0 0 0-1.96 3.12c.1.86-.57 1.63-1.45 1.63h-.38c-.86 0-1.6.6-1.76 1.44L14 10",
    "m22 13-.82-.33c-.86-.34-1.82.2-1.98 1.11c-.11.7-.72 1.22-1.43 1.22H17",
    "m11 2 .33.82c.34.86-.2 1.82-1.11 1.98C9.52 4.9 9 5.52 9 6.23V7",
    "M11 13c1.93 1.93 2.83 4.17 2 5-.83.83-3.07-.07-5-2-1.93-1.93-2.83-4.17-2-5 .83-.83 3.07.07 5 2Z",
  )
  val keyboard: Icon = stroke("keyboard", "M2 6h20v12H2z", "M6 10h.01", "M10 10h.01", "M14 10h.01", "M18 10h.01", "M6 14h.01", "M10 14h8")
  val desktop: Icon = stroke("desktop", "M2 4h20v12H2z", "M8 20h8", "M12 16v4")
  val smartphone: Icon = stroke("smartphone", "M7 2h10a2 2 0 0 1 2 2v16a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2z", "M12 18h.01")
  val tablet: Icon = stroke("tablet", "M4 2h16a2 2 0 0 1 2 2v16a2 2 0 0 1-2 2H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2z", "M12 18h.01")
  val faceId: Icon = stroke(
    "face-id",
    "M3 7V5a2 2 0 0 1 2-2h2",
    "M17 3h2a2 2 0 0 1 2 2v2",
    "M21 17v2a2 2 0 0 1-2 2h-2",
    "M7 21H5a2 2 0 0 1-2-2v-2",
    "M9 10h.01",
    "M15 10h.01",
    "M9.5 15a3.5 3.5 0 0 0 5 0",
  )
  // Fingerprint / Touch ID (stroke ridges) — prior paths closed incorrectly and looked blank/broken.
  val touchId: Icon = stroke(
    "touch-id",
    "M12 11v2a1 1 0 0 0 2 0v-1a3 3 0 0 0-6 0c0 1.5.4 3.5 1.2 5.5",
    "M16.5 12c0 2.2-.4 4.5-1.5 6.8",
    "M7.5 12c0 1.8.3 3.6 1 5.5",
    "M19 11a7 7 0 0 0-14 0",
    "M5 15.5c0 2.2.6 4.2 1.6 6",
    "M19 15.5c0 2.2-.6 4.2-1.6 6",
    "M12 7a4 4 0 0 1 4 4",
    "M12 7a4 4 0 0 0-3.5 2",
  )
  val equal: Icon = stroke("equal", "M5 9h14", "M5 15h14")
  val hash: Icon = stroke("hash", "M4 9h16", "M4 15h16", "M10 3 8 21", "M16 3l-2 18")
  val atSign: Icon = stroke("at-sign", "M12 16a4 4 0 1 0 0-8 4 4 0 0 0 0 8z", "M16 12v1a3 3 0 0 0 6 0v-1a10 10 0 1 0-3.92 7.94")
  val percent: Icon = stroke("percent", "M19 5 5 19", "M6.5 9a2.5 2.5 0 1 0 0-5 2.5 2.5 0 0 0 0 5z", "M17.5 20a2.5 2.5 0 1 0 0-5 2.5 2.5 0 0 0 0 5z")
  val slash: Icon = stroke("slash", "M12 22a10 10 0 1 0 0-20 10 10 0 0 0 0 20z", "M4.93 4.93l14.14 14.14")
  val move: Icon = stroke("move", "M5 9l-3 3 3 3", "M9 5l3-3 3 3", "M15 19l-3 3-3-3", "M19 9l3 3-3 3", "M2 12h20", "M12 2v20")
  val crosshair: Icon = stroke("crosshair", "M12 22a10 10 0 1 0 0-20 10 10 0 0 0 0 20z", "M22 12h-4", "M6 12H2", "M12 6V2", "M12 22v-4")
  val target: Icon = stroke("target", "M12 22a10 10 0 1 0 0-20 10 10 0 0 0 0 20z", "M12 18a6 6 0 1 0 0-12 6 6 0 0 0 0 12z", "M12 14a2 2 0 1 0 0-4 2 2 0 0 0 0 4z")
  val compass: Icon = stroke("compass", "M12 22a10 10 0 1 0 0-20 10 10 0 0 0 0 20z", "M16.24 7.76l-2.12 6.36-6.36 2.12 2.12-6.36 6.36-2.12z")
  val navigation: Icon = stroke("navigation", "M3 11l19-9-9 19-2-8-8-2z")
  val send: Icon = stroke("send", "M22 2 11 13", "M22 2l-7 20-4-9-9-4 20-7z")
  val reply: Icon = stroke("reply", "M9 17 4 12l5-5", "M20 18v-2a4 4 0 0 0-4-4H4")
  val forward: Icon = stroke("forward", "M15 17l5-5-5-5", "M4 18v-2a4 4 0 0 1 4-4h12")
  val inboxIn: Icon = stroke("inbox-in", "M22 12h-6l-2 3h-4l-2-3H2", "M5.45 5.11 2 12v6a2 2 0 0 0 2 2h16a2 2 0 0 0 2-2v-6l-3.45-6.89A2 2 0 0 0 16.76 4H7.24a2 2 0 0 0-1.79 1.11z")
  val logIn: Icon = login
  val logOut: Icon = logout
  val userPlus: Icon = stroke("user-plus", "M16 21v-2a4 4 0 0 0-4-4H5a4 4 0 0 0-4 4v2", "M8.5 11a4 4 0 1 0 0-8 4 4 0 0 0 0 8z", "M20 8v6", "M23 11h-6")
  val userMinus: Icon = stroke("user-minus", "M16 21v-2a4 4 0 0 0-4-4H5a4 4 0 0 0-4 4v2", "M8.5 11a4 4 0 1 0 0-8 4 4 0 0 0 0 8z", "M23 11h-6")
  val userCheck: Icon = stroke("user-check", "M16 21v-2a4 4 0 0 0-4-4H5a4 4 0 0 0-4 4v2", "M8.5 11a4 4 0 1 0 0-8 4 4 0 0 0 0 8z", "M17 11l2 2 4-4")
  val userX: Icon = stroke("user-x", "M16 21v-2a4 4 0 0 0-4-4H5a4 4 0 0 0-4 4v2", "M8.5 11a4 4 0 1 0 0-8 4 4 0 0 0 0 8z", "M18 8l5 5", "M23 8l-5 5")
  val userSettings: Icon = stroke("user-settings", "M16 21v-2a4 4 0 0 0-4-4H5a4 4 0 0 0-4 4v2", "M8.5 11a4 4 0 1 0 0-8 4 4 0 0 0 0 8z", "M19 8l2 2", "M19 8l-2 2")
  val shoppingBag: Icon = stroke("shopping-bag", "M6 2 3 6v14a2 2 0 0 0 2 2h14a2 2 0 0 0 2-2V6l-3-4z", "M3 6h18", "M16 10a4 4 0 0 1-8 0")
  val packageIcon: Icon = stroke(
    "package",
    "M16.5 9.4 7.55 4.24",
    "M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z",
    "M3.27 6.96 12 12.01l8.73-5.05",
    "M12 22.08V12",
  )
  val truck: Icon = stroke("truck", "M1 3h15v13H1z", "M16 8h4l3 3v5h-7V8z", "M5.5 21a2.5 2.5 0 1 0 0-5 2.5 2.5 0 0 0 0 5z", "M18.5 21a2.5 2.5 0 1 0 0-5 2.5 2.5 0 0 0 0 5z")
  val map: Icon = stroke("map", "M1 6v16l7-4 8 4 7-4V2l-7 4-8-4-7 4z", "M8 2v16", "M16 6v16")
  val book: Icon = stroke("book", "M4 19.5A2.5 2.5 0 0 1 6.5 17H20", "M6.5 2H20v20H6.5A2.5 2.5 0 0 1 4 19.5v-15A2.5 2.5 0 0 1 6.5 2z")
  val bookOpen: Icon = stroke("book-open", "M2 3h6a4 4 0 0 1 4 4v14a3 3 0 0 0-3-3H2z", "M22 3h-6a4 4 0 0 0-4 4v14a3 3 0 0 1 3-3h7z")
  val newspaper: Icon = stroke(
    "newspaper",
    "M4 22h16a2 2 0 0 0 2-2V4a2 2 0 0 0-2-2H8a2 2 0 0 0-2 2v16a2 2 0 0 1-2 2zm0 0a2 2 0 0 1-2-2v-9c0-1.1.9-2 2-2h2",
    "M18 14h-8",
    "M15 18h-5",
    "M10 6h8v4h-8V6z",
  )
  val coffee: Icon = stroke("coffee", "M18 8h1a4 4 0 0 1 0 8h-1", "M2 8h16v9a4 4 0 0 1-4 4H6a4 4 0 0 1-4-4V8z", "M6 1v3", "M10 1v3", "M14 1v3")
  val music: Icon = stroke("music", "M9 18V5l12-2v13", "M6 21a3 3 0 1 0 0-6 3 3 0 0 0 0 6z", "M18 19a3 3 0 1 0 0-6 3 3 0 0 0 0 6z")
  val film: Icon = stroke("film", "M7 2v20", "M17 2v20", "M2 12h20", "M2 7h5", "M2 17h5", "M17 17h5", "M17 7h5")
  val headphones: Icon = stroke(
    "headphones",
    "M3 18v-6a9 9 0 0 1 18 0v6",
    "M21 19a2 2 0 0 1-2 2h-1a2 2 0 0 1-2-2v-3a2 2 0 0 1 2-2h3z",
    "M3 19a2 2 0 0 0 2 2h1a2 2 0 0 0 2-2v-3a2 2 0 0 0-2-2H3z",
  )
  val award: Icon = stroke("award", "M12 15a7 7 0 1 0 0-14 7 7 0 0 0 0 14z", "M8.21 13.89 7 23l5-3 5 3-1.21-9.12")
  val giftCard: Icon = gift
  val percentage: Icon = percent
  val infinity: Icon = stroke("infinity", "M18.178 8c5.096 0 5.096 8 0 8-5.095 0-7.133-8-12.739-8-4.585 0-4.585 8 0 8 5.606 0 7.644-8 12.74-8z")
  val wifiFull: Icon = wifi
  val signal: Icon = stroke("signal", "M2 20h.01", "M7 20v-4", "M12 20v-8", "M17 20V8", "M22 20V4")
  val cast: Icon = stroke("cast", "M2 16.1A5 5 0 0 1 5.9 20", "M2 12.05A9 9 0 0 1 9.95 20", "M2 8V6a2 2 0 0 1 2-2h16a2 2 0 0 1 2 2v12a2 2 0 0 1-2 2h-6", "M2 20h.01")
  val airplay: Icon = stroke("airplay", "M5 17H4a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h16a2 2 0 0 1 2 2v10a2 2 0 0 1-2 2h-1", "M12 15l5 6H7l5-6z")
  val monitor: Icon = desktop
  val hardDrive: Icon = stroke(
    "hard-drive",
    "M22 12H2",
    "M5.45 5.11 2 12v6a2 2 0 0 0 2 2h16a2 2 0 0 0 2-2v-6l-3.45-6.89A2 2 0 0 0 16.76 4H7.24a2 2 0 0 0-1.79 1.11z",
    "M6 16h.01",
    "M10 16h.01",
  )
  val cpu: Icon = stroke("cpu", "M6 6h12v12H6z", "M9 9h6v6H9z", "M9 1v3", "M15 1v3", "M9 20v3", "M15 20v3", "M20 9h3", "M20 14h3", "M1 9h3", "M1 14h3")
  val tool: Icon = stroke(
    "tool",
    "M14.7 6.3a1 1 0 0 0 0 1.4l1.6 1.6a1 1 0 0 0 1.4 0l3.77-3.77a6 6 0 0 1-7.94 7.94l-6.91 6.91a2.12 2.12 0 0 1-3-3l6.91-6.91a6 6 0 0 1 7.94-7.94l-3.76 3.76z",
  )
  val wrench: Icon = tool
  val hammer: Icon = stroke(
    "hammer",
    "M15 12l-8.5 8.5c-.83.83-2.17.83-3 0 0 0 0 0 0 0a2.12 2.12 0 0 1 0-3L12 9",
    "M17.64 15 22 10.64",
    "M20.91 11.7l-1.25-1.25c-.6-.6-.93-1.4-.93-2.25v-.86L16.01 4.6a5.56 5.56 0 0 0-3.94-1.64H9l.92.82A6.18 6.18 0 0 1 12 8.4v1.56l2 2h2.47l2.26 1.91",
  )
  val lifeBuoy: Icon = stroke(
    "life-buoy",
    "M12 22a10 10 0 1 0 0-20 10 10 0 0 0 0 20z",
    "M12 16a4 4 0 1 0 0-8 4 4 0 0 0 0 8z",
    "M4.93 4.93l4.24 4.24",
    "M14.83 14.83l4.24 4.24",
    "M14.83 9.17l4.24-4.24",
    "M9.17 14.83l-4.24 4.24",
  )
  val anchor: Icon = stroke("anchor", "M12 22V8", "M5 12H2a10 10 0 0 0 20 0h-3", "M12 8a3 3 0 1 0 0-6 3 3 0 0 0 0 6z")

  /** All shipped built-in icons for gallery / search (W9-T03/T04). */
  val all: Seq[Icon] = Seq(
    x,
    menu,
    home,
    search,
    settings,
    user,
    users,
    plus,
    minus,
    check,
    checkCircle,
    info,
    warning,
    error,
    ban,
    trash,
    edit,
    copy,
    clipboard,
    download,
    upload,
    link,
    externalLink,
    eye,
    eyeOff,
    lock,
    unlock,
    key,
    bell,
    mail,
    phone,
    message,
    calendar,
    clock,
    star,
    heart,
    bookmark,
    flag,
    pin,
    filter,
    grid,
    list,
    table,
    image,
    camera,
    video,
    play,
    pause,
    stop,
    volume,
    mute,
    mic,
    wifi,
    wifiOff,
    bluetooth,
    globe,
    mapPin,
    folder,
    file,
    archive,
    save,
    printer,
    share,
    logout,
    login,
    refresh,
    undo,
    redo,
    chevronUp,
    chevronDown,
    chevronLeft,
    chevronRight,
    chevronsLeft,
    chevronsRight,
    arrowUp,
    arrowDown,
    arrowLeft,
    arrowRight,
    moreHorizontal,
    moreVertical,
    drag,
    sortAsc,
    sortDesc,
    trendUp,
    trendDown,
    activity,
    zap,
    sun,
    moon,
    bulb,
    gift,
    tag,
    tags,
    creditCard,
    wallet,
    dollar,
    bank,
    receipt,
    cart,
    bed,
    question,
    accessibility,
    thumbUp,
    thumbDown,
    alignLeft,
    alignCenter,
    alignRight,
    bold,
    italic,
    code,
    terminal,
    database,
    server,
    cloud,
    inbox,
    paperclip,
    scissors,
    history,
    timeline,
    layers,
    layout,
    sidebar,
    maximize,
    minimize,
    loader,
    circle,
    square,
    checkbox,
    checkboxEmpty,
    radioOn,
    id,
    shield,
    alertCircle,
    power,
    battery,
    watch,
    alarm,
    leaf,
    broom,
    tshirt,
    party,
    keyboard,
    desktop,
    smartphone,
    tablet,
    faceId,
    touchId,
    equal,
    hash,
    atSign,
    percent,
    slash,
    move,
    crosshair,
    target,
    compass,
    navigation,
    send,
    reply,
    forward,
    userPlus,
    userMinus,
    userCheck,
    userX,
    shoppingBag,
    packageIcon,
    truck,
    map,
    book,
    bookOpen,
    newspaper,
    coffee,
    music,
    film,
    headphones,
    award,
    infinity,
    signal,
    cast,
    airplay,
    hardDrive,
    cpu,
    tool,
    hammer,
    lifeBuoy,
    anchor,
  )

  val byName: Map[String, Icon] = all.map(i => i.name -> i).toMap

}
