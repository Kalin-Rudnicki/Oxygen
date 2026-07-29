package oxygen.ui.web.component

import org.scalajs.dom.{FileList, HTMLInputElement}
import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}
import zio.ZIO

/**
  * W11-T04: HTML file drop zone — drag/drop **or** click to open the system file picker.
  *
  * Configurable prompt, `accept` MIME/extension filter, and multi-file selection.
  */
final case class FileDropZone(
    private val _prompt: String,
    private val _accept: Option[String],
    private val _multiple: Boolean,
) extends PWidget.Deferred.Stateful[Any, Nothing, FileDropZone.State] {

  def prompt(p: String): FileDropZone = copy(_prompt = p)
  def accept(a: String): FileDropZone = copy(_accept = Option(a).filter(_.nonEmpty))
  def accept(a: Option[String]): FileDropZone = copy(_accept = a.filter(_.nonEmpty))
  def noAccept: FileDropZone = copy(_accept = None)
  def multiple(m: Boolean): FileDropZone = copy(_multiple = m)
  def single: FileDropZone = multiple(false)
  def multi: FileDropZone = multiple(true)

  override protected def build: PWidget[Any, Nothing, FileDropZone.State, FileDropZone.State] =
    FileDropZone.render(this)

}
object FileDropZone {

  final case class State(hover: Boolean = false, lastNames: List[String] = Nil) {
    def enter: State = copy(hover = true)
    def leave: State = copy(hover = false)
    def dropped(names: List[String]): State = copy(hover = false, lastNames = names)
  }

  private def namesFrom(files: FileList): List[String] =
    (0 until files.length).map(i => files(i).name).toList

  val empty: FileDropZone =
    FileDropZone(
      _prompt = "Drop files here or click to browse",
      _accept = None,
      _multiple = true,
    )

  def apply(): FileDropZone = empty

  def apply(prompt: String): FileDropZone = empty.prompt(prompt)

  def apply(configure: FileDropZone => FileDropZone): FileDropZone =
    configure(empty)

  private def render(cfg: FileDropZone): WidgetS[State] =
    Widget.state[State].fix { st =>
      val s = st.renderTimeValue
      div(
        position.relative,
        padding := S.spacing._8,
        border(2.px, "dashed", if s.hover then S.color.primary.standard else S.color.fg.subtle),
        borderRadius := S.borderRadius._4,
        backgroundColor := (if s.hover then S.color.primary.subtle else S.color.bg.layerOne),
        textAlign.center,
        color := S.color.fg.moderate,
        cursor.pointer,
        userSelect.none,
        DnD.allowDrop,
        onDragEnter := st.update(_.enter),
        onDragLeave := st.update(_.leave),
        onDrop.e.handle { e =>
          e.preventDefault()
          st.update(_.dropped(namesFrom(e.dataTransfer.files)))
        },
        // Click anywhere on the zone → open hidden file input.
        onClick.e.handle { e =>
          val root = e.currentTarget.asInstanceOf[org.scalajs.dom.Element]
          val input = root.querySelector("input[type=file]").asInstanceOf[HTMLInputElement]
          if input != null then input.click()
          ZIO.unit
        },
        // Visually hidden native picker (still in DOM for a11y / click()).
        input(
          `type`.file,
          Widget.when(cfg._multiple)(multiple := "multiple"),
          cfg._accept.map(a => Widget.raw.htmlAttr("accept", a)).getOrElse(Widget.empty),
          // keep off-layout but focusable via parent click
          position.absolute,
          width := 1.px,
          height := 1.px,
          opacity := "0",
          overflow.hidden,
          Widget.raw.css("clip", "rect(0,0,0,0)"),
          // stop bubble so parent onClick doesn't re-open after pick
          onClick.e.handle { e =>
            e.stopPropagation()
            ZIO.unit
          },
          onChange.e.handle { e =>
            val el = e.target.asInstanceOf[HTMLInputElement]
            val names = Option(el.files).map(namesFrom).getOrElse(Nil)
            // allow re-selecting the same file later
            el.value = ""
            st.update(_.dropped(names))
          },
        ),
        Icon.upload.lg,
        div(height := S.spacing._2),
        cfg._prompt,
        div(
          marginTop := S.spacing._2,
          fontSize := S.fontSize._1,
          color := S.color.fg.subtle,
          "Drag & drop or click to choose files",
        ),
        if s.lastNames.nonEmpty then
          div(
            marginTop := S.spacing._3,
            fontSize := S.fontSize._2,
            color := S.color.fg.default,
            s"Received: ${s.lastNames.mkString(", ")}",
          )
        else Widget.empty,
      )
    }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Form (labeled, composable Deferred builder)
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /**
    * Labeled file drop zone form. Value is last received file names (`List[String]`).
    *
    * {{{
    * FileDropZone.form("Attachments").prompt("Drop CSV").zoomOut[Page](_.files)
    * }}}
    */
  final case class form private (
      private val _fieldName: String,
      private val _label: Label,
      private val _zone: FileDropZone,
      private val _surroundingPadding: String,
      private val _labelSpacing: Option[String],
  ) extends PForm.Deferred.Stateful[Any, Nothing, FileDropZone.State, List[String]] {

    override protected lazy val build: PForm[Any, Nothing, FileDropZone.State, FileDropZone.State, List[String]] =
      Form.makeWith(
        _fieldName,
        div(
          padding := _surroundingPadding,
          maxWidth := 100.pct,
          boxSizing.borderBox,
          _label,
          Spacing.vertical.opt(_labelSpacing),
          _zone,
        ),
      )(_.lastNames)

    def modLabel(f: Label => Label): form = copy(_label = f(_label))
    def modZone(f: FileDropZone => FileDropZone): form = copy(_zone = f(_zone))
    def label: Label = _label
    def zone: FileDropZone = _zone
    def prompt(p: String): form = modZone(_.prompt(p))
    def accept(a: String): form = modZone(_.accept(a))
    def multiple(m: Boolean): form = modZone(_.multiple(m))
    def single: form = modZone(_.single)
    def multi: form = modZone(_.multi)
    def describe(d: Widget): form = modLabel(_.describe(d))
    def labelMod(mods: Widget*): form = modLabel(_.mod(mods*))
    def surroundingPadding(p: String): form = copy(_surroundingPadding = p)
    def labelSpacing(s: Option[String]): form = copy(_labelSpacing = s)
    def noLabelSpacing: form = labelSpacing(None)

  }
  object form {

    def apply(label: String): FileDropZone.form =
      new FileDropZone.form(
        _fieldName = label,
        _label = Label(label),
        _zone = FileDropZone.empty,
        _surroundingPadding = 10.px,
        _labelSpacing = Some(Label.defaultInputSpacing),
      )

  }

}
