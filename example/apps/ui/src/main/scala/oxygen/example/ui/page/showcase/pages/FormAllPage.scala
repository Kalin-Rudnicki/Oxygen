package oxygen.example.ui.page.showcase.pages

import oxygen.core.typeclass.StrictEnum
import oxygen.example.ui.page.showcase.ShowcaseLayout
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import zio.*

object FormAllPage extends RoutablePage.NoParams[Any] {

  enum Role derives StrictEnum { case Engineer, Designer, Manager, Other }
  enum SizePref derives StrictEnum { case Small, Medium, Large }
  enum Plan derives StrictEnum { case Free, Pro, Team, Enterprise }

  final case class PageState(
      // text family
      fullName: TextField.State = TextField.State.empty,
      email: TextField.State = TextField.State.empty,
      password: TextField.State = TextField.State.empty,
      username: TextField.State = TextField.State.initial("ada"),
      // textarea
      bio: TextArea.State = TextArea.State.empty,
      notes: TextArea.State = TextArea.State.initial("Optional notes…"),
      // choices
      acceptTerms: Boolean = false,
      newsletter: Boolean = true,
      darkMode: Boolean = false,
      notifications: Boolean = true,
      role: HorizontalRadio.State[Role] = HorizontalRadio.State.initialFirst[Role],
      sizePref: HorizontalRadio.State[SizePref] = HorizontalRadio.State.initial[SizePref](SizePref.Medium),
      plan: Dropdown.State[Plan] = Dropdown.State.initialFirst[Plan],
      country: Dropdown.State[String] =
        Dropdown.State.initialFirst(Seq("United States", "Canada", "United Kingdom", "Germany", "Japan", "Australia")),
      // datetime
      startDate: DatePicker.State = DatePicker.State.today,
      meetingTime: TimePicker.State = TimePicker.State.noon,
      kickoff: DateTimePicker.State = DateTimePicker.State.empty,
      // color + files
      brandColor: ColorPicker.State = ColorPicker.State.of("#3b82f6"),
      attachments: FileDropZone.State = FileDropZone.State(),
  )

  override val path: Seq[String] = Seq("showcase", "forms", "all")
  override def title(s: PageState): String = "All form fields"
  override def initialLoad(params: Unit): ZIO[Scope, UIError, PageState] = ZIO.succeed(PageState())
  override def postLoad(state: WidgetState[PageState], initialState: PageState): ZIO[Scope, UIError, Unit] = ZIO.unit

  private def vSpace: Widget = div(height := S.spacing._4)
  private def sectionTitle(t: String): Widget =
    h3(t, marginTop := S.spacing._2, marginBottom := S.spacing._3, color := S.color.fg.default)

  override protected def component(state: WidgetState[PageState], renderState: PageState): WidgetS[PageState] = {
    val s = renderState
    ShowcaseLayout
      .page(FormAllPage, "All form fields")(
        ShowcaseLayout.note(
          "Kitchen-sink of supported form controls (mock only). Scroll the page — every control is live and bound to page state.",
        ),
        ////// Text fields
        sectionTitle("Text fields"),
        TextField.form[String]("Full name").describe("TextField · text").width(36.ch).widget.discardAction.zoomOut[PageState](_.fullName),
        TextField.form[String]("Email").email.describe("TextField · email").width(36.ch).widget.discardAction.zoomOut[PageState](_.email),
        TextField.form[String]("Password").password.describe("TextField · password").width(36.ch).widget.discardAction.zoomOut[PageState](_.password),
        TextField.form[String]("Username").describe("Pre-filled TextField").width(36.ch).widget.discardAction.zoomOut[PageState](_.username),
        vSpace,
        ////// Text areas
        sectionTitle("Text areas"),
        TextArea.form[String]("Bio").describe("TextArea · multi-line").width(100.pct).height(6.rem).widget.discardAction.zoomOut[PageState](_.bio),
        TextArea.form[String]("Internal notes").describe("Optional notes").width(100.pct).height(5.rem).widget.discardAction.zoomOut[PageState](_.notes),
        vSpace,
        ////// Checkboxes + toggle
        sectionTitle("Checkboxes & toggle"),
        Checkbox.boolean("I agree to the terms of service").zoomOut[PageState](_.acceptTerms),
        div(height := S.spacing._2),
        Checkbox.boolean("Subscribe to product newsletter").zoomOut[PageState](_.newsletter),
        div(height := S.spacing._3),
        div(
          display.flex,
          alignItems.center,
          gap := S.spacing._3,
          flexWrap.wrap,
          ToggleThumb.boolean.zoomOut[PageState](_.darkMode),
          span(color := S.color.fg.default, "Dark mode preference (ToggleThumb)"),
        ),
        div(height := S.spacing._2),
        div(
          display.flex,
          alignItems.center,
          gap := S.spacing._3,
          flexWrap.wrap,
          ToggleThumb.boolean(_.positive).zoomOut[PageState](_.notifications),
          span(color := S.color.fg.default, "Enable notifications (positive toggle)"),
        ),
        vSpace,
        ////// Segmented radio
        sectionTitle("Horizontal radio (segmented)"),
        div(
          marginBottom := S.spacing._4,
          Label("Role"),
          div(height := S.spacing._1),
          HorizontalRadio.of[Role].configure(_.primary.medium).zoomOut[PageState](_.role),
        ),
        div(
          marginBottom := S.spacing._4,
          Label("Preferred size"),
          div(height := S.spacing._1),
          HorizontalRadio.of[SizePref].configure(_.informational.small).zoomOut[PageState](_.sizePref),
        ),
        vSpace,
        ////// Dropdowns
        sectionTitle("Dropdowns"),
        div(
          marginBottom := S.spacing._4,
          Label("Plan"),
          div(height := S.spacing._1),
          Dropdown.of[Plan].configure(_.primary.medium.width(28.ch)).zoomOut[PageState](_.plan),
        ),
        div(
          marginBottom := S.spacing._4,
          Label("Country"),
          div(height := S.spacing._1),
          Dropdown.of[String].configure(_.medium.width(28.ch).setNone("— select —")).zoomOut[PageState](_.country),
        ),
        vSpace,
        ////// Date / time
        sectionTitle("Date & time pickers"),
        div(
          marginBottom := S.spacing._4,
          Label("Start date (DatePicker)"),
          div(height := S.spacing._1),
          DatePicker.empty.zoomOut[PageState](_.startDate),
        ),
        div(
          marginBottom := S.spacing._4,
          Label("Meeting time (TimePicker)"),
          div(height := S.spacing._1),
          TimePicker().zoomOut[PageState](_.meetingTime),
        ),
        div(
          marginBottom := S.spacing._4,
          Label("Kickoff (DateTimePicker)"),
          div(height := S.spacing._1),
          DateTimePicker.empty.zoomOut[PageState](_.kickoff),
        ),
        vSpace,
        ////// Color
        sectionTitle("Color"),
        div(
          marginBottom := S.spacing._4,
          Label("Brand color (ColorPicker)"),
          div(height := S.spacing._1),
          ColorPicker.widget.zoomOut[PageState](_.brandColor),
        ),
        vSpace,
        ////// Upload
        sectionTitle("File upload"),
        div(
          marginBottom := S.spacing._4,
          Label("Attachments (FileDropZone)"),
          div(height := S.spacing._1),
          FileDropZone("Drop files here or click to browse").zoomOut[PageState](_.attachments),
        ),
        vSpace,
        ////// Live summary
        sectionTitle("Live state summary"),
        div(
          padding := S.spacing._4,
          backgroundColor := S.color.bg.layerTwo,
          borderRadius := S.borderRadius._3,
          border(1.px, "solid", S.color.bg.layerThree),
          fontSize := S.fontSize._2,
          color := S.color.fg.moderate,
          whiteSpace.pre,
          Seq(
            s"name       = ${s.fullName.text}",
            s"email      = ${s.email.text}",
            s"username   = ${s.username.text}",
            s"bio len    = ${s.bio.text.length}",
            s"terms      = ${s.acceptTerms}",
            s"newsletter = ${s.newsletter}",
            s"darkMode   = ${s.darkMode}",
            s"notify     = ${s.notifications}",
            s"role       = ${s.role.selected}",
            s"size       = ${s.sizePref.selected}",
            s"plan       = ${s.plan.selected}",
            s"country    = ${s.country.selected}",
            s"date       = ${s.startDate.selected}",
            s"time       = ${s.meetingTime.toLocalTime}",
            s"kickoff    = ${s.kickoff.toLocalDateTime}",
            s"color      = ${s.brandColor.hex}",
            s"files      = ${if s.attachments.lastNames.isEmpty then "(none)" else s.attachments.lastNames.mkString(", ")}",
          ).mkString("\n"),
        ),
        vSpace,
        ////// Actions
        sectionTitle("Actions"),
        div(
          display.flex,
          gap := S.spacing._2,
          flexWrap.wrap,
          alignItems.center,
          Button("Submit form")
            .primary
            .leading(Icon.check)
            .disabled(!s.acceptTerms)
            .content(
              onClick := PageMessages.add(
                PageMessage.positive(
                  s"Submitted (mock): ${emptyFallback(s.fullName.text, "anon")} · ${s.plan.selected.getOrElse("?")} · ${s.role.selected}",
                ),
              ),
            ),
          Button("Secondary").subtle,
          Button("Danger").negative.subtle.leading(Icon.trash),
          Button().iconOnly(Icon.settings).small.subtle,
          Button("Save").leading(Icon.save).small,
        ),
        div(height := S.spacing._2),
        span(
          fontSize := S.fontSize._1,
          color := S.color.fg.subtle,
          if s.acceptTerms then "Submit enabled — terms accepted."
          else "Accept terms to enable primary submit.",
        ),
      )
  }

  private def emptyFallback(s: String, fallback: String): String =
    if s.trim.isEmpty then fallback else s.trim

}
