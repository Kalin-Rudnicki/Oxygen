package oxygen.ui.web.defaults

import oxygen.predef.core.*
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import zio.{Scope, ZIO}

object ComponentsPage extends RoutablePage.NoParams[Any] {

  final case class State(
      sequence: ArraySeq[Int],
      useGlobalToggleThumbs: Boolean,
      individualToggleThumbs: Set[(ToggleThumb.Style, ToggleThumb.Style, ToggleThumb.Size)],
      globalToggleThumbs: Boolean,
      horizontalRadio: HorizontalRadio.State[SmallEnum],
      dropdown1: Dropdown.State[SmallEnum],
      dropdown2: Dropdown.State[SmallEnum],
      modal: Option[ModalForm],
      textValue: String,
  )

  final case class ModalForm(
      value: String,
  )

  override val path: Seq[String] = Seq("page", "internal", "components")

  override def title(state: State): String = "Components"

  override def initialLoad(params: Unit): ZIO[Scope, UIError, State] =
    ZIO.succeed {
      State(
        sequence = ArraySeq.empty,
        useGlobalToggleThumbs = false,
        individualToggleThumbs = Set.empty,
        globalToggleThumbs = false,
        horizontalRadio = HorizontalRadio.State.initialFirst,
        dropdown1 = Dropdown.State.initialNone,
        dropdown2 = Dropdown.State.initialNone,
        modal = None,
        textValue = "",
      )
    }

  override def postLoad(state: WidgetState[State]): ZIO[Scope, UIError, Unit] = ZIO.unit

  override protected def component(state: State): WidgetS[State] =
    fragment(
      O.Scrollable,
      h1("Components"),
      iconSection, // TODO (KR) : move down
      miscSection,
      sectionSection,
      buttonsSection,
      sequenceSection.zoomOut[State](_.sequence),
      toggleThumbSection,
      horizontalRadioSection,
      formSection,
      div(height := 25.px),
    )

  enum SmallEnum derives StrictEnum { case A, B, C }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Misc Section
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private lazy val miscSection: WidgetS[State] =
    Section.section1("Misc.")(
      pageMessageSection,
      modalSection.zoomOut[State](_.modal),
    )

  private lazy val pageMessageSection: Widget =
    Section.section2("Page Messages")(
      PageMessagesBottomCorner.attached,
      Widget.foreach(
        PageMessage.Type.Success -> Button.Style.Positive,
        PageMessage.Type.Info -> Button.Style.Info,
        PageMessage.Type.Warning -> Button.Style.Alert,
        PageMessage.Type.Error -> Button.Style.Negative,
      ) { case (messageType, buttonType) =>
        span(
          display.inlineBlock,
          padding(S.spacing._3, S.spacing._5),
          Button(
            messageType.toString,
            _(style = buttonType),
          )(
            Widget.withPageInstance {
              onClick := PageMessages.add(PageMessage.make(messageType, s"This is a \"$messageType\" page message"))
            },
          ),
        )
      },
    )

  private lazy val modalSection: WidgetS[Option[ModalForm]] =
    Section.section2("Modal")(
      Button("Show Modal")(onClick.s[Option[ModalForm]].setState(ModalForm("").some)),
      Modal.option()(
        h1("Hello Modal"),
        padding(S.spacing._0, S.spacing._5),
        Section.info(_(borderColor = S.color.status.informational))(
          "This is a modal",
        ),
        Widget.withPageInstance {
          {
            Form.textField[String]("Value").zoomOut[ModalForm](_.value).required &&
            Form.submitButton("Submit")
          }.onSubmit.asv[Modal.Close] { (_, rh, v) =>
            PageMessages.add(PageMessage.info(s"Submit:\n$v")) *>
              rh.raiseAction(Modal.Close)
          }
        },
      ),
    )

  private lazy val iconSection: Widget = {
    val space: Widget =
      span(
        display.inlineBlock,
        width := 24.px,
        height := 24.px,
      )

    Section.section1("Icon")(
      color.blue,
      svg(
        htmlWidth := 24,
        htmlHeight := 24,
        svgViewBox := "0 0 24 24",
        svgFill := "none",
        svgStroke := "currentColor",
        svgStrokeWidth := 2,
        svgStrokeLineCap.round,
        svgStrokeLineJoin.round,
        svgPolygon(
          svgPoints := "12 2 15.09 8.26 22 9.27 17 14.14 18.18 21.02 12 17.77 5.82 21.02 7 14.14 2 9.27 8.91 8.26 12 2",
        ),
      )(
        color.yellow,
      ),
      space,
      svg(
        htmlWidth := 24,
        htmlHeight := 24,
        svgViewBox := "0 0 24 24",
        svgFill := "currentColor",
        svgStroke := "currentColor",
        svgStrokeWidth := 2,
        svgStrokeLineCap.round,
        svgStrokeLineJoin.round,
        svgPath(
          svgD := "M20.84 4.61a5.5 5.5 0 0 0-7.78 0L12 5.67l-1.06-1.06a5.5 5.5 0 0 0-7.78 7.78l1.06 1.06L12 21.23l7.78-7.78 1.06-1.06a5.5 5.5 0 0 0 0-7.78z",
        ),
      )(
        color.red,
      ),
      space,
      svg(
        htmlWidth := 24,
        htmlHeight := 24,
        svgViewBox := "0 0 24 24",
        svgFill := "none",
        svgStroke := "currentColor",
        svgG(),
        svgG(svgStrokeLineCap.round, svgStrokeLineJoin.round),
        svgG(
          svgStrokeWidth := 1,
          svgPath(
            svgD := "M4.67,18.788A9.991,9.991,0,0,0,22,12V6a1,1,0,0,0-1.062-1,21.6,21.6,0,0,0-3.854.731,10.569,10.569,0,0,0-4.767-3.681,1,1,0,0,0-.635,0A10.592,10.592,0,0,0,6.931,5.713,21.024,21.024,0,0,0,3.063,5,1,1,0,0,0,2,6v6a9.93,9.93,0,0,0,2.592,6.679A.938.938,0,0,0,4.67,18.788ZM20,12a8.009,8.009,0,0,1-8,8,7.892,7.892,0,0,1-5.481-2.186C8.13,11.586,13.731,8.049,20,7.12ZM8.954,6.368A8.9,8.9,0,0,1,12,4.078a8.749,8.749,0,0,1,3.045,2.288A20.715,20.715,0,0,0,12,7.788c-.031-.018-.066-.033-.1-.051q-.655-.369-1.308-.676L10.44,6.99c-.5-.23-.989-.435-1.469-.615ZM4,7.128a21.322,21.322,0,0,1,6.091,1.917A14.7,14.7,0,0,0,5.1,15.656c-.019.05-.041.1-.06.148-.008.022-.018.042-.026.064A7.908,7.908,0,0,1,4,12Z",
          ),
        ),
      )(
        color.cyan,
        backgroundColor.red,
        borderRadius := 15.px,
        padding := 4.px,
        width := 50.px,
        height := 50.px,
      ),
      space,
      basicSvg(24, 24)(
        svgPath(
          svgFill.none,
          svgStrokeWidth := 2,
          svgStroke := S.color.brand.primary1.getColorValue.toUpperCase,
          svgD := "M 12 0 L 4 24 L 24 8 L 0 8 L 20 24 Z",
        ),
      )(
        backgroundColor.black,
        padding := 6.px,
        borderRadius := 16.px,
      ),
      space,
      basicSvg(24, 24)(
        svgPath(
          svgFill.none,
          svgStrokeWidth := 2,
          svgStroke := S.color.brand.primary1.getColorValue,
          svgD := "M 12 0 L 0 24 L 24 24 Z",
        ),
        svgPath(
          svgFill.none,
          svgStrokeWidth := 2,
          svgStroke := S.color.brand.primary2.getColorValue,
          svgD := "M 12 7 L 6 20 L 18 20 Z",
        ),
      )(
        backgroundColor.black,
        padding := 6.px,
        borderRadius := 10.px,
      ),
    )
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Other
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private lazy val sectionSection: Widget =
    Section.section1("Section 1")(
      Section.section2("Section 2")(
        span("TODO", color := S.color.status.negative, fontSize := S.fontSize._6),
      ),
      Section.section2("Section 2", _(headerColor = S.color.status.alert))(
        Section.info()(
          "Info Section",
        ),
        Section.info(_(borderColor = S.color.status.positive, backHighlight = true))(
          "Positive Info Section",
        ),
        Section.info(_(borderColor = S.color.status.negative))(
          "Negative Info Section",
        ),
        Section.info(_(borderColor = S.color.status.alert, backHighlight = true))(
          width := 50.pct,
          "Alert Info Section",
        ),
      ),
    )

  private lazy val buttonsSection: Widget = {
    val styles: Seq[Button.Style] = Button.Style.values.toSeq
    val sizes: Seq[Button.Size] = Button.Size.values.toSeq

    Section.section1("Buttons")(
      table(
        borderCollapse.collapse,
        tr(
          border.csss(2.px, "solid", "black"),
          th("Style", padding(S.spacing._2, S.spacing._5)),
          Widget.foreach(sizes) { size =>
            th(size.toString, padding(S.spacing._2, S.spacing._5))
          },
        ),
        Widget.foreach(styles) { style =>
          tr(
            border.csss(2.px, "solid", "black"),
            td(style.toString, padding(S.spacing._2, S.spacing._5)),
            Widget.foreach(sizes) { size =>
              td(
                textAlign.center,
                Button("Click Me!", _(style = style, size = size)),
                padding(S.spacing._2, S.spacing._5),
              )
            },
          )
        },
      ),
    )
  }

  private lazy val toggleThumbSection: WidgetS[State] = {
    val sizes: Seq[ToggleThumb.Size] = ToggleThumb.Size.values.toSeq
    val styles: Seq[(ToggleThumb.Style, ToggleThumb.Style)] =
      ToggleThumb.Style.values.toSeq.map((_, ToggleThumb.Style.Off)) ++ Seq(
        ToggleThumb.Style.Positive -> ToggleThumb.Style.Negative,
      )

    Widget.state[State].fix { state =>
      def row(onStyle: ToggleThumb.Style, offStyle: ToggleThumb.Style): Widget =
        tr(
          border.csss(2.px, "solid", "black"),
          td(padding(S.spacing._1, S.spacing._3))(s"$onStyle / $offStyle"),
          Widget.foreach(sizes) { size =>
            td(padding(S.spacing._1, S.spacing._3))(
              textAlign.center,
              if (state.renderTimeValue.useGlobalToggleThumbs)
                ToggleThumb
                  .boolean(_(onStyle = onStyle, offStyle = offStyle, size = size))
                  .attach(state.zoomIn(_.globalToggleThumbs))
              else
                ToggleThumb
                  .set((onStyle, offStyle, size), _(onStyle = onStyle, offStyle = offStyle, size = size))
                  .attach(state.zoomIn(_.individualToggleThumbs)),
            )
          },
        )

      Section.section1("Toggle Thumb")(
        div(
          display.flex,
          alignItems.center,
          ToggleThumb
            .boolean(_(onStyle = ToggleThumb.Style.Positive))
            .attach(state.zoomIn(_.useGlobalToggleThumbs)),
          span(display.inlineBlock, width := 10.px),
          span("Use Global State"),
        ),
        div(height := 25.px),
        table(
          borderCollapse.collapse,
          tr(
            th(padding(S.spacing._1, S.spacing._3))("Style"),
            Widget.foreach(sizes) { size =>
              th(padding(S.spacing._1, S.spacing._3))(size.toString)
            },
          ),
          Widget.foreach(styles) { row(_, _) },
        ),
      )
    }
  }

  private lazy val horizontalRadioSection: WidgetS[State] = {
    val sizes: Seq[HorizontalRadio.Size] = HorizontalRadio.Size.values.toSeq
    val styles: Seq[(HorizontalRadio.Style, HorizontalRadio.Style)] =
      HorizontalRadio.Style.values.toSeq.map((_, HorizontalRadio.Style.Off)) ++ Seq(
        HorizontalRadio.Style.Positive -> HorizontalRadio.Style.Negative,
      )

    Widget.state[State].fix { state =>
      def row(selectedStyle: HorizontalRadio.Style, notSelectedStyle: HorizontalRadio.Style): Widget =
        tr(
          border.csss(2.px, "solid", "black"),
          td(padding(S.spacing._1, S.spacing._3))(s"$selectedStyle / $notSelectedStyle"),
          Widget.foreach(sizes) { size =>
            td(padding(S.spacing._1, S.spacing._3))(
              textAlign.center,
              HorizontalRadio(_(selectedStyle = selectedStyle, notSelectedStyle = notSelectedStyle, size = size))
                .attach(state.zoomIn(_.horizontalRadio)),
            )
          },
        )

      Section.section1("Horizontal Radio")(
        table(
          borderCollapse.collapse,
          tr(
            th(padding(S.spacing._1, S.spacing._3))("Style"),
            Widget.foreach(sizes) { size =>
              th(padding(S.spacing._1, S.spacing._3))(size.toString)
            },
          ),
          Widget.foreach(styles) { row(_, _) },
        ),
      )
    }
  }

  private lazy val sequenceSection: WidgetS[ArraySeq[Int]] = {
    def elem(idx: Int): WidgetAS[Int, Int] =
      Widget.state[Int].fix { state =>
        tr(
          border.csss(2.px, "solid", "black"),
          td(
            padding(S.spacing._1, S.spacing._3),
            state.renderTimeValue.toString,
          ),
          td(
            padding(S.spacing._1, S.spacing._3),
            Button("+")(
              onClick := state.update(_ + 1),
            ),
          ),
          td(
            padding(S.spacing._1, S.spacing._3),
            Button("-")(
              onClick := state.update(_ - 1),
            ),
          ),
          td(
            padding(S.spacing._1, S.spacing._3),
            Button("Remove", _(style = Button.Style.DestructiveSubtle))(
              onClick.action(idx),
            ),
          ),
        )
      }

    Section.section1("Sequence")(
      div(
        Button("Add")(
          onClick.updateState[ArraySeq[Int]](0 +: _),
        ),
      ),
      div(height := 10.px),
      table(
        borderCollapse.collapse,
        tr(
          border.csss(2.px, "solid", "black"),
          th(padding(S.spacing._1, S.spacing._3))("Value"),
          th(padding(S.spacing._1, S.spacing._3))("+"),
          th(padding(S.spacing._1, S.spacing._3))("-"),
          th(padding(S.spacing._1, S.spacing._3))("Remove"),
        ),
        Widget.seq[ArraySeq].withIndex(elem).handleActionStateful.s { (s, idx) =>
          s.update { v =>
            val (before, atAndAfter) = v.splitAt(idx)
            before ++ atAndAfter.drop(1)
          }
        },
      ),
      div(height := 10.px),
      div(
        Button("Add")(
          onClick.updateState[ArraySeq[Int]](_ :+ 0),
        ),
      ),
    )
  }

  private lazy val formSection: WidgetS[State] = {
    Section.section1("Form")(
      Form.textField[String]("Text Field 1").widget.discardAction.zoomOut[State](_.textValue),
      Form.textField[String]("Text Field 2", "test").widget.discardAction.zoomOut[State](_.textValue),
      Form.textField[String]("Text Field 3", fragment("a", br, "b")).widget.discardAction.zoomOut[State](_.textValue),
      Form.textArea[String]("Text Area").widget.discardAction.zoomOut[State](_.textValue),
      Form.horizontalRadio[SmallEnum]("Horizontal Radio 1").widget.discardAction.zoomOut[State](_.horizontalRadio),
      Form.horizontalRadio[SmallEnum]("Horizontal Radio 2", "descr").widget.discardAction.zoomOut[State](_.horizontalRadio),
      Form.dropdown[SmallEnum]("Dropdown 1", "descr").widget.discardAction.zoomOut[State](_.dropdown1),
      Form.dropdown[SmallEnum]("Dropdown 2", inputProps = _.apply(style = Dropdown.Style.Alert, closeOnMouseLeave = true), showSetNone = "Unset").widget.discardAction.zoomOut[State](_.dropdown2),
    )
  }

}
