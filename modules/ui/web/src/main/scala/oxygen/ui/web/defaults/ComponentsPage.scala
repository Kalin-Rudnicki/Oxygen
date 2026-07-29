package oxygen.ui.web.defaults

import oxygen.predef.core.*
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import oxygen.ui.web.service.Window
import zio.{Scope, ZIO}
import zio.http.{Path, QueryParams}

object ComponentsPage extends RoutablePage.NoParams[Any] {

  /** Large enough to prove pagination keeps DOM small; not virtualized 10k. */
  private val StressTotal: Int = 500
  private val StressItems: IndexedSeq[Int] = IndexedSeq.range(0, StressTotal)

  final case class PageState(
      sequence: ArraySeq[Int],
      useGlobalToggleThumbs: Boolean,
      individualToggleThumbs: Set[String],
      globalToggleThumbs: Boolean,
      horizontalRadio: HorizontalRadio.State[SmallEnum],
      dropdown1: Dropdown.State[BigEnum],
      dropdown2: Dropdown.State[SmallEnum],
      modal: Option[ModalForm],
      textField: TextField.State,
      textArea: TextArea.State,
      heart: Boolean,
      /** W6-T05: large-N pagination stress (items generated; only page slice in DOM). */
      stressPager: Pagination.State,
      lazyDemo: LazySection.State,
      /** W9-T04: filter string for icon index. */
      iconFilter: String,
      sortable: SortableList.State[String],
      fileDrop: FileDropZone.State,
  )

  final case class ModalForm(
      value: TextField.State,
  )

  override val path: Seq[String] = Seq("internal", "components")

  override def title(state: PageState): String = "Components"

  override def initialLoad(params: Unit): ZIO[Scope, UIError, PageState] =
    ZIO.succeed {
      PageState(
        sequence = ArraySeq.empty,
        useGlobalToggleThumbs = false,
        individualToggleThumbs = Set.empty,
        globalToggleThumbs = false,
        horizontalRadio = HorizontalRadio.State.initialFirst,
        dropdown1 = Dropdown.State.initialNone,
        dropdown2 = Dropdown.State.initialNone,
        modal = None,
        textField = TextField.State.empty,
        textArea = TextArea.State.empty,
        heart = false,
        stressPager = Pagination.State.initial(pageSize = 25, total = StressTotal),
        lazyDemo = LazySection.State(),
        iconFilter = "",
        sortable = SortableList.State.of(Seq("Alpha", "Bravo", "Charlie", "Delta")),
        fileDrop = FileDropZone.State(),
      )
    }

  override def postLoad(state: WidgetState[PageState], initialState: PageState): ZIO[Scope, UIError, Unit] = ZIO.unit

  override protected def component(state: WidgetState[PageState], renderState: PageState): WidgetS[PageState] =
    fragment(
      O.Scrollable,
      h1("Components — builder showcase"),
      p(
        color := S.color.fg.moderate,
        "HolyGrail builders, overflow cases, grid, pagination stress, lazy section. Gallery only — not Storybook.",
      ),
      iconSection, // TODO (KR) : move down
      miscSection,
      sectionSection,
      buttonsSection,
      sequenceSection.zoomOut[PageState](_.sequence),
      toggleThumbSection,
      horizontalRadioSection,
      formSection,
      overflowTortureSection,
      columnsDemoSection,
      stressPaginationSection.zoomOut[PageState](_.stressPager),
      lazySectionDemo.zoomOut[PageState](_.lazyDemo),
      dndDemoSection,
      div(height := 150.px),
    )

  enum SmallEnum derives StrictEnum { case A, B, C }

  enum BigEnum derives StrictEnum { case A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Misc Section
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private lazy val miscSection: WidgetS[PageState] =
    SectionWithHeader.section1("Misc.")(
      Section.section2()(
        Button("Open new page").content(
          onClick := Window.newTab(PageURL(Path.root, QueryParams.empty)),
        ),
      ),
      pageMessageSection,
      modalSection.zoomOut[PageState](_.modal),
    )

  private lazy val pageMessageSection: Widget =
    SectionWithHeader.section2("Page Messages")(
      PageMessagesBottomCorner.default,
      Widget.foreach(
        PageMessage.Type.Primary -> ((b: Button.Const) => b.primary),
        PageMessage.Type.Positive -> ((b: Button.Const) => b.positive),
        PageMessage.Type.Negative -> ((b: Button.Const) => b.negative),
        PageMessage.Type.Info -> ((b: Button.Const) => b.informational),
        PageMessage.Type.Warning -> ((b: Button.Const) => b.alert),
        PageMessage.Type.Error -> ((b: Button.Const) => b.negative),
      ) { case (messageType, style) =>
        span(
          display.inlineBlock,
          padding(S.spacing._3, S.spacing._5),
          style(Button(messageType.toString)).content(
            Widget.withPageInstance {
              onClick := PageMessages.add(PageMessage.make(messageType, s"This is a \"$messageType\" page message"))
            },
          ),
        )
      },
    )

  private lazy val modalSection: WidgetS[Option[ModalForm]] =
    SectionWithHeader.section2("Modal")(
      Button("Show Modal").content(onClick.s[Option[ModalForm]].setState(ModalForm(TextField.State.empty).some)),
      Modal.option()(
        h1("Hello Modal"),
        padding(S.spacing._0, S.spacing._5),
        InfoSection(_.informational)(
          "This is a modal",
        ),
        Widget.withPageInstance {
          {
            TextField.form[String]("Value").zoomOut[ModalForm](_.value).required <*>
              Button.form("Submit")
          }.handleActionStateful.a[Modal.Close] { (rh, _, v) =>
            PageMessages.add(PageMessage.info(s"Submit:\n$v")) *>
              rh.raiseAction(Modal.Close)
          }
        },
      ),
    )

  /** W9-T04: browsable/filterable index of shipped icons. */
  private lazy val iconSection: WidgetS[PageState] =
    Widget.state[PageState].fix { st =>
      val q = st.renderTimeValue.iconFilter.trim.toLowerCase
      val icons = Icon.all.filter(i => q.isEmpty || i.name.toLowerCase.contains(q))
      SectionWithHeader.section1(s"Icons (${icons.size} / ${Icon.all.size})")(
        p(
          color := S.color.fg.moderate,
          "Built-in stroke set (currentColor). Filter by name; use Icon.custom for app-specific SVG.",
        ),
        div(
          display.flex,
          alignItems.center,
          gap := S.spacing._3,
          marginBottom := S.spacing._4,
          span("Filter:", color := S.color.fg.moderate),
          input(
            `type`.text,
            value := st.renderTimeValue.iconFilter,
            onInput.e.handle { e =>
              val v = e.target.asInstanceOf[org.scalajs.dom.HTMLInputElement].value
              st.update(_.copy(iconFilter = v))
            },
            width := 24.ch,
            padding := S.spacing._2,
            border(1.px, "solid", S.color.fg.subtle),
            borderRadius := S.borderRadius._3,
            backgroundColor := S.color.bg.layerTwo,
            color := S.color.fg.default,
          ),
        ),
        div(
          display.flex,
          flexWrap.wrap,
          gap := S.spacing._3,
          color := S.color.fg.default,
          Widget.foreach(icons) { ic =>
            div(
              display.flex,
              flexDirection.column,
              alignItems.center,
              justifyContent.center,
              width := 96.px,
              minHeight := 72.px,
              padding := S.spacing._2,
              borderRadius := S.borderRadius._3,
              backgroundColor := S.color.bg.layerTwo,
              ic.md,
              span(
                ic.name,
                fontSize := S.fontSize._1,
                color := S.color.fg.subtle,
                marginTop := S.spacing._1,
                textAlign.center,
                O.Ellipsis,
                maxWidth := 88.px,
              ),
            )
          },
        ),
      )
    }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Other
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private lazy val sectionSection: Widget =
    SectionWithHeader.section1("Section 1")(
      SectionWithHeader.section2("Section 2")(
        span("TODO", color := S.color.status.negative, fontSize := S.fontSize._6),
      ),
      SectionWithHeader.section2("Section 2", _.informational)(
        InfoSection()(
          "Info Section",
        ),
        InfoSection(_.positive.backHighlight)(
          "Positive Info Section",
        ),
        InfoSection(_.negative.backHighlight)(
          "Negative Info Section",
        ),
        InfoSection(_.alert.backHighlight)(
          width := 50.pct,
          "Alert Info Section",
        ),
        SectionWithHeader.section3("Section 3"),
      ),
    )

  private lazy val buttonsSection: Widget = {
    final case class BtnStyle(name: String, f: Button.Const => Button.Const)
    final case class BtnSize(name: String, f: Button.Const => Button.Const)

    val styles: Seq[BtnStyle] =
      (for
        a <- Seq(
          BtnStyle("Primary", _.primary),
          BtnStyle("Positive", _.positive),
          BtnStyle("Negative", _.negative),
          BtnStyle("Alert", _.alert),
          BtnStyle("Informational", _.informational),
          BtnStyle("Destructive", _.destructive),
          BtnStyle("Accent", _.accent),
          BtnStyle("Neutral", _.neutral),
        )
        b <- Seq(
          BtnStyle("Solid", _.solid),
          BtnStyle("Subtle", _.subtle),
          BtnStyle("Minimal", _.minimal),
        )
      yield BtnStyle(s"${a.name} ${b.name}", a.f.andThen(b.f))) ++
        Seq(
          BtnStyle("Disabled", _.disabled(true)),
          BtnStyle("Progress", _.progress(true)),
        )
    val sizes: Seq[BtnSize] =
      Seq(
        BtnSize("XS", _.extraSmall),
        BtnSize("S", _.small),
        BtnSize("M", _.medium),
        BtnSize("L", _.large),
        BtnSize("XL", _.extraLarge),
      )

    SectionWithHeader.section1("Buttons")(
      table(
        borderCollapse.collapse,
        tr(
          border.csss(2.px, "solid", "black"),
          th("Style", padding(S.spacing._2, S.spacing._5)),
          Widget.foreach(sizes) { size =>
            th(size.name, padding(S.spacing._2, S.spacing._5))
          },
        ),
        Widget.foreach(styles) { style =>
          tr(
            border.csss(2.px, "solid", "black"),
            td(style.name, padding(S.spacing._2, S.spacing._5)),
            Widget.foreach(sizes) { size =>
              td(
                textAlign.center,
                size.f(style.f(Button("Click Me!"))),
                padding(S.spacing._2, S.spacing._5),
              )
            },
          )
        },
      ),
      div(height := 16.px),
      // W9-T05: icon slots
      p(color := S.color.fg.moderate, "Icon slots (leading / trailing / icon-only)"),
      div(
        display.flex,
        flexWrap.wrap,
        gap := S.spacing._3,
        alignItems.center,
        Button("Save").leading(Icon.save).small,
        Button("Next").trailing(Icon.chevronRight).primary.small,
        Button("Download").leading(Icon.download).trailing(Icon.externalLink).subtle.small,
        Button().iconOnly(Icon.settings).small.subtle,
        Button().iconOnly(Icon.trash).small.negative.subtle,
      ),
    )
  }

  private lazy val toggleThumbSection: WidgetS[PageState] = {
    final case class ToggleStyle(name: String, f: ToggleThumb => ToggleThumb)
    final case class ToggleSize(name: String, f: ToggleThumb => ToggleThumb)

    val sizes: Seq[ToggleSize] =
      Seq(
        ToggleSize("XS", _.extraSmall),
        ToggleSize("S", _.small),
        ToggleSize("M", _.medium),
        ToggleSize("L", _.large),
        ToggleSize("XL", _.extraLarge),
      )
    val styles: Seq[ToggleStyle] =
      Seq(
        ToggleStyle("Primary", _.primary),
        ToggleStyle("Positive", _.positive),
        ToggleStyle("Negative", _.negative),
        ToggleStyle("Alert", _.alert),
        ToggleStyle("Informational", _.informational),
        ToggleStyle("Brand1", _.brandPrimary1),
        ToggleStyle("Brand2", _.brandPrimary2),
        ToggleStyle("PosNeg", _.positiveNegative),
        ToggleStyle("Primary/Alert", _.primaryEnabled.alertDisabled),
      )

    Widget.state[PageState].fix { state =>
      def row(style: ToggleStyle): Widget =
        tr(
          border.csss(2.px, "solid", "black"),
          td(padding(S.spacing._1, S.spacing._3))(style.name),
          Widget.foreach(sizes) { size =>
            val cfg = size.f(style.f(ToggleThumb.empty))
            td(padding(S.spacing._1, S.spacing._3))(
              textAlign.center,
              if state.renderTimeValue.useGlobalToggleThumbs then
                cfg.boolean.attach(state.zoomIn(_.globalToggleThumbs))
              else
                cfg.set(style.name).attach(state.zoomIn(_.individualToggleThumbs)),
            )
          },
        )

      SectionWithHeader.section1("Toggle Thumb")(
        div(
          display.flex,
          alignItems.center,
          ToggleThumb
            .boolean(_.positiveEnabled.alertDisabled.large)
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
              th(padding(S.spacing._1, S.spacing._3))(size.name)
            },
          ),
          Widget.foreach(styles) { row },
        ),
      )
    }
  }

  private lazy val horizontalRadioSection: WidgetS[PageState] = {
    final case class RadioStyle(name: String, f: HorizontalRadio => HorizontalRadio)
    final case class RadioSize(name: String, f: HorizontalRadio => HorizontalRadio)

    val sizes: Seq[RadioSize] =
      Seq(RadioSize("S", _.small), RadioSize("M", _.medium), RadioSize("L", _.large))
    val styles: Seq[RadioStyle] =
      Seq(
        RadioStyle("Primary", _.primary),
        RadioStyle("Positive", _.positive),
        RadioStyle("Negative", _.negative),
        RadioStyle("Alert", _.alert),
        RadioStyle("Informational", _.informational),
        RadioStyle("Brand1", _.brandPrimary1),
        RadioStyle("Brand2", _.brandPrimary2),
        RadioStyle("PosNeg", _.positiveNegative),
        RadioStyle("Primary/Alert", _.primarySelected.alertNotSelected),
      )

    Widget.state[PageState].fix { state =>
      def row(style: RadioStyle): Widget =
        tr(
          border.csss(2.px, "solid", "black"),
          td(padding(S.spacing._1, S.spacing._3))(style.name),
          Widget.foreach(sizes) { size =>
            td(padding(S.spacing._1, S.spacing._3))(
              textAlign.center,
              size.f(style.f(HorizontalRadio.empty)).of[SmallEnum]
                .attach(state.zoomIn(_.horizontalRadio)),
            )
          },
        )

      SectionWithHeader.section1("Horizontal Radio")(
        table(
          borderCollapse.collapse,
          tr(
            th(padding(S.spacing._1, S.spacing._3))("Style"),
            Widget.foreach(sizes) { size =>
              th(padding(S.spacing._1, S.spacing._3))(size.name)
            },
          ),
          Widget.foreach(styles) { row },
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
            Button("-").small.content(
              onClick := state.update(_ - 1),
            ),
          ),
          td(
            padding(S.spacing._1, S.spacing._3),
            Button("+").small.content(
              onClick := state.update(_ + 1),
            ),
          ),
          td(
            padding(S.spacing._1, S.spacing._3),
            Button("Remove").destructive.minimal.small.content(
              onClick.action(idx),
            ),
          ),
        )
      }

    SectionWithHeader.section1("Sequence")(
      div(
        Button("Add").content(
          onClick.updateState[ArraySeq[Int]](0 +: _),
        ),
      ),
      div(height := 10.px),
      table(
        borderCollapse.collapse,
        tr(
          border.csss(2.px, "solid", "black"),
          th(padding(S.spacing._1, S.spacing._3))("Value"),
          th(padding(S.spacing._1, S.spacing._3))("-"),
          th(padding(S.spacing._1, S.spacing._3))("+"),
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
        Button("Add").content(
          onClick.updateState[ArraySeq[Int]](_ :+ 0),
        ),
      ),
    )
  }

  private lazy val formSection: WidgetS[PageState] = {
    SectionWithHeader.section1("Form")(
      TextField.form[String]("Text Field 1").widget.discardAction.zoomOut[PageState](_.textField),
      TextField.form[String]("Text Field 2").describe("test").widget.discardAction.zoomOut[PageState](_.textField),
      TextField.form[String]("Text Field 3").describe("a\nb").labelMod(color.red).widget.discardAction.zoomOut[PageState](_.textField),
      TextArea.form[String]("Text Area").widget.discardAction.zoomOut[PageState](_.textArea),
      HorizontalRadio
        .form[SmallEnum]("Horizontal Radio 1")
        .modRadio(_.buttonExtra("[ ", " ]", fontWeight := S.fontWeight.extraBold))
        .widget
        .discardAction
        .zoomOut[PageState](_.horizontalRadio),
      HorizontalRadio.form[SmallEnum]("Horizontal Radio 2").describe("descr").widget.discardAction.zoomOut[PageState](_.horizontalRadio),
      Dropdown.form[BigEnum]("Dropdown 1").describe("descr").widget.discardAction.zoomOut[PageState](_.dropdown1),
      Dropdown
        .form[SmallEnum]("Dropdown 2")
        .modDropdown(_.negative.closeOnMouseLeave.setNone("Unset").externalBorder(3.px, "red").internalBorder(1.px, "blue").maxDropdownHeight(250.px))
        .widget
        .discardAction
        .zoomOut[PageState](_.dropdown2),
      Button.form("Submit").widget.discardAction,
    )
  }

  /** W6-T05: large-N via pagination — only current page in the DOM. */
  private lazy val stressPaginationSection: WidgetS[Pagination.State] =
    fragment(
      SectionWithHeader.section1("Stress pagination (large N)")(
        p(
          color := S.color.fg.moderate,
          s"$StressTotal synthetic rows; page size 25. Only the current slice is rendered.",
        ),
      ).fixState[Pagination.State],
      Pagination.controls,
      div(height := 8.px).fixState[Pagination.State],
      Widget.state[Pagination.State].fix { st =>
        val rows: Seq[Int] = st.renderTimeValue.slice(StressItems)
        table(
          width := 100.pct,
          thead(tr(th("#"), th("Label"), th("Token"))),
          tbody(
            Widget.foreach(rows) { (i: Int) =>
              tr(
                td(i.toString),
                td(s"Row $i"),
                td(f"tok-$i%04x"),
              )
            },
          ),
        )
      },
    )

  /** W6-T04 dogfood: deferred body. */
  private lazy val lazySectionDemo: WidgetS[LazySection.State] =
    LazySection.panel("Lazy section (expand to build body)")(
      div(
        color := S.color.fg.default,
        "This body was not in the tree while collapsed.",
        div(height := 8.px),
        Button("Nested action").small.subtle,
      ),
    )

  /** W11-T05: DnD gut-check — sortable list + file drop. */
  private lazy val dndDemoSection: WidgetS[PageState] =
    SectionWithHeader.section1("Drag & drop")(
      p(color := S.color.fg.moderate, "HTML5 DnD: drag rows to reorder; drop files on the zone."),
      SortableList(Widget.state[String].get(name => span(name))).zoomOut[PageState](_.sortable),
      div(height := S.spacing._4),
      FileDropZone().zoomOut[PageState](_.fileDrop),
    )

  /** W5-T05: column collapse gut-check (resize viewport). */
  private lazy val columnsDemoSection: Widget = {
    def cell(label: String): Widget =
      div(
        backgroundColor := S.color.bg.layerTwo,
        padding := S.spacing._3,
        borderRadius := S.borderRadius._3,
        label,
      )
    SectionWithHeader.section1("Columns (resize window)")(
      Row(
        Col.span(12).md(6).lg(4)(cell("xs12 / md6 / lg4")).widget,
        Col.span(12).md(6).lg(4)(cell("xs12 / md6 / lg4")).widget,
        Col.span(12).md(12).lg(4)(cell("xs12 / md12 / lg4")).widget,
      ).widget,
    )
  }

  /** W3-T05: permanent long-string demos for overflow regression visibility. */
  private lazy val overflowTortureSection: Widget = {
    val unbreakable =
      "supercalifragilisticexpialidocious_token_" + ("X" * 80) + "_END"
    SectionWithHeader.section1("Overflow torture")(
      div(
        width := 280.px,
        border(1.px, "solid", S.color.fg.subtle),
        padding := S.spacing._3,
        O.Ellipsis,
        unbreakable,
      ),
      div(height := 10.px),
      div(
        width := 280.px,
        border(1.px, "solid", S.color.fg.subtle),
        padding := S.spacing._3,
        O.WrapText,
        unbreakable,
      ),
      div(height := 10.px),
      Button(unbreakable).small.subtle,
      div(height := 10.px),
      Label("Very long field label that should wrap when the form column is narrow"),
      div(
        width := 280.px,
        O.WrapText,
        "Table-cell-like: " + unbreakable,
      ),
    )
  }

}
