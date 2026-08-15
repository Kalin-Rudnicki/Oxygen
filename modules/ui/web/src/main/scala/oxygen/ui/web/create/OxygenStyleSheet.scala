package oxygen.ui.web.create

object OxygenStyleSheet extends StyleSheetBuilder {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Global
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  // Root: rem basis + stop iOS text inflation games. Viewport meta is separate
  // (PageHtmlResponse / PageApp.ensureViewportMeta) — without it phones still look desktop-y.
  T.html.apply(
    fontSize := "16px",
    Widget.raw.css("-webkit-text-size-adjust", "100%"),
    Widget.raw.css("text-size-adjust", "100%"),
  )

  T.body.apply(
    backgroundColor := OxygenStyleVars.color.bg.default,
    color := OxygenStyleVars.color.fg.default,
    fontSize := OxygenStyleVars.fontSize._3,
    lineHeight := "1.5",
    // safe areas for notched phones
    Widget.raw.css("padding-left", "env(safe-area-inset-left)"),
    Widget.raw.css("padding-right", "env(safe-area-inset-right)"),
  )

  T.h1.apply(
    color := OxygenStyleVars.color.fg.default,
    fontSize := "1.75rem",
    fontWeight := OxygenStyleVars.fontWeight.bold,
    marginTop := "0",
  )

  // OXY-158: composable media queries — trim the h1 on narrow (phone) viewports.
  // Written with the ordinary selector DSL; the `@media (max-width: …)` wrapper is emitted for us.
  media(MediaQuery.belowMd) {
    T.h1.apply(
      fontSize := "1.5rem",
    )
  }

  T.apply("*")
    .apply(
      boxSizing.borderBox,
      fontFamily := S.fontStyle.default,
    )

  // W3-T01: flex/grid children shrink by default (classic long-text overflow fix)
  T.apply("*, *::before, *::after").apply(
    minWidth := 0,
    minHeight := 0,
  )

  // Helpers applied via classes on widgets that need deliberate overflow policy
  object Ellipsis extends Class("oxy-ellipsis") {
    selector(
      overflow.hidden,
      textOverflow := "ellipsis",
      whiteSpace.nowrap,
      maxWidth := 100.pct,
    )
  }

  object WrapText extends Class("oxy-wrap") {
    selector(
      whiteSpace := "normal",
      wordBreak := "break-word",
      maxWidth := 100.pct,
    )
  }

  object ClipX extends Class("oxy-clip-x") {
    selector(
      overflowX.hidden,
      maxWidth := 100.pct,
    )
  }

  // W3-T04: default field chrome + focus ring.
  // layerTwo inset so fields contrast on Section.level1 (layerOne) cards.
  T.apply("input, textarea").apply(
    transition := "border-color 0.15s ease, box-shadow 0.15s ease, background-color 0.15s ease",
    border := s"1px solid ${S.color.fg.subtle}",
    color := S.color.fg.default,
    backgroundColor := S.color.bg.layerTwo,
  )

  T.apply("input:focus-visible, textarea:focus-visible").apply(
    outline := s"2px solid ${S.color.fg.focus}",
    Widget.raw.css("outline-offset", "1px"),
    borderColor := S.color.fg.focus,
  )

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      NavBar
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  object NavBar extends Class("nav-bar") { nb =>

    selector(
      width := 100.vw,
      flex := "0 1 auto",
      display.flex,
      flexDirection.row,
      flexWrap.nowrap,
      height := "max-content",
      // alignItems.stretch,
    )

    object Section extends nb.Class("section") { s =>

      selector(
        display.flex,
        flexWrap.wrap,
        height := "fit-content",
        alignItems := "end",
      )

      object Shrink extends s.Modifier("shrink") {

        selector(
          flex := "0 1 auto",
        )

      }

      object Expand extends s.Modifier("expand") {

        selector(
          flex := "1 0 auto",
        )

      }

      object Elem extends s.Class("elem") {

        selector(
          cursor.pointer,
          userSelect.none,
          display.inlineFlex,
          justifyContent.center,
          alignItems.center,
          padding := "0 1rem",
          fontSize := OxygenStyleVars.fontSize._5,
        )

      }

    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Scrollable
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  object Scrollable extends Class("scrollable") { s =>

    val scrollbarColor: CSSVar = s.cssVar("scrollbar-color")
    val scrollbarWidth: CSSVar = s.cssVar("scrollbar-width")
    val scrollbarThumbColor: CSSVar = s.cssVar("scrollbar-thumb-color")
    val thumbRadius: CSSVar = s.cssVar("scrollbar-thumb-radius")
    val scrollbarBottomRightRadius: CSSVar = s.cssVar("scrollbar-bottom-right-radius")

    Scrollable(
      overflowY.auto,
      scrollbarColor := "transparent",
      scrollbarThumbColor := S.color.fg.subtle,
      scrollbarWidth := 10.px,
      thumbRadius := 5.px,
      scrollbarBottomRightRadius := "0",
    )

    Scrollable.pc(":-webkit-scrollbar")(
      backgroundColor := scrollbarColor,
      width := scrollbarWidth,
      borderBottomRightRadius := scrollbarBottomRightRadius,
    )
    Scrollable.pc(":-webkit-scrollbar-corner")(
      backgroundColor := scrollbarColor,
    )

    Scrollable.pc(":-webkit-scrollbar-thumb")(
      backgroundColor := scrollbarThumbColor,
      borderRadius := thumbRadius,
    )

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Button
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  object Button extends Class("button") { b =>

    // W3-T04: modern chrome; colors still come from widget intent vars.
    // No margin — parents own spacing (flex gap, etc.). Global margin fought layouts.
    Button(
      transition := "background-color 0.15s ease, border-color 0.15s ease, color 0.15s ease, box-shadow 0.15s ease",
      userSelect.none,
      lineHeight := "1.25",
      letterSpacing := "0.01em",
      margin := "0",
    )

    Button.pc(":focus-visible")(
      outline := s"2px solid ${S.color.fg.focus}",
      Widget.raw.css("outline-offset", "2px"),
    )

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      ToggleThumb
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  object ToggleThumb extends Class("toggle-thumb") { tt =>

    Track(
      display.inlineBlock,
      position.relative,
      cursor.pointer,
      userSelect.none,
      borderStyle.solid,
      borderColor := S.color.fg.inverse,
      transition := "background-color 0.4s",
    )

    Thumb(
      position.absolute,
      borderStyle.solid,
      borderColor := S.color.fg.inverse,
      borderRadius := 50.pct,
      backgroundColor := S.color.bg.default,
      transition := "transform 0.4s",
    )

    object Track extends tt.Class("track")
    object Thumb extends tt.Class("thumb")

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      HorizontalRadio
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  object HorizontalRadio extends Class("horizontal-radio") { hr =>

    HorizontalRadio(
      display.inlineBlock,
      cursor.pointer,
      userSelect.none,
    )

    HorizontalRadio.Button(
      display.inlineBlock,
      margin := "0",
      fontWeight := S.fontWeight.medium,
    )

    object Button extends hr.Class("button")

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Modal
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  object ModalOverlay extends Class("modal-overlay") { mo =>

    selector(
      position.fixed,
      top := 0,
      left := 0,
      width := 100.vw,
      height := 100.vh,
      display.flex,
      justifyContent.center,
      alignItems.center,
      cursor.pointer,
      zIndex := ZIndices.modalBehindPageMessages,
      // W7-T07: light enter motion (durations zero under prefers-reduced-motion via Motion.sheet)
      animation := "oxy-fade-in var(--oxy-motion-duration-fast) var(--oxy-motion-easing-enter) both",
    )

    object Modal extends mo.Class("modal") {

      selector(
        cursor.auto,
        borderRadius := S.borderRadius.l,
        animation := "oxy-slide-up var(--oxy-motion-duration-normal) var(--oxy-motion-easing-enter) both",
      )

    }

    object AbovePageMessages extends mo.Modifier("above-page-messages") {

      selector(
        zIndex := ZIndices.modalInFrontOfPageMessages,
      )

    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Dropdown
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  object Dropdown extends Class("dropdown") { dd =>

    Dropdown(
      display.inlineBlock,
      position.relative,
      overflowY.visible,
    )

    Dropdown.Display(
      width := 100.pct,
      cursor.pointer,
      userSelect.none,
    )

    (Dropdown >> Dropdown.Options)(
      width := 100.pct,
      display.none,
      position.absolute,
      zIndex := "100",
    )
    ((Dropdown & Dropdown.Expanded) >> Dropdown.Options)(
      display.block,
      // W7-T07: subtle open animation
      animation := "oxy-fade-in var(--oxy-motion-duration-fast) var(--oxy-motion-easing-enter) both",
    )

    Dropdown.Options.Option(
      width := 100.pct,
      cursor.pointer,
      userSelect.none,
    )

    object Expanded extends dd.Modifier("expanded")
    object Display extends dd.Class("display")
    object Options extends dd.Class("options") { opts =>
      object Option extends opts.Class("option") { o =>
        object Selected extends o.Modifier("selected")
        object First extends o.Modifier("first")
        object Last extends o.Modifier("last")
      }
    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Table
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  object Table extends Class("table") { tab =>

    object Bordered extends tab.Modifier("bordered")
    object HeaderCellVars extends CellVars("header")
    object CellCellVars extends CellVars("cell")

    object RowBorders extends tab.Modifier("row-borders")
    object CellBorders extends tab.Modifier("cell-borders")

    abstract class CellVars(n: String) extends tab.Class(s"$n-vars") { vars =>
      val fgColor: CSSVar = vars.cssVar("fg-color")
      val bgColor: CSSVar = vars.cssVar("bg-color")
      val padding: CSSVar = vars.cssVar("padding")
      val alignment: CSSVar = vars.cssVar("alignment")
    }

    val defaultBorderColor: CSSVar = tab.cssVar("border-color")
    val defaultBorderWidth: CSSVar = tab.cssVar("border-width")

    ///////  ///////////////////////////////////////////////////////////////

    // Collapse keeps shared cell borders. Radius/overflow do NOT belong on <table>
    // (collapse ignores radius; overflow:hidden clips border paint at corners).
    // Clip + outer edge live on [[Shell]] (Table Deferred outer div).
    Table(
      borderCollapse.collapse,
      width := 100.pct,
    )

    /**
      * Outer clip wrapper class. Radius / border / overflow are set inline in
      * [[oxygen.ui.web.component.Table]] build. [[OuterBorder]] marks when the shell
      * owns the perimeter so cell rules can drop outer edges (no double stroke).
      */
    object Shell extends tab.Class("shell") { sh =>
      object OuterBorder extends sh.Modifier("outer-border")

      Shell(
        display.block,
        maxWidth := 100.pct,
      )
    }

    // Borders live on th/td only — never on tr (tr full-box borders double the shell perimeter).
    // Cell mode: full grid on every cell.
    (
      ((Table & Table.CellBorders) >> (T.th | T.td)) |
        (Table >> Bordered)
    )(
      borderColor := defaultBorderColor,
      borderWidth := defaultBorderWidth,
      borderStyle.solid,
    )

    // Row mode: horizontal rules only (bottom edge of each cell).
    ((Table & Table.RowBorders) >> (T.th | T.td))(
      borderColor := defaultBorderColor,
      borderWidth := defaultBorderWidth,
      borderStyle.solid,
      borderLeftWidth := 0.px,
      borderRightWidth := 0.px,
      borderTopWidth := 0.px,
    )

    // Shell owns the outer edge → suppress cell edges that would double-paint with it.
    // Row mode: last body row bottom would sit under the shell bottom.
    ((Shell & Shell.OuterBorder) >> (Table & Table.RowBorders) >> T.tbody >> T.tr.pc("last-child") >> (T.th | T.td))(
      borderBottomWidth := 0.px,
    )
    // Also if rows are direct children of table (no tbody wrapper).
    ((Shell & Shell.OuterBorder) >> (Table & Table.RowBorders) > T.tr.pc("last-child") >> (T.th | T.td))(
      borderBottomWidth := 0.px,
    )

    // Cell mode: drop the four outer sides of the grid.
    ((Shell & Shell.OuterBorder) >> (Table & Table.CellBorders) >> T.thead.pc("first-child") >> T.tr.pc("first-child") >> (T.th | T.td))(
      borderTopWidth := 0.px,
    )
    ((Shell & Shell.OuterBorder) >> (Table & Table.CellBorders) >> T.tbody.pc("first-child") >> T.tr.pc("first-child") >> (T.th | T.td))(
      borderTopWidth := 0.px, // no thead case
    )
    ((Shell & Shell.OuterBorder) >> (Table & Table.CellBorders) >> T.tbody.pc("last-child") >> T.tr.pc("last-child") >> (T.th | T.td))(
      borderBottomWidth := 0.px,
    )
    ((Shell & Shell.OuterBorder) >> (Table & Table.CellBorders) >> T.tr >> (T.th | T.td).pc("first-child"))(
      borderLeftWidth := 0.px,
    )
    ((Shell & Shell.OuterBorder) >> (Table & Table.CellBorders) >> T.tr >> (T.th | T.td).pc("last-child"))(
      borderRightWidth := 0.px,
    )

    (Table >> T.th)(
      color := HeaderCellVars.fgColor,
      backgroundColor := HeaderCellVars.bgColor,
      padding := HeaderCellVars.padding,
      textAlign := HeaderCellVars.alignment,
      fontWeight := "600",
      fontSize := S.fontSize._2,
      letterSpacing := "0.02em",
      textTransform := "uppercase",
    )

    (Table >> T.td)(
      color := CellCellVars.fgColor,
      backgroundColor := CellCellVars.bgColor,
      padding := CellCellVars.padding,
      textAlign := CellCellVars.alignment,
      fontSize := S.fontSize._3,
    )

    // subtle hover on body rows (header excluded via tbody)
    (Table >> T.tbody >> T.tr.hover >> T.td)(
      backgroundColor := S.color.bg.layerTwo,
    )

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Label
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  object Label extends Class("label") { l =>
    object LabelText extends l.Class("label-text")
    object DescriptionText extends l.Class("description-text")
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Compiled
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  override val compiled: StyleSheet = StyleSheet.derived[OxygenStyleSheet.type]

}

val O: OxygenStyleSheet.type = OxygenStyleSheet
