package oxygen.ui.web.create

// TODO (KR) : relocate this
import oxygen.ui.web.style.colorPalette

object OxygenStyleSheet extends StyleSheetBuilder {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Global
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  T.body.apply(
    backgroundColor := OxygenStyleVars.color.bg.default,
    color := OxygenStyleVars.color.fg.default,
    fontSize := OxygenStyleVars.fontSize._3,
  )

  T.h1.apply(
    color := OxygenStyleVars.color.brand.primary2.dark,
    // TODO (KR) :
    paddingLeft := 100.px,
    fontSize := "3rem",
  )

  T.apply("*")
    .apply(
      boxSizing.borderBox,
      fontFamily := S.fontStyle.default,
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
  //      CenteredCardPage
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  object CenteredCardPage extends Class("centered-card-page") { ccp =>

    selector(
      display.flex,
      flexDirection.column,
      justifyContent.center,
      alignItems.center,
    )

    object Card extends ccp.Class("card") {

      selector(
        backgroundColor := OxygenStyleVars.color.bg.layerOne,
        display.flex,
        flexDirection.column,
        alignItems.center,
        justifyContent.center,
        maxHeight := 70.pct,
        minWidth := "min(300px, 100%)",
        maxWidth := 600.px,
        width := 50.pct,
        borderRadius := 40.px,
        padding := 40.px,
      )

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
      scrollbarThumbColor := colorPalette.czr.gray._1000,
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

  object Button extends Class("button") { b => }

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
      backgroundColor := S.color.bg.base.getColorValue.lighten(5.0),
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
    )

    object Modal extends mo.Class("modal") {

      selector(
        cursor.auto,
        borderRadius := S.borderRadius.l,
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

    Table(
      borderCollapse.collapse,
    )

    (
      ((Table & Table.RowBorders) >> T.tr) |
        ((Table & Table.CellBorders) >> (T.th | T.td)) |
        (Table >> Bordered)
    )(
      borderColor := defaultBorderColor,
      borderWidth := defaultBorderWidth,
      borderStyle.solid,
    )

    (Table >> T.th)(
      color := HeaderCellVars.fgColor,
      backgroundColor := HeaderCellVars.bgColor,
      padding := HeaderCellVars.padding,
      textAlign := HeaderCellVars.alignment,
    )

    (Table >> T.td)(
      color := CellCellVars.fgColor,
      backgroundColor := CellCellVars.bgColor,
      padding := CellCellVars.padding,
      textAlign := CellCellVars.alignment,
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
