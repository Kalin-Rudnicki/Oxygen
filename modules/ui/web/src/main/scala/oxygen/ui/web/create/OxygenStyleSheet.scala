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

        selector.hover(
          backgroundColor.red,
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

  object Scrollable extends Class("scrollable") {

    selector(
      overflowY.auto,
    )

    selector.pc(":-webkit-scrollbar")(
      backgroundColor.transparent,
      width := 10.px,
    )

    selector.pc(":-webkit-scrollbar-thumb")(
      backgroundColor := colorPalette.czr.gray._1000,
      borderRadius := 5.px,
    )

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Button
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  object Button extends Class("button") { b =>

    selector(
      borderWidth := 1.px,
      borderStyle.solid,
      borderColor.transparent,
      cursor.pointer,
      userSelect.none,
    )

    /////// Size ///////////////////////////////////////////////////////////////

    object Small extends b.Modifier("small") {

      selector(
        padding(S.spacing._2px, S.spacing._3),
        borderRadius := S.borderRadius._2,
        fontSize := S.fontSize._2,
        fontWeight._600,
      )

    }

    object Medium extends b.Modifier("medium") {

      selector(
        padding(S.spacing._1, S.spacing._4),
        borderRadius := S.borderRadius._4,
        fontSize := S.fontSize._3,
        fontWeight._600,
      )

    }

    object Large extends b.Modifier("large") {

      selector(
        padding(S.spacing._2, S.spacing._8),
        borderRadius := S.borderRadius._5,
        fontSize := S.fontSize._4,
        fontWeight._700,
      )

    }

    /////// Style ///////////////////////////////////////////////////////////////

    // =====| Primary |=====

    object Primary extends b.Modifier("primary") {

      private lazy val baseColor: CSSColor = S.color.primary.getColorValue

      selector(
        borderWidth := 2.px,
        borderColor := S.color.primary.strong,
        backgroundColor := baseColor,
        color := S.color.fg.inverse,
      )

      selector.hover(
        backgroundColor := baseColor.darken(15.0),
      )

      selector.active(
        backgroundColor := baseColor.darken(30.0),
      )

    }

    object PrimarySubtle extends b.Modifier("primary-subtle") {

      private lazy val baseColor: CSSColor = S.color.primary.getColorValue

      selector(
        backgroundColor := baseColor.darken(70.0).setOpacity(30.0),
        color := baseColor,
        borderColor := baseColor,
      )

      selector.hover(
        backgroundColor := baseColor.darken(50.0).setOpacity(30.0),
      )

      selector.active(
        backgroundColor := baseColor.darken(75.0).setOpacity(30.0),
      )

    }

    object PrimaryMinimal extends b.Modifier("primary-minimal") {

      private lazy val baseColor: CSSColor = S.color.primary.getColorValue

      selector(
        backgroundColor := baseColor.darken(70.0).setOpacity(30.0),
        color := baseColor,
      )

      selector.hover(
        backgroundColor := baseColor.darken(50.0).setOpacity(30.0),
        borderColor := baseColor,
      )

      selector.active(
        backgroundColor := baseColor.darken(75.0).setOpacity(30.0),
      )

    }

    // =====| Destructive |=====

    // TODO (KR) : have separate `destructive` color

    object Destructive extends b.Modifier("destructive") {

      private lazy val baseColor: CSSColor = S.color.status.negative.getColorValue

      selector(
        borderWidth := 2.px,
        borderColor := S.color.status.negative.strong,
        backgroundColor := baseColor,
        color := S.color.fg.inverse,
      )

      selector.hover(
        backgroundColor := baseColor.darken(15.0),
      )

      selector.active(
        backgroundColor := baseColor.darken(30.0),
      )

    }

    object DestructiveSubtle extends b.Modifier("destructive-subtle") {

      private lazy val baseColor: CSSColor = S.color.status.negative.getColorValue

      selector(
        backgroundColor := baseColor.darken(70.0).setOpacity(30.0),
        color := baseColor,
        borderColor := baseColor,
      )

      selector.hover(
        backgroundColor := baseColor.darken(50.0).setOpacity(30.0),
      )

      selector.active(
        backgroundColor := baseColor.darken(75.0).setOpacity(30.0),
      )

    }

    object DestructiveMinimal extends b.Modifier("destructive-minimal") {

      private lazy val baseColor: CSSColor = S.color.status.negative.getColorValue

      selector(
        backgroundColor := baseColor.darken(70.0).setOpacity(30.0),
        color := baseColor,
      )

      selector.hover(
        backgroundColor := baseColor.darken(50.0).setOpacity(30.0),
        borderColor := baseColor,
      )

      selector.active(
        backgroundColor := baseColor.darken(75.0).setOpacity(30.0),
      )

    }

    // =====| Positive |=====

    object Positive extends b.Modifier("positive") {

      private lazy val baseColor: CSSColor = S.color.status.positive.getColorValue

      selector(
        borderWidth := 2.px,
        borderColor := S.color.status.positive.strong,
        backgroundColor := baseColor,
        color := S.color.fg.inverse,
      )

      selector.hover(
        backgroundColor := baseColor.darken(15.0),
      )

      selector.active(
        backgroundColor := baseColor.darken(30.0),
      )

    }

    object PositiveSubtle extends b.Modifier("positive-subtle") {

      private lazy val baseColor: CSSColor = S.color.status.positive.getColorValue

      selector(
        backgroundColor := baseColor.darken(70.0).setOpacity(30.0),
        color := baseColor,
        borderColor := baseColor,
      )

      selector.hover(
        backgroundColor := baseColor.darken(50.0).setOpacity(30.0),
      )

      selector.active(
        backgroundColor := baseColor.darken(75.0).setOpacity(30.0),
      )

    }

    object PositiveMinimal extends b.Modifier("positive-minimal") {

      private lazy val baseColor: CSSColor = S.color.status.positive.getColorValue

      selector(
        backgroundColor := baseColor.darken(70.0).setOpacity(30.0),
        color := baseColor,
      )

      selector.hover(
        backgroundColor := baseColor.darken(50.0).setOpacity(30.0),
        borderColor := baseColor,
      )

      selector.active(
        backgroundColor := baseColor.darken(75.0).setOpacity(30.0),
      )

    }

    // =====| Negative |=====

    object Negative extends b.Modifier("negative") {

      private lazy val baseColor: CSSColor = S.color.status.negative.getColorValue

      selector(
        borderWidth := 2.px,
        borderColor := S.color.status.negative.strong,
        backgroundColor := baseColor,
        color := S.color.fg.inverse,
      )

      selector.hover(
        backgroundColor := baseColor.darken(15.0),
      )

      selector.active(
        backgroundColor := baseColor.darken(30.0),
      )

    }

    object NegativeSubtle extends b.Modifier("negative-subtle") {

      private lazy val baseColor: CSSColor = S.color.status.negative.getColorValue

      selector(
        backgroundColor := baseColor.darken(70.0).setOpacity(30.0),
        color := baseColor,
        borderColor := baseColor,
      )

      selector.hover(
        backgroundColor := baseColor.darken(50.0).setOpacity(30.0),
      )

      selector.active(
        backgroundColor := baseColor.darken(75.0).setOpacity(30.0),
      )

    }

    object NegativeMinimal extends b.Modifier("negative-minimal") {

      private lazy val baseColor: CSSColor = S.color.status.negative.getColorValue

      selector(
        backgroundColor := baseColor.darken(70.0).setOpacity(30.0),
        color := baseColor,
      )

      selector.hover(
        backgroundColor := baseColor.darken(50.0).setOpacity(30.0),
        borderColor := baseColor,
      )

      selector.active(
        backgroundColor := baseColor.darken(75.0).setOpacity(30.0),
      )

    }

    // =====| Alert |=====

    object Alert extends b.Modifier("alert") {

      private lazy val baseColor: CSSColor = S.color.status.alert.getColorValue

      selector(
        borderWidth := 2.px,
        borderColor := S.color.status.alert.strong,
        backgroundColor := baseColor,
        color := S.color.fg.inverse,
      )

      selector.hover(
        backgroundColor := baseColor.darken(15.0),
      )

      selector.active(
        backgroundColor := baseColor.darken(30.0),
      )

    }

    object AlertSubtle extends b.Modifier("alert-subtle") {

      private lazy val baseColor: CSSColor = S.color.status.alert.getColorValue

      selector(
        backgroundColor := baseColor.darken(70.0).setOpacity(30.0),
        color := baseColor,
        borderColor := baseColor,
      )

      selector.hover(
        backgroundColor := baseColor.darken(50.0).setOpacity(30.0),
      )

      selector.active(
        backgroundColor := baseColor.darken(75.0).setOpacity(30.0),
      )

    }

    object AlertMinimal extends b.Modifier("alert-minimal") {

      private lazy val baseColor: CSSColor = S.color.status.alert.getColorValue

      selector(
        backgroundColor := baseColor.darken(70.0).setOpacity(30.0),
        color := baseColor,
      )

      selector.hover(
        backgroundColor := baseColor.darken(50.0).setOpacity(30.0),
        borderColor := baseColor,
      )

      selector.active(
        backgroundColor := baseColor.darken(75.0).setOpacity(30.0),
      )

    }

    // =====| Info |=====

    object Info extends b.Modifier("info") {

      private lazy val baseColor: CSSColor = S.color.status.informational.getColorValue

      selector(
        borderWidth := 2.px,
        borderColor := S.color.status.informational.strong,
        backgroundColor := baseColor,
        color := S.color.fg.inverse,
      )

      selector.hover(
        backgroundColor := baseColor.darken(15.0),
      )

      selector.active(
        backgroundColor := baseColor.darken(30.0),
      )

    }

    object InfoSubtle extends b.Modifier("info-subtle") {

      private lazy val baseColor: CSSColor = S.color.status.informational.getColorValue

      selector(
        backgroundColor := baseColor.darken(70.0).setOpacity(30.0),
        color := baseColor,
        borderColor := baseColor,
      )

      selector.hover(
        backgroundColor := baseColor.darken(50.0).setOpacity(30.0),
      )

      selector.active(
        backgroundColor := baseColor.darken(75.0).setOpacity(30.0),
      )

    }

    object InfoMinimal extends b.Modifier("info-minimal") {

      private lazy val baseColor: CSSColor = S.color.status.informational.getColorValue

      selector(
        backgroundColor := baseColor.darken(70.0).setOpacity(30.0),
        color := baseColor,
      )

      selector.hover(
        backgroundColor := baseColor.darken(50.0).setOpacity(30.0),
        borderColor := baseColor,
      )

      selector.active(
        backgroundColor := baseColor.darken(75.0).setOpacity(30.0),
      )

    }

    // =====| Disabled |=====

    object Disabled extends b.Modifier("disabled") {

      selector(
        backgroundColor := "#0004",
        color := S.color.fg.subtle,
        cursor.notAllowed,
      )

    }

    object DisabledProgress extends b.Modifier("disabled-progress") {

      selector(
        backgroundColor := "#0004",
        color := S.color.fg.subtle,
        cursor.progress,
      )

    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      ToggleThumb
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  object ToggleThumb extends Class("toggle-thumb") { tt =>

    private object sizes {
      abstract class Size(val trackHeight: Int, val trackWidth: Int, val thumbPadding: Int) {
        val thumbSpacing: Int = thumbPadding - 1
        val thumbSize: Int = trackHeight - 2 * thumbPadding
        val translation: Int = trackWidth - trackHeight
        // TODO (KR) :
      }

      object small extends Size(15, 30, 3)
      object medium extends Size(20, 40, 3)
      object large extends Size(30, 60, 3)
    }

    selector(
      display.inlineBlock,
      position.relative,
      cursor.pointer,
      userSelect.none,
      border.csss(2.px, "solid", S.color.fg.inverse),
      transition := "background-color 0.4s",
    )

    (ToggleThumb & ToggleThumb.Small)(
      height := sizes.small.trackHeight.px,
      width := sizes.small.trackWidth.px,
      borderRadius := sizes.small.trackHeight.px,
    )

    (ToggleThumb & ToggleThumb.Medium)(
      height := sizes.medium.trackHeight.px,
      width := sizes.medium.trackWidth.px,
      borderRadius := sizes.medium.trackHeight.px,
    )

    (ToggleThumb & ToggleThumb.Large)(
      height := sizes.large.trackHeight.px,
      width := sizes.large.trackWidth.px,
      borderRadius := sizes.large.trackHeight.px,
    )

    /////// Thumb ///////////////////////////////////////////////////////////////

    object Thumb extends tt.Class("thumb") {

      Thumb(
        position.absolute,
        border.csss(1.px, "solid", S.color.fg.inverse),
        borderRadius := 50.pct,
        backgroundColor := S.color.bg.base.getColorValue.lighten(5.0),
        transition := "transform 0.4s",
      )

      (Small >> Thumb)(
        top := sizes.small.thumbSpacing.px,
        left := sizes.small.thumbSpacing.px,
        height := sizes.small.thumbSize.px,
        width := sizes.small.thumbSize.px,
      )

      (Medium >> Thumb)(
        top := sizes.medium.thumbSpacing.px,
        left := sizes.medium.thumbSpacing.px,
        height := sizes.medium.thumbSize.px,
        width := sizes.medium.thumbSize.px,
      )

      (Large >> Thumb)(
        top := sizes.large.thumbSpacing.px,
        left := sizes.large.thumbSpacing.px,
        height := sizes.large.thumbSize.px,
        width := sizes.large.thumbSize.px,
      )

      ((Small & Enabled) >> Thumb)(
        transform := s"translateX(${sizes.small.translation.px})",
      )

      ((Medium & Enabled) >> Thumb)(
        transform := s"translateX(${sizes.medium.translation.px})",
      )

      ((Large & Enabled) >> Thumb)(
        transform := s"translateX(${sizes.large.translation.px})",
      )

    }

    /////// Enabled/Disabled ///////////////////////////////////////////////////////////////

    object Enabled extends tt.Modifier("enabled")

    object Disabled extends tt.Modifier("disabled")

    /////// Size ///////////////////////////////////////////////////////////////

    object Small extends tt.Modifier("small")
    object Medium extends tt.Modifier("medium")
    object Large extends tt.Modifier("large")

    /////// Style ///////////////////////////////////////////////////////////////

    object Primary extends tt.Modifier("primary") {

      selector(
        backgroundColor := S.color.primary,
      )

    }

    object Positive extends tt.Modifier("positive") {

      selector(
        backgroundColor := S.color.status.positive,
      )

    }

    object Negative extends tt.Modifier("negative") {

      selector(
        backgroundColor := S.color.status.negative,
      )

    }

    object Alert extends tt.Modifier("alert") {

      selector(
        backgroundColor := S.color.status.alert,
      )

    }

    object Info extends tt.Modifier("info") {

      selector(
        backgroundColor := S.color.status.informational,
      )

    }

    object BrandPrimary1 extends tt.Modifier("brand-primary-1") {

      selector(
        backgroundColor := S.color.brand.primary1,
      )

    }

    object BrandPrimary2 extends tt.Modifier("brand-primary-2") {

      selector(
        backgroundColor := S.color.brand.primary2,
      )

    }

    object Off extends tt.Modifier("off") {

      selector(
        backgroundColor := S.color.bg.base.getColorValue.lighten(15.0).setOpacity(60.0),
      )

    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      HorizontalRadio
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  object HorizontalRadio extends Class("horizontal-radio") { tt =>

    private object sizes {
      abstract class Size(val radius: CSSVar)

      object small extends Size(S.borderRadius._3)
      object medium extends Size(S.borderRadius._4)
      object large extends Size(S.borderRadius._5)
    }

    HorizontalRadio(
      display.inlineBlock,
      margin(S.spacing._2, S.spacing._0),
      cursor.pointer,
      userSelect.none,
      borderWidth := 2.px,
      borderStyle.solid,
    )

    (HorizontalRadio & HorizontalRadio.Small)(
      borderRadius := sizes.small.radius,
    )
    (HorizontalRadio & HorizontalRadio.Medium)(
      borderRadius := sizes.medium.radius,
    )
    (HorizontalRadio & HorizontalRadio.Large)(
      borderRadius := sizes.large.radius,
    )

    Button(
      display.inlineBlock,
      margin := "0",
      borderStyle.none,
      fontWeight._500,
      borderRightWidth := 2.px,
      borderRightStyle.solid,
    )
    (Button & Button.Last)(
      borderRightStyle.none,
    )

    ((HorizontalRadio & Small) >> Button)(
      fontSize := S.fontSize._2,
      padding(S.spacing._1, S.spacing._3),
    )
    ((HorizontalRadio & Small) >> Button.First)(
      borderTopLeftRadius := sizes.small.radius,
      borderBottomLeftRadius := sizes.small.radius,
    )
    ((HorizontalRadio & Small) >> Button.Last)(
      borderTopRightRadius := sizes.small.radius,
      borderBottomRightRadius := sizes.small.radius,
    )

    ((HorizontalRadio & Medium) >> Button)(
      fontSize := S.fontSize._4,
      padding(s"calc(${S.spacing._1.varString} * 1.5)", S.spacing._4),
    )
    ((HorizontalRadio & Medium) >> Button.First)(
      borderTopLeftRadius := sizes.medium.radius,
      borderBottomLeftRadius := sizes.medium.radius,
    )
    ((HorizontalRadio & Medium) >> Button.Last)(
      borderTopRightRadius := sizes.medium.radius,
      borderBottomRightRadius := sizes.medium.radius,
    )

    ((HorizontalRadio & Large) >> Button)(
      fontSize := S.fontSize._6,
      padding(S.spacing._2, S.spacing._5),
    )
    ((HorizontalRadio & Large) >> Button.First)(
      borderTopLeftRadius := sizes.large.radius,
      borderBottomLeftRadius := sizes.large.radius,
    )
    ((HorizontalRadio & Large) >> Button.Last)(
      borderTopRightRadius := sizes.large.radius,
      borderBottomRightRadius := sizes.large.radius,
    )

    /////// Button ///////////////////////////////////////////////////////////////

    object Button extends tt.Class("button") { b =>

      object First extends b.Modifier("first")
      object Last extends b.Modifier("last")

    }

    /////// Size ///////////////////////////////////////////////////////////////

    object Small extends tt.Modifier("small")
    object Medium extends tt.Modifier("medium")
    object Large extends tt.Modifier("large")

    /////// Style ///////////////////////////////////////////////////////////////

    object Primary extends tt.Modifier("primary") {

      selector(
        backgroundColor := S.color.primary,
        color := S.color.fg.inverse,
      )

      selector.hover(
        backgroundColor := S.color.primary.getColorValue.lighten(40.0),
      )

    }

    object Positive extends tt.Modifier("positive") {

      selector(
        backgroundColor := S.color.status.positive,
        color := S.color.fg.inverse,
      )

      selector.hover(
        backgroundColor := S.color.status.positive.getColorValue.lighten(40.0),
      )

    }

    object Negative extends tt.Modifier("negative") {

      selector(
        backgroundColor := S.color.status.negative,
        color := S.color.fg.inverse,
      )

      selector.hover(
        backgroundColor := S.color.status.negative.getColorValue.lighten(40.0),
      )

    }

    object Alert extends tt.Modifier("alert") {

      selector(
        backgroundColor := S.color.status.alert,
        color := S.color.fg.inverse,
      )

      selector.hover(
        backgroundColor := S.color.status.alert.getColorValue.lighten(40.0),
      )

    }

    object Info extends tt.Modifier("info") {

      selector(
        backgroundColor := S.color.status.informational,
        color := S.color.fg.inverse,
      )

      selector.hover(
        backgroundColor := S.color.status.informational.getColorValue.lighten(40.0),
      )

    }

    object BrandPrimary1 extends tt.Modifier("brand-primary-1") {

      selector(
        backgroundColor := S.color.brand.primary1,
      )

      selector.hover(
        backgroundColor := S.color.brand.primary1.getColorValue.lighten(40.0),
      )

    }

    object BrandPrimary2 extends tt.Modifier("brand-primary-2") {

      selector(
        backgroundColor := S.color.brand.primary2,
      )

      selector.hover(
        backgroundColor := S.color.brand.primary2.getColorValue.lighten(40.0),
      )

    }

    object Off extends tt.Modifier("off") {

      selector(
        backgroundColor := S.color.bg.base.getColorValue.lighten(15.0).setOpacity(60.0),
      )

      selector.hover(
        backgroundColor := S.color.bg.base.getColorValue.lighten(40.0).setOpacity(60.0),
      )

    }

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

    object Expanded extends dd.Modifier("expanded")
    object Display extends dd.Class("display")
    object Options extends dd.Class("options") { opts =>
      object Option extends opts.Class("option") { o =>
        object Selected extends o.Modifier("selected")
        object First extends o.Modifier("first")
        object Last extends o.Modifier("last")
      }
    }

    object Primary extends dd.Modifier("primary")
    object Positive extends dd.Modifier("positive")
    object Negative extends dd.Modifier("negative")
    object Alert extends dd.Modifier("alert")
    object Info extends dd.Modifier("info")
    object BrandPrimary1 extends dd.Modifier("brand-primary-1")
    object BrandPrimary2 extends dd.Modifier("brand-primary-2")

    Dropdown(
      display.inlineBlock,
      position.relative,
      width := 100.pct,
      margin(S.spacing._2, S.spacing._0),
      boxSizing.borderBox,
    )

    (Dropdown >> Dropdown.Display)(
      padding(S.spacing._1, S.spacing._2),
      borderRadius := S.borderRadius._4,
      cursor.pointer,
      userSelect.none,
      width := 100.pct,
      color.black, // TODO (KR) :
      backgroundColor.white, // TODO (KR) :
      boxSizing.borderBox,
    )

    ((Dropdown & Dropdown.Expanded) >> Dropdown.Display)(
      borderBottomLeftRadius := S.borderRadius._0,
      borderBottomRightRadius := S.borderRadius._0,
    )

    (Dropdown >> Dropdown.Options)(
      display.none,
      position.absolute,
      zIndex := "100",
      width := 100.pct,
      boxSizing.borderBox,
    )
    ((Dropdown & Dropdown.Expanded) >> Dropdown.Options)(
      display.block,
    )

    Dropdown.Options.Option(
      borderWidth := 1.px,
      padding(S.spacing._1, S.spacing._3),
      borderColor := S.color.fg.moderate,
      borderStyle.solid,
      borderBottomStyle.none,
      cursor.pointer,
      userSelect.none,
      width := 100.pct,
      color.black, // TODO (KR) :
      backgroundColor := "#DDD", // TODO (KR) :
      boxSizing.borderBox,
    )

    (Dropdown.Options.Option & Dropdown.Options.Option.Last)(
      borderBottomStyle.solid,
      borderBottomLeftRadius := S.borderRadius._2,
      borderBottomRightRadius := S.borderRadius._2,
    )

    (Dropdown.Primary >> Dropdown.Options.Option.Selected)(
      backgroundColor := S.color.primary,
    )
    (Dropdown.Primary >> Dropdown.Options.Option.hover)(
      backgroundColor := S.color.primary.getColorValue.lighten(30.0),
    )

    (Dropdown.Positive >> Dropdown.Options.Option.Selected)(
      backgroundColor := S.color.status.positive,
    )
    (Dropdown.Positive >> Dropdown.Options.Option.hover)(
      backgroundColor := S.color.status.positive.getColorValue.lighten(30.0),
    )

    (Dropdown.Negative >> Dropdown.Options.Option.Selected)(
      backgroundColor := S.color.status.negative,
    )
    (Dropdown.Negative >> Dropdown.Options.Option.hover)(
      backgroundColor := S.color.status.negative.getColorValue.lighten(30.0),
    )

    (Dropdown.Alert >> Dropdown.Options.Option.Selected)(
      backgroundColor := S.color.status.alert,
    )
    (Dropdown.Alert >> Dropdown.Options.Option.hover)(
      backgroundColor := S.color.status.alert.getColorValue.lighten(30.0),
    )

    (Dropdown.Info >> Dropdown.Options.Option.Selected)(
      backgroundColor := S.color.status.informational,
    )
    (Dropdown.Info >> Dropdown.Options.Option.hover)(
      backgroundColor := S.color.status.informational.getColorValue.lighten(30.0),
    )

    (Dropdown.BrandPrimary1 >> Dropdown.Options.Option.Selected)(
      backgroundColor := S.color.brand.primary1,
    )
    (Dropdown.BrandPrimary1 >> Dropdown.Options.Option.hover)(
      backgroundColor := S.color.brand.primary1.getColorValue.lighten(30.0),
    )

    (Dropdown.BrandPrimary2 >> Dropdown.Options.Option.Selected)(
      backgroundColor := S.color.brand.primary2,
    )
    (Dropdown.BrandPrimary2 >> Dropdown.Options.Option.hover)(
      backgroundColor := S.color.brand.primary2.getColorValue.lighten(30.0),
    )

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Compiled
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  override val compiled: StyleSheet = StyleSheet.derived[OxygenStyleSheet.type]

}

val O: OxygenStyleSheet.type = OxygenStyleSheet
