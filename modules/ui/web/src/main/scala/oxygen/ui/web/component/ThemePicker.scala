package oxygen.ui.web.component

import org.scalajs.dom.window
import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}
import oxygen.ui.web.service.Theme
import oxygen.ui.web.style.OxygenThemes
import oxygen.ui.web.style.OxygenThemes.Pack

/**
  * Reusable, drop-in theme-pack picker. Renders [[OxygenThemes.all]] (or a filtered `packs` list)
  * as selectable cards (swatch + name + blurb + selected state) and calls [[Theme.applyAndPersist]]
  * on select.
  *
  * Self-contained: backed by a shared [[GlobalState]] seeded from the stored / current pack, so it
  * reflects the active pack and can be dropped into any page without wiring page state.
  *
  * a11y: `role=radiogroup` container, `role=radio` + `aria-checked` per card.
  *
  * {{{
  * ThemePicker()                                      // all packs
  * ThemePicker(OxygenThemes.graphiteFamilyPacks)      // graphite family only
  * }}}
  *
  * Known limitation (by design, OXY-154): the highlight does not live-update on cross-tab /
  * programmatic pack changes until the page re-renders. Cross-tab *application* still works via
  * `ColorTheme.install` / `Theme.subscribeCrossTab`.
  */
object ThemePicker {

  private def readStored: String =
    Option(window.localStorage.getItem(Theme.storageKey))
      .flatMap(OxygenThemes.parse)
      .getOrElse(OxygenThemes.default)
      .id

  private object PackState extends GlobalState[String]("ThemePicker")(readStored)

  private def swatch(label: String, hex: String): Widget =
    div(
      display.flex,
      flexDirection.column,
      alignItems.center,
      gap := S.spacing._1,
      div(
        width := 40.px,
        height := 40.px,
        borderRadius := S.borderRadius._3,
        backgroundColor := hex,
        border(1.px, "solid", S.color.bg.layerThree),
      ),
      span(fontSize := S.fontSize._1, color := S.color.fg.subtle, label),
      span(fontSize := S.fontSize._1, color := S.color.fg.minimal, hex),
    )

  private def card(st: WidgetState[String], pack: Pack, activeId: String): Widget = {
    val on: Boolean = pack.id == activeId
    div(
      Widget.raw.htmlAttr("role", "radio"),
      Widget.raw.htmlAttr("aria-checked", if on then "true" else "false"),
      Widget.raw.htmlAttr("aria-label", pack.name),
      display.flex,
      flexDirection.column,
      gap := S.spacing._3,
      padding := S.spacing._4,
      borderRadius := S.borderRadius._4,
      border(2.px, "solid", if on then S.color.primary.standard else S.color.bg.layerThree),
      backgroundColor := S.color.bg.layerOne,
      // header row: name/blurb + apply button
      div(
        display.flex,
        alignItems.center,
        justifyContent.spaceBetween,
        gap := S.spacing._3,
        flexWrap.wrap,
        div(
          div(fontWeight := S.fontWeight.bold, fontSize := S.fontSize._5, color := S.color.fg.default, pack.name),
          div(fontSize := S.fontSize._2, color := S.color.fg.moderate, marginTop := S.spacing._1, pack.blurb),
        ), {
          val label = if on then "Selected" else "Use this theme"
          val base = Button(label).small
          val btn = if on then base.primary else base.subtle
          btn.content(onClick := (Theme.applyAndPersist(pack) *> st.set(pack.id)))
        },
      ),
      // swatches
      div(
        display.flex,
        gap := S.spacing._2,
        flexWrap.wrap,
        swatch("Primary", pack.primarySwatch),
        swatch("Accent", pack.accentSwatch),
        swatch("BG", pack.bgSwatch),
        swatch("Danger", pack.dark.danger),
        swatch("Success", pack.dark.success),
      ),
    )
  }

  /** Card list for the given packs (defaults to all Oxygen theme packs). */
  def apply(packs: Seq[Pack] = OxygenThemes.all): Widget =
    Widget.state[String].detach(PackState) { st =>
      val activeId: String = st.renderTimeValue
      div(
        Widget.raw.htmlAttr("role", "radiogroup"),
        Widget.raw.htmlAttr("aria-label", "Theme pack"),
        display.flex,
        flexDirection.column,
        gap := S.spacing._3,
        Widget.foreach(packs.toList)(card(st, _, activeId)),
      )
    }

}
