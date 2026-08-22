package oxygen.ui.web.service

import zio.*

/**
  * Unified facade over the two independent color-theme services:
  *   - [[ColorMode]] — light / dark / system (attr `data-color-mode`, key `oxygen.color-mode`)
  *   - [[Theme]]     — theme pack / `OxygenThemes` (attr `data-oxygen-theme`, key `oxygen.theme-pack`)
  *
  * Replaces the copy-pasted `prePageLoad` snippet:
  * {{{
  *   ColorMode.applyStoredOrSystem *>
  *     Theme.applyStoredOrDefault *>
  *     ColorMode.subscribeCrossTab *>
  *     Theme.subscribeCrossTab
  * }}}
  * with a single combinator:
  * {{{
  *   override protected def prePageLoad: RIO[Env & Scope, Unit] = ColorTheme.install
  * }}}
  *
  * The individual [[ColorMode]] / [[Theme]] methods remain public — the manual form still works.
  */
object ColorTheme {

  /**
    * Apply the stored (or system / default) color mode + theme pack to the live document.
    * Safe to call first-paint from `prePageLoad`. No cross-tab subscription (see [[install]]).
    */
  def applyStored: UIO[Unit] =
    ColorMode.applyStoredOrSystem *>
      Theme.applyStoredOrDefault

  /**
    * Subscribe both color mode + theme pack to their cross-tab [[Broadcast]] channels.
    * Lives for the provided [[Scope]] (app root / `prePageLoad` is the usual home).
    */
  def subscribeCrossTab: URIO[Scope, Unit] =
    ColorMode.subscribeCrossTab *>
      Theme.subscribeCrossTab

  /**
    * One-call bootstrap: apply stored preferences, then keep them in sync across tabs.
    * Preferred over the manual 4-line snippet.
    */
  def install: URIO[Scope, Unit] =
    applyStored *>
      subscribeCrossTab

}
