package oxygen.ui.web.defaults

import oxygen.ui.web.create.*
import oxygen.ui.web.style.{OxygenColorSystem, OxygenThemes}

object OxygenStyleVarDefaults {

  /** Cold-start pack. Runtime pack swaps via [[oxygen.ui.web.service.Theme.applyPack]]. */
  private val defaultPack = OxygenThemes.default

  private val darkPalette: OxygenColorSystem.GeneratedPalette =
    OxygenColorSystem.generate(defaultPack.dark, OxygenColorSystem.Mode.Dark)

  private val lightPalette: OxygenColorSystem.GeneratedPalette =
    OxygenColorSystem.generate(defaultPack.light, OxygenColorSystem.Mode.Light)

  /** Default Oxygen theme (dark mode values on :root / data-color-mode=dark). */
  val Oxygen: OxygenStyleVars[String] =
    OxygenColorSystem.toStyleVars(darkPalette)

  /** Light mode companion values. */
  val OxygenLight: OxygenStyleVars[String] =
    OxygenColorSystem.toStyleVars(lightPalette)

  /** CSS variable sheets for both modes. Prefer emitting both; switch via data-color-mode. */
  def oxygenColorSheets: Seq[StyleSheet] =
    Seq(
      OxygenStyleVars.toCSS(Oxygen, scope = """:root, [data-color-mode="dark"]""", header = "oxygen-default-dark"),
      OxygenStyleVars.toCSS(OxygenLight, scope = """[data-color-mode="light"]""", header = "oxygen-default-light"),
    )

}
