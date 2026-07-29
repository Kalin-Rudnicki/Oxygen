package oxygen.ui.web.service

import org.scalajs.dom.document
import oxygen.predef.core.*
import zio.*

/**
  * Light / dark / system color mode. Values rebind CSS vars via `data-color-mode` on `<html>`.
  * First-paint: call [[applyStoredOrSystem]] from `prePageLoad` (or set attribute inline before CSS).
  */
object ColorMode {

  /**
    * Preference mode.
    *
    * [[Mode.Concrete]] is the resolved light/dark value written to the document.
    * [[Mode.System]] follows the OS preference until resolved.
    */
  sealed abstract class Mode(final val lower: String, final val pretty: String)
  object Mode {

    given StrictEnum[ColorMode.Mode] = StrictEnum.deriveNel(a => NonEmptyList.of(a.lower, a.pretty))

    sealed abstract class Concrete(lower: String, pretty: String) extends Mode(lower, pretty)
    object Concrete {

      given StrictEnum[ColorMode.Mode.Concrete] = StrictEnum.deriveNel(a => NonEmptyList.of(a.lower, a.pretty))

      def show(mode: Mode.Concrete): String = mode.lower

      def resolve(mode: Mode): UIO[Concrete] =
        mode match {
          case c: Concrete => ZIO.succeed(c)
          case System      => prefersDark.map(if _ then Dark else Light)
        }

    }

    case object System extends Mode("system", "System")
    case object Light extends Concrete("light", "Light")
    case object Dark extends Concrete("dark", "Dark")

  }

  val storageKey: String = "oxygen.color-mode"

  def parse(raw: String): Option[Mode] = StrictEnum[Mode].decodeOption(raw)

  def show(mode: Mode): String = mode.lower

  def prefersDark: UIO[Boolean] =
    MatchMedia.matchesZIO("(prefers-color-scheme: dark)")

  def setDocumentAttribute(resolved: Mode.Concrete): UIO[Unit] =
    ZIO.succeed {
      document.documentElement.setAttribute("data-color-mode", Mode.Concrete.show(resolved))
    }

  def applyMode(mode: Mode): UIO[Unit] =
    Mode.Concrete.resolve(mode).flatMap(setDocumentAttribute)

  def readStored: UIO[Option[Mode]] =
    ZIO.succeed {
      Option(org.scalajs.dom.window.localStorage.getItem(storageKey)).flatMap(parse)
    }

  def writeStored(mode: Mode): UIO[Unit] =
    ZIO.succeed {
      org.scalajs.dom.window.localStorage.setItem(storageKey, show(mode))
    }

  /**
    * Apply stored preference or system default to `<html data-color-mode>`.
    * Safe to call before style sheets are injected if CSS uses both selectors.
    */
  def applyStoredOrSystem: UIO[Unit] =
    readStored.map(_.getOrElse(Mode.System)).flatMap(applyMode)

  def setAndPersist(mode: Mode): UIO[Unit] =
    writeStored(mode) *>
      applyMode(mode) *>
      // W12-T02: notify other tabs (best-effort)
      Broadcast.postThemeMode(mode).ignore

  /**
    * Listen for theme changes from other tabs and apply.
    * Lives for the provided [[Scope]] (app root / prePageLoad is the usual home).
    */
  def subscribeCrossTab: URIO[Scope, Unit] =
    Broadcast.subscribeThemeMode.foreach(applyMode).forkScoped.unit

}
