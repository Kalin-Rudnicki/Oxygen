package oxygen.ui.web.service

import org.scalajs.dom.{document, window, HTMLElement}
import oxygen.ui.web.create.*
import oxygen.ui.web.internal.PageManager
import oxygen.ui.web.style.OxygenColorSystem
import oxygen.ui.web.style.OxygenColorSystem.Mode
import oxygen.ui.web.style.OxygenThemes
import oxygen.ui.web.style.OxygenThemes.Pack
import zio.*

/**
  * Runtime theme-pack service: apply CSS var packs, persist selection, re-render.
  *
  * Pack *definitions* live in [[OxygenThemes]]; mutation of the live document belongs here.
  */
object Theme {

  val storageKey: String = OxygenThemes.storageKey

  private val darkStyleElId: String = "oxygen-theme-active-dark"
  private val lightStyleElId: String = "oxygen-theme-active-light"

  private def varsBlock(scope: String, vars: OxygenStyleVars[String]): String =
    OxygenStyleVars.toCSS(vars, scope, "theme-pack").innerHTML

  private def upsertStyle(id: String, css: String): Unit = {
    Option(document.getElementById(id)).foreach { old =>
      old.parentNode.removeChild(old)
    }
    val el = document.createElement("style")
    el.id = id
    el.asInstanceOf[HTMLElement].textContent = css
    document.head.appendChild(el)
  }

  def applyPack(pack: Pack): UIO[Unit] =
    ZIO.succeed {
      val darkVars = OxygenColorSystem.toStyleVars(OxygenColorSystem.generate(pack.dark, Mode.Dark))
      val lightVars = OxygenColorSystem.toStyleVars(OxygenColorSystem.generate(pack.light, Mode.Light))
      upsertStyle(darkStyleElId, varsBlock(""":root, [data-color-mode="dark"]""", darkVars))
      upsertStyle(lightStyleElId, varsBlock("""[data-color-mode="light"]""", lightVars))
      document.documentElement.setAttribute("data-oxygen-theme", pack.id)
    }

  def readStored: UIO[Option[Pack]] =
    ZIO.succeed {
      Option(window.localStorage.getItem(storageKey)).flatMap(OxygenThemes.parse)
    }

  def writeStored(pack: Pack): UIO[Unit] =
    ZIO.succeed {
      window.localStorage.setItem(storageKey, pack.id)
    }

  def applyAndPersist(pack: Pack): UIO[Unit] =
    writeStored(pack) *>
      applyPack(pack) *>
      PageManager.reRenderCurrentPage *>
      Broadcast.postThemePack(pack).ignore

  def applyStoredOrDefault: UIO[Unit] =
    readStored.map(_.getOrElse(OxygenThemes.default)).flatMap(applyPack)

  def currentId: UIO[String] =
    ZIO.succeed {
      Option(document.documentElement.getAttribute("data-oxygen-theme")).getOrElse(OxygenThemes.default.id)
    }

  def current: UIO[Pack] =
    currentId.map(id => OxygenThemes.byId.getOrElse(id, OxygenThemes.default))

  /**
    * Listen for pack changes from other tabs and apply CSS.
    * Lives for the provided [[Scope]] (app root / prePageLoad is the usual home).
    */
  def subscribeCrossTab: URIO[Scope, Unit] =
    Broadcast.subscribeThemePack.foreach(applyPack).forkScoped.unit

}
