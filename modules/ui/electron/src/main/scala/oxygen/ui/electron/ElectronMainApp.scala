package oxygen.ui.electron

import oxygen.ui.electron.facades.*
import scala.scalajs.js

trait ElectronMainApp extends scala.App {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Customizations
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def loadHtmlPath: String = "index.html"

  // TODO (KR) : do something smarter with this, like actually be able to leverage the menu
  def hideMenu: Boolean = true

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Internal
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private val electron = js.Dynamic.global.require("electron")
  private val app = electron.app

  private def createWindow(): Unit = {
    val winOptions = new BrowserWindowOptions {
      width = 1200
      height = 800
      webPreferences = new WebPreferences {
        nodeIntegration = true
        contextIsolation = false
      }
    }

    new BrowserWindow(winOptions).loadFile(loadHtmlPath)
    if hideMenu then
      electron.Menu.setApplicationMenu(null)
  }

  app.whenReady().`then`(() => createWindow())

  app.on(
    "window-all-closed",
    () => {
      if js.Dynamic.global.process.platform.asInstanceOf[String] != "darwin" then
        app.quit()
    },
  )

}
