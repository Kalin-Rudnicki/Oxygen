package oxygen.ui.electron.facades

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@js.native
@JSImport("electron", "BrowserWindow")
class BrowserWindow(@scala.annotation.unused options: js.UndefOr[BrowserWindowOptions] = js.undefined) extends js.Object {
  def loadURL(url: String): Unit = js.native
  def loadFile(filePath: String): Unit = js.native
  def close(): Unit = js.native
  def destroy(): Unit = js.native
  def focus(): Unit = js.native
  def isFocused(): Boolean = js.native

  // WebContents is its own complex object, usually defined as a separate trait
  val webContents: js.Dynamic = js.native

  // Event handling (standard EventEmitter style)
  def on(event: String, listener: js.Function): this.type = js.native
  def once(event: String, listener: js.Function): this.type = js.native
}

// Use a trait for options to allow literal object creation
trait BrowserWindowOptions extends js.Object {
  var width: js.UndefOr[Int] = js.undefined
  var height: js.UndefOr[Int] = js.undefined
  var x: js.UndefOr[Int] = js.undefined
  var y: js.UndefOr[Int] = js.undefined
  var useContentSize: js.UndefOr[Boolean] = js.undefined
  var center: js.UndefOr[Boolean] = js.undefined
  var webPreferences: js.UndefOr[WebPreferences] = js.undefined
}

trait WebPreferences extends js.Object {
  var nodeIntegration: js.UndefOr[Boolean] = js.undefined
  var contextIsolation: js.UndefOr[Boolean] = js.undefined
  var preload: js.UndefOr[String] = js.undefined
}
