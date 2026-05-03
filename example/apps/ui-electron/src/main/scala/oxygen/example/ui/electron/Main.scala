package oxygen.example.ui.electron

import oxygen.ui.electron.*
import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("main")
object Main extends ElectronMainApp {

  override def hideMenu: Boolean = false

}
