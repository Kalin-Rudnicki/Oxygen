package oxygen.node.facades

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@JSImport("fs", JSImport.Namespace)
@js.native
private[node] object FS extends js.Object {
  def readFile(path: String, encoding: String, callback: js.Function2[js.Error, String, Unit]): Unit = js.native
  def writeFile(path: String, data: String, callback: js.Function1[js.Error, Unit]): Unit = js.native
  def existsSync(path: String): Boolean = js.native
  def mkdir(path: String, options: js.Any, callback: js.Function1[js.Error, Unit]): Unit = js.native
  def readdir(path: String, callback: js.Function2[js.Error, js.Array[String], Unit]): Unit = js.native
}
