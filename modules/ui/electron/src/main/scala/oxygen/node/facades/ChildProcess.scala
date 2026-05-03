package oxygen.node.facades

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@JSImport("child_process", JSImport.Namespace)
@js.native
private[node] object ChildProcess extends js.Object {
  def spawn(command: String, args: js.Array[String], options: js.Any): js.Dynamic = js.native
}
