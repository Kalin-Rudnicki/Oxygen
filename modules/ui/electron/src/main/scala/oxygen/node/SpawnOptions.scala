package oxygen.node

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

final case class SpawnOptions(
    cwd: Option[String] = None,
    env: Option[Map[String, String]] = None,
    shell: Boolean = false,
) {

  def toJS: js.Dynamic = {
    val o = js.Dynamic.literal()
    cwd.foreach(c => o.cwd = c)
    env.foreach(e => o.env = e.toJSDictionary)
    if shell then o.shell = true
    o
  }

}
