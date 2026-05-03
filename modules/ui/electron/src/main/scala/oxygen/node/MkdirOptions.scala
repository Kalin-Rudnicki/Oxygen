package oxygen.node

import scala.scalajs.js

final case class MkdirOptions(
    recursive: Boolean = false,
) {

  def toJS: js.Dynamic = {
    val o = js.Dynamic.literal()
    o.recursive = recursive
    o
  }

}
