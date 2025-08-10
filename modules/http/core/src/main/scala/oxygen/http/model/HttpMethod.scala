package oxygen.http.model

import oxygen.core.Enum

sealed abstract class HttpMethod(final val method: String) {
  override final def toString: String = method
}
object HttpMethod {

  def apply(method: String): HttpMethod = HttpMethod.Standard.ToString.decode(method).getOrElse(HttpMethod.NonStandard(method))

  sealed abstract class Standard(method: String) extends HttpMethod(method), Enum[HttpMethod.Standard]
  object Standard extends Enum.Companion[HttpMethod.Standard] {

    // format: off
    override val values: Array[Standard] = Array(HttpMethod.GET, HttpMethod.POST, HttpMethod.PUT, HttpMethod.DELETE, HttpMethod.HEAD, HttpMethod.OPTIONS, HttpMethod.CONNECT, HttpMethod.TRACE, HttpMethod.PATCH)
    // format: on

  }

  final case class NonStandard private[HttpMethod] (_method: String) extends HttpMethod(_method)

  // =====|  |=====

  case object GET extends HttpMethod.Standard("GET")
  case object POST extends HttpMethod.Standard("POST")
  case object PUT extends HttpMethod.Standard("PUT")
  case object PATCH extends HttpMethod.Standard("PATCH")
  case object DELETE extends HttpMethod.Standard("DELETE")
  case object HEAD extends HttpMethod.Standard("HEAD")
  case object OPTIONS extends HttpMethod.Standard("OPTIONS")
  case object CONNECT extends HttpMethod.Standard("CONNECT")
  case object TRACE extends HttpMethod.Standard("TRACE")

}
