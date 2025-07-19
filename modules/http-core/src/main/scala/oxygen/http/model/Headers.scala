package oxygen.http.model

import oxygen.predef.core.*

final case class Headers(rawHeaders: Contiguous[(String, String)]) {

  private lazy val lowerHeaderMap: Map[String, List[String]] = rawHeaders.toList.groupMap(_._1.toLowerCase)(_._2)

  def header(key: String): List[String] = lowerHeaderMap.getOrElse(key.toLowerCase, Nil)

  def ++(that: Headers): Headers =
    Headers(this.rawHeaders ++ that.rawHeaders)

}
object Headers {

  val empty: Headers = Headers(Contiguous.empty)

  def apply(headers: (String, String)*): Headers = Headers(Contiguous(headers*))

}
