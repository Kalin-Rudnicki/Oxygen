package oxygen.http.model

import oxygen.predef.core.*

final case class Headers(rawHeaders: ArraySeq[(String, String)]) {

  private lazy val lowerHeaderMap: Map[String, List[String]] = rawHeaders.toList.groupMap(_._1.toLowerCase)(_._2)

  def header(key: String): List[String] = lowerHeaderMap.getOrElse(key.toLowerCase, Nil)

  def ++(that: Headers): Headers =
    Headers(this.rawHeaders ++ that.rawHeaders)

}
object Headers {

  val empty: Headers = Headers(ArraySeq.empty[(String, String)])

  inline def of(headers: (String, String)*): Headers = Headers(ArraySeq(headers*))

}
