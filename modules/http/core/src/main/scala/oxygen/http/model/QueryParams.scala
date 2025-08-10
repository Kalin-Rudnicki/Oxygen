package oxygen.http.model

import oxygen.predef.core.*

final case class QueryParams(rawQueryParams: Contiguous[(String, String)]) {

  private lazy val queryParamMap: Map[String, List[String]] = rawQueryParams.toList.groupMap(_._1)(_._2)

  def queryParam(key: String): List[String] = queryParamMap.getOrElse(key, Nil)

}
object QueryParams {

  val empty: QueryParams = QueryParams(Contiguous.empty)

  def apply(queryParams: (String, String)*): QueryParams = QueryParams(Contiguous(queryParams*))

}
