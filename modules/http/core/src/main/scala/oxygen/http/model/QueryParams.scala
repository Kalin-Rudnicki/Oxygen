package oxygen.http.model

import oxygen.predef.core.*

final case class QueryParams(rawQueryParams: ArraySeq[(String, String)]) {

  private lazy val queryParamMap: Map[String, List[String]] = rawQueryParams.toList.groupMap(_._1)(_._2)

  def queryParam(key: String): List[String] = queryParamMap.getOrElse(key, Nil)

}
object QueryParams {

  val empty: QueryParams = QueryParams(ArraySeq.empty[(String, String)])

  inline def of(queryParams: (String, String)*): QueryParams = QueryParams(ArraySeq(queryParams*))

}
