package oxygen.http.model.internal

import oxygen.predef.core.*
import zio.*
import zio.http.*

private[http] final case class RequestBuilder(
    method: Method,
    paths: Growable[String],
    queryParams: Growable[(String, Chunk[String])],
    headers: Growable[Header],
    body: Body,
) {

  def addQueryParams(qp: Growable[(String, Chunk[String])]): RequestBuilder = copy(queryParams = this.queryParams ++ qp)

}
private[http] object RequestBuilder {
  val empty: RequestBuilder = RequestBuilder(Method.ANY, Growable.empty, Growable.empty, Growable.empty, Body.empty)
}
