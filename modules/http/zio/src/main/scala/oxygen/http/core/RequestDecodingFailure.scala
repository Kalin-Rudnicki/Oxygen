package oxygen.http.core

import oxygen.json.*
import oxygen.predef.core.*
import oxygen.schema.JsonSchema

final case class RequestDecodingFailure(
    sources: List[RequestDecodingFailure.Source],
    cause: DecodingFailureCause,
) extends Throwable derives JsonSchema {

  private def showSources: String = sources match
    case Nil           => "(no sources)"
    case source :: Nil => source.toString
    case _             => sources.mkString("[", ", ", "]")

  override def getMessage: String = s"Error decoding http-request $showSources : $cause"

  override def toString: String = getMessage

}
object RequestDecodingFailure {

  enum Source derives JsonSchema {

    @jsonType("RequestQueryParam") case QueryParam(name: String)
    @jsonType("RequestHeader") case Header(name: String)
    @jsonType("RequestBody") case Body

    override final def toString: String = this match
      case Source.QueryParam(name) => s"query-param '$name'"
      case Source.Header(name)     => s"header '$name'"
      case Source.Body             => "body"

  }

}
