package oxygen.http.core

import oxygen.json.*
import oxygen.predef.core.*
import oxygen.schema.JsonSchema

final case class ResponseDecodingFailure(
    sources: List[ResponseDecodingFailure.Source],
    cause: DecodingFailureCause,
) extends Throwable derives JsonSchema {

  private def showSources: String = sources match
    case Nil           => "(no sources)"
    case source :: Nil => source.toString
    case _             => sources.mkString("[", ", ", "]")

  override def getMessage: String = s"Error decoding http-response $showSources : $cause"

  override def toString: String = getMessage

}
object ResponseDecodingFailure {

  enum Source derives JsonSchema {
    @jsonType("ResponseHeader") case Header(name: String)
    @jsonType("ResponseBody") case Body

    override final def toString: String = this match
      case Source.Header(name) => s"response header '$name'"
      case Source.Body         => "response body"

  }

}
