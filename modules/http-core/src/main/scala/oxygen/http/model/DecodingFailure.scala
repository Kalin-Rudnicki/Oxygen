package oxygen.http.model

import oxygen.json.*

final case class DecodingFailure(
    source: DecodingFailure.Source,
    cause: DecodingFailure.Cause,
) extends Throwable derives JsonCodec {

  override def getMessage: String = s"Error decoding http-$source : $cause"

  override def toString: String = getMessage

}
object DecodingFailure {

  sealed trait Source derives JsonCodec

  enum RequestSource extends Source {

    @jsonType("RequestQueryParam") case QueryParam(name: String)
    @jsonType("RequestHeader") case Header(name: String)
    @jsonType("RequestBody") case Body

    override final def toString: String = this match
      case RequestSource.QueryParam(name) => s"request query-param '$name'"
      case RequestSource.Header(name)     => s"request header '$name'"
      case RequestSource.Body             => "request body"

  }

  enum ResponseSource extends Source {
    @jsonType("ResponseHeader") case Header(name: String)
    @jsonType("ResponseBody") case Body

    override final def toString: String = this match
      case ResponseSource.Header(name) => s"response header '$name'"
      case ResponseSource.Body         => "response body"

  }

  enum Cause derives JsonCodec {
    case MissingRequired
    case ManyNotSupported(received: Int)
    case DecodeError(error: String)

    override final def toString: String = this match
      case Cause.MissingRequired            => "Missing required parameter"
      case Cause.ManyNotSupported(received) => s"Providing many parameters is not allowed (received $received)"
      case Cause.DecodeError(error)         => s"[failed to decode] $error"

  }

}
