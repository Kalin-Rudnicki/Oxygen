package oxygen.http.core

import oxygen.http.core.partial.*
import oxygen.http.schema.*
import zio.*
import zio.http.{Body, Headers, Status}

final case class ResponseCodec[A](statusCodes: StatusCodes[A], codec: ResponseCodecNoStatus[A]) {

  def unsafeBuild(apiName: String, endpointName: String): ResponseSchema =
    codec.schemaAggregator.unsafeBuild(apiName, endpointName, statusCodes.expectedStatuses)

  def canLikelyDecode(status: Status): Boolean = statusCodes.expectedStatuses.contains(status)
  def decode(headers: Headers, body: Body): ZIO[Scope, ResponseDecodingFailure, A] = codec.decode(headers, body)

  def encode(value: A): (Status, Headers, Body) = {
    val (headers, body) = codec.encode(value)
    (statusCodes.status(value), headers, body)
  }

}
object ResponseCodec {

  given nothing: ResponseCodec[Nothing] = ResponseCodec(new StatusCodes.DNE, ResponseCodecNoStatus.DNE)

}
