package oxygen.http.core

import oxygen.http.model.*
import zio.*

trait ResponseCodec[A] {
  def decode(headers: Headers, body: HttpBody): ZIO[Scope, DecodingFailure, A]
  def encode(value: A): (Headers, HttpBody)
}
object ResponseCodec {

  final case class FromBodyCodec[A](bodyCodec: BodyCodec[A]) extends ResponseCodec[A] {
    override def decode(headers: Headers, body: HttpBody): ZIO[Scope, DecodingFailure, A] = bodyCodec.decode(body).mapError(DecodingFailure(DecodingFailure.ResponseSource.Body, _))
    override def encode(value: A): (Headers, HttpBody) = (Headers.empty, bodyCodec.encode(value))
  }

  given fromBodyCodec: [A: BodyCodec as bodyCodec] => ResponseCodec[A] = FromBodyCodec(bodyCodec)

}
