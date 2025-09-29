package oxygen.http.core

import oxygen.http.core.partial.*
import oxygen.http.schema.*
import oxygen.predef.core.*
import zio.*
import zio.http.{Body, Headers, Status}

trait ResponseCodec[A] {

  def unsafeBuild(apiName: String, endpointName: String): ResponseSchema

  def canLikelyDecode(status: Status): Boolean
  def decode(status: Status, headers: Headers, body: Body): ZIO[Scope, ResponseDecodingFailure, A]

  def encode(value: A): (Status, Headers, Body)

}
object ResponseCodec {

  final case class Standard[A](statusCodes: StatusCodes[A], codec: ResponseCodecNoStatus[A]) extends ResponseCodec[A] {

    override def unsafeBuild(apiName: String, endpointName: String): ResponseSchema =
      codec.schemaAggregator.unsafeBuild(apiName, endpointName, statusCodes.expectedStatuses)

    override def canLikelyDecode(status: Status): Boolean = statusCodes.expectedStatuses.contains(status)
    override def decode(status: Status, headers: Headers, body: Body): ZIO[Scope, ResponseDecodingFailure, A] = codec.decode(headers, body)

    override def encode(value: A): (Status, Headers, Body) = {
      val (headers, body) = codec.encode(value)
      (statusCodes.status(value), headers, body)
    }

  }

  // TODO (KR) : add schema for this, and ability to auto-derive something of this shape
  final case class FirstOf[A](cases: NonEmptyList[ResponseCodec.Standard[? <: A]]) extends ResponseCodec[A] {

    override def unsafeBuild(apiName: String, endpointName: String): ResponseSchema =
      throw new UnsupportedOperationException("Not supported: ResponseCodec.FirstOf.unsafeBuild")

    override def canLikelyDecode(status: Status): Boolean = cases.exists(_.canLikelyDecode(status))

    override def decode(status: Status, headers: Headers, body: Body): ZIO[Scope, ResponseDecodingFailure, A] =
      cases.find(_.canLikelyDecode(status)) match {
        case Some(kase) => kase.decode(status, headers, body)
        case None       => ZIO.fail(ResponseDecodingFailure(Nil, DecodingFailureCause.DecodeError(s"No case can handle status: $status")))
      }

    override def encode(value: A): (Status, Headers, Body) =
      throw new UnsupportedOperationException("Not supported: ResponseCodec.FirstOf.encode")

  }
  object FirstOf {

    final class Builder1[A] {
      def subtype[B <: A]: Builder2[A, B] = new Builder2[A, B]
      def id: Builder2[A, A] = subtype[A]
    }

    final class Builder2[A, B <: A] {
      def statusCodes(codes: StatusCodes[B]): Builder3[A, B] = Builder3(codes)
      def exact(status: Status): Builder3[A, B] = statusCodes(StatusCodes.Exact(status))
    }

    final class Builder3[A, B <: A](codes: StatusCodes[B]) {
      def codec(codec: ResponseCodecNoStatus[B]): Standard[B] = Standard(codes, codec)
    }

  }

  given nothing: ResponseCodec[Nothing] = ResponseCodec.Standard(new StatusCodes.DNE, ResponseCodecNoStatus.DNE)

  def firstOf[A](
      c0: FirstOf.Builder1[A] => ResponseCodec.Standard[? <: A],
      cN: (FirstOf.Builder1[A] => ResponseCodec.Standard[? <: A])*,
  ): ResponseCodec[A] = {
    val builder = new FirstOf.Builder1[A]
    ResponseCodec.FirstOf(NonEmptyList(c0(builder), cN.toList.map(_(builder))))
  }

}
