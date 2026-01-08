package oxygen.http.core

import oxygen.http.core.partial.*
import oxygen.http.model.internal.ReceivedResponse
import oxygen.http.schema.*
import oxygen.http.schema.partial.ResponseSchemaAggregator
import oxygen.predef.core.*
import zio.*
import zio.http.{Body, Headers, Response, Status}

trait ResponseCodec[A] {

  def unsafeBuild(apiName: String, endpointName: String): ResponseSchema

  def canLikelyDecode(status: Status): Boolean

  def decode(response: ReceivedResponse): ZIO[Scope, ResponseDecodingFailure, A]

  def encode(value: A): (Status, Headers, Body)
  final def encodeResponse(value: A): Response = {
    val (status, headers, body) = encode(value)
    Response(status = status, headers = headers, body = body)
  }

  final def transform[B](ab: A => B, ba: B => A): ResponseCodec[B] = ResponseCodec.Transform(this, ab, ba)
  final def transformZIO[B](ab: A => ZIO[Scope, ResponseDecodingFailure, B], ba: B => A): ResponseCodec[B] = ResponseCodec.TransformZIO(this, ab, ba)

}
object ResponseCodec {

  inline def apply[A](using ev: ResponseCodec[A]): ResponseCodec[A] = ev

  final case class Standard[A](statusCodes: StatusCodes[A], codec: ResponseCodecNoStatus[A]) extends ResponseCodec[A] {

    override def unsafeBuild(apiName: String, endpointName: String): ResponseSchema =
      codec.schemaAggregator.unsafeBuild(apiName, endpointName, statusCodes.expectedStatuses)

    override def canLikelyDecode(status: Status): Boolean = statusCodes.expectedStatuses.contains(status)
    override def decode(response: ReceivedResponse): ZIO[Scope, ResponseDecodingFailure, A] = codec.decode(response.headers, response.body)

    override def encode(value: A): (Status, Headers, Body) = {
      val (headers, body) = codec.encode(value)
      (statusCodes.status(value), headers, body)
    }

  }

  final case class Raw(expectedStatuses: ExpectedStatuses) extends ResponseCodec[(Status, Headers, Body)] {

    override def unsafeBuild(apiName: String, endpointName: String): ResponseSchema =
      ResponseSchemaAggregator.empty.unsafeBuild(apiName, endpointName, expectedStatuses)

    override def canLikelyDecode(status: Status): Boolean =
      expectedStatuses.contains(status)

    override def decode(response: ReceivedResponse): ZIO[Scope, ResponseDecodingFailure, (Status, Headers, Body)] =
      ZIO.succeed { (response.status, ZioHttpCompat.filterStandardHeaders(response.headers), response.body) }

    override def encode(value: (Status, Headers, Body)): (Status, Headers, Body) =
      (value._1, ZioHttpCompat.filterStandardHeaders(value._2), value._3)

  }

  final case class Transform[A, B](a: ResponseCodec[A], ab: A => B, ba: B => A) extends ResponseCodec[B] {

    override def unsafeBuild(apiName: String, endpointName: String): ResponseSchema = a.unsafeBuild(apiName, endpointName)

    override def canLikelyDecode(status: Status): Boolean = a.canLikelyDecode(status)

    override def decode(response: ReceivedResponse): ZIO[Scope, ResponseDecodingFailure, B] = a.decode(response).map(ab)

    override def encode(value: B): (Status, Headers, Body) = a.encode(ba(value))

  }

  final case class TransformZIO[A, B](a: ResponseCodec[A], ab: A => ZIO[Scope, ResponseDecodingFailure, B], ba: B => A) extends ResponseCodec[B] {

    override def unsafeBuild(apiName: String, endpointName: String): ResponseSchema = a.unsafeBuild(apiName, endpointName)

    override def canLikelyDecode(status: Status): Boolean = a.canLikelyDecode(status)

    override def decode(response: ReceivedResponse): ZIO[Scope, ResponseDecodingFailure, B] = a.decode(response).flatMap(ab)

    override def encode(value: B): (Status, Headers, Body) = a.encode(ba(value))

  }

  // TODO (KR) : add schema for this, and ability to auto-derive something of this shape
  final case class FirstOf[A](cases: NonEmptyList[ResponseCodec.Standard[? <: A]]) extends ResponseCodec[A] {

    override def unsafeBuild(apiName: String, endpointName: String): ResponseSchema =
      throw new UnsupportedOperationException("Not supported: ResponseCodec.FirstOf.unsafeBuild")

    override def canLikelyDecode(status: Status): Boolean = cases.exists(_.canLikelyDecode(status))

    override def decode(response: ReceivedResponse): ZIO[Scope, ResponseDecodingFailure, A] =
      cases.find(_.canLikelyDecode(response.status)) match {
        case Some(kase) => kase.decode(response)
        case None       => ZIO.fail(ResponseDecodingFailure(Nil, DecodingFailureCause.DecodeError(s"No case can handle status: ${response.status}", DecodingFailureCause.DecodeInput.NotApplicable)))
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
