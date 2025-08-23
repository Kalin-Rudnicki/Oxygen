package oxygen.http.core.partial

import oxygen.http.core.ResponseDecodingFailure
import oxygen.http.core.partial.{PartialBodyCodec, PartialParamCodec}
import oxygen.http.schema.{ResponseBodySchema, ResponseHeaderSchema}
import oxygen.http.schema.partial.ResponseSchemaAggregator
import oxygen.predef.core.*
import zio.*
import zio.http.{Body, Header, Headers}

sealed trait ResponseCodecNoStatus[A] {

  val sources: List[ResponseDecodingFailure.Source]
  val schemaAggregator: ResponseSchemaAggregator

  def decode(headers: Headers, body: Body): ZIO[Scope, ResponseDecodingFailure, A]

  private[ResponseCodecNoStatus] def encodeInternal(value: A, acc: ResponseCodecNoStatus.Builder): ResponseCodecNoStatus.Builder
  final def encode(value: A): (Headers, Body) = encodeInternal(value, ResponseCodecNoStatus.Builder.empty).build

  final def ++[B](that: ResponseCodecNoStatus[B])(using zip: Zip[A, B]): ResponseCodecNoStatus[zip.Out] = ResponseCodecNoStatus.And(this, that, zip)

}
object ResponseCodecNoStatus {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Builder
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private final case class Builder(
      headers: Growable[Header.Custom],
      body: Body,
  ) {
    def build: (Headers, Body) = (Headers(headers.toArraySeq), body)
  }
  private object Builder {
    val empty: Builder = Builder(Growable.empty, Body.empty)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Givens
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  given unit: ResponseCodecNoStatus[Unit] = ResponseCodecNoStatus.Empty
  given fromBody: [A: PartialBodyCodec as codec] => ResponseCodecNoStatus[A] = ResponseCodecNoStatus.ApplyPartialBody(codec)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Impls
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  case object DNE extends ResponseCodecNoStatus[Nothing] {
    override val sources: List[ResponseDecodingFailure.Source] = Nil
    override val schemaAggregator: ResponseSchemaAggregator = ResponseSchemaAggregator.empty
    override def decode(headers: Headers, body: Body): ZIO[Scope, ResponseDecodingFailure, Nothing] = throw new RuntimeException("This is supposed to be DNE!")
    override private[ResponseCodecNoStatus] def encodeInternal(value: Nothing, acc: Builder): Builder = throw new RuntimeException("This is supposed to be DNE!")
  }

  case object Empty extends ResponseCodecNoStatus[Unit] {
    override val sources: List[ResponseDecodingFailure.Source] = Nil
    override val schemaAggregator: ResponseSchemaAggregator = ResponseSchemaAggregator.empty
    override def decode(headers: Headers, body: Body): ZIO[Scope, ResponseDecodingFailure, Unit] = ZIO.unit
    override private[ResponseCodecNoStatus] def encodeInternal(value: Unit, acc: Builder): Builder = acc
  }

  final case class ApplyPartialHeader[A](partial: PartialParamCodec[A], name: String, doc: Option[String]) extends ResponseCodecNoStatus[A] {

    override val sources: List[ResponseDecodingFailure.Source] = ResponseDecodingFailure.Source.Header(name) :: Nil
    override val schemaAggregator: ResponseSchemaAggregator = ResponseSchemaAggregator.header(ResponseHeaderSchema(name, partial.partialParamSchema.tpe, partial.partialParamSchema.schema, doc))

    override def decode(headers: Headers, body: Body): ZIO[Scope, ResponseDecodingFailure, A] =
      ZIO.fromEither { partial.decode(headers.rawHeaders(name).toList).leftMap(ResponseDecodingFailure(sources, _)) }

    override private[ResponseCodecNoStatus] def encodeInternal(value: A, acc: Builder): Builder = {
      val encoded = partial.encode(value)
      if (encoded.nonEmpty) acc.copy(headers = acc.headers ++ Growable.many(encoded).map(Header.Custom(name, _)))
      else acc
    }

  }

  sealed trait ApplyPartialBody[A] extends ResponseCodecNoStatus[A]
  object ApplyPartialBody {

    def apply[A](partial: PartialBodyCodec[A]): ApplyPartialBody[A] =
      partial match {
        case PartialBodyCodec.Empty              => ApplyPartialBodyEmpty(PartialBodyCodec.Empty)
        case partial: PartialBodyCodec.Single[A] => ApplyPartialBodySingle(partial)
      }

  }

  final case class ApplyPartialBodyEmpty(partial: PartialBodyCodec.Empty.type) extends ApplyPartialBody[Unit] {

    override val sources: List[ResponseDecodingFailure.Source] = ResponseDecodingFailure.Source.Body :: Nil
    override val schemaAggregator: ResponseSchemaAggregator = ResponseSchemaAggregator.body(ResponseBodySchema.Empty)

    override def decode(headers: Headers, body: Body): ZIO[Scope, ResponseDecodingFailure, Unit] =
      ZIO.unit

    override private[ResponseCodecNoStatus] def encodeInternal(value: Unit, acc: Builder): Builder =
      acc

  }

  final case class ApplyPartialBodySingle[A](partial: PartialBodyCodec.Single[A]) extends ApplyPartialBody[A] {

    override val sources: List[ResponseDecodingFailure.Source] = ResponseDecodingFailure.Source.Body :: Nil
    override val schemaAggregator: ResponseSchemaAggregator = ResponseSchemaAggregator.body(ResponseBodySchema.Single(partial.partialBodySchema.schema))

    override def decode(headers: Headers, body: Body): ZIO[Scope, ResponseDecodingFailure, A] =
      partial.decode(body).mapError(ResponseDecodingFailure(sources, _))

    override private[ResponseCodecNoStatus] def encodeInternal(value: A, acc: Builder): Builder =
      acc.copy(body = partial.encode(value))

  }

  final case class And[A, B, C](a: ResponseCodecNoStatus[A], b: ResponseCodecNoStatus[B], zip: Zip.Out[A, B, C]) extends ResponseCodecNoStatus[C] {

    override val sources: List[ResponseDecodingFailure.Source] = a.sources ++ b.sources
    override val schemaAggregator: ResponseSchemaAggregator = a.schemaAggregator >>> b.schemaAggregator

    override def decode(headers: Headers, body: Body): ZIO[Scope, ResponseDecodingFailure, C] =
      for {
        aValue <- a.decode(headers, body)
        bValue <- b.decode(headers, body)
      } yield zip.zip(aValue, bValue)

    override private[ResponseCodecNoStatus] def encodeInternal(value: C, acc: Builder): Builder = {
      val (aValue, bValue) = zip.unzip(value)
      b.encodeInternal(bValue, a.encodeInternal(aValue, acc))
    }

  }

}
