package oxygen.http.core.partial

import oxygen.http.core.{DecodingFailureCause, ReadOnlyCachedHttpBody, ResponseDecodingFailure}
import oxygen.http.core.partial.{PartialBodyCodec, PartialParamCodec}
import oxygen.http.schema.{ResponseBodySchema, ResponseHeaderSchema}
import oxygen.http.schema.partial.ResponseSchemaAggregator
import oxygen.meta.k0.*
import oxygen.predef.core.*
import oxygen.schema.*
import zio.*
import zio.http.{Body, Header, Headers, MediaType, ServerSentEvent}
import zio.stream.*

sealed trait ResponseCodecNoStatus[A] {

  val sources: List[ResponseDecodingFailure.Source]
  val schemaAggregator: ResponseSchemaAggregator

  def decode(headers: Headers, body: ReadOnlyCachedHttpBody): ZIO[Scope, ResponseDecodingFailure, A]

  private[ResponseCodecNoStatus] def encodeInternal(value: A, acc: ResponseCodecNoStatus.Builder): ResponseCodecNoStatus.Builder
  final def encode(value: A): (Headers, Body) = encodeInternal(value, ResponseCodecNoStatus.Builder.empty).build

  final def ++[B](that: ResponseCodecNoStatus[B])(using zip: Zip[A, B]): ResponseCodecNoStatus[zip.Out] = ResponseCodecNoStatus.And(this, that, zip)

  final def transform[B](ab: A => B, ba: B => A): ResponseCodecNoStatus[B] = ResponseCodecNoStatus.Transform(this, ab, ba)
  final def transformOrFail[B](ab: A => Either[String, B], ba: B => A): ResponseCodecNoStatus[B] = ResponseCodecNoStatus.TransformOrFail(this, ab, ba)

  inline final def autoTransform[B]: ResponseCodecNoStatus[B] = {
    val (ab, ba) = ProductGeneric.deriveTransform[A, B]
    transform(ab, ba)
  }

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

  def ssePlainBody[A: PlainTextSchema as codec]: ResponseCodecNoStatus[Stream[ResponseDecodingFailure, ServerSentEvent[A]]] =
    ResponseCodecNoStatus.ApplyPartialBodySSE(PartialBodyCodec.ServerSentEvents(codec))
  def sseJsonBody[A: JsonSchema as codec]: ResponseCodecNoStatus[Stream[ResponseDecodingFailure, ServerSentEvent[A]]] =
    ResponseCodecNoStatus.ApplyPartialBodySSE(PartialBodyCodec.ServerSentEvents(codec))
  def sseBody[A](codec: AnySchemaT[A]): ResponseCodecNoStatus[Stream[ResponseDecodingFailure, ServerSentEvent[A]]] =
    ResponseCodecNoStatus.ApplyPartialBodySSE(PartialBodyCodec.ServerSentEvents(codec))

  object header {

    object plain {

      def required[A: PlainTextSchema](name: String, doc: Option[String] = None): ResponseCodecNoStatus[A] =
        ResponseCodecNoStatus.ApplyPartialHeader(PartialParamCodec.Plain.required[A], name, doc)

      def optional[A: PlainTextSchema](name: String, doc: Option[String] = None): ResponseCodecNoStatus[Option[A]] =
        ResponseCodecNoStatus.ApplyPartialHeader(PartialParamCodec.Plain.optional[A], name, doc)

      def manyNonEmpty[A: PlainTextSchema](name: String, doc: Option[String] = None): ResponseCodecNoStatus[NonEmptyList[A]] =
        ResponseCodecNoStatus.ApplyPartialHeader(PartialParamCodec.Plain.manyNonEmpty[A], name, doc)

      def many[S[_]: SeqOps, A: PlainTextSchema](name: String, doc: Option[String] = None): ResponseCodecNoStatus[S[A]] =
        ResponseCodecNoStatus.ApplyPartialHeader(PartialParamCodec.Plain.many[S, A], name, doc)

    }

    object json {

      def required[A: JsonSchema](name: String, doc: Option[String] = None): ResponseCodecNoStatus[A] =
        ResponseCodecNoStatus.ApplyPartialHeader(PartialParamCodec.Json.required[A], name, doc)

      def optional[A: JsonSchema](name: String, doc: Option[String] = None): ResponseCodecNoStatus[Option[A]] =
        ResponseCodecNoStatus.ApplyPartialHeader(PartialParamCodec.Json.optional[A], name, doc)

      def manyNonEmpty[A: JsonSchema](name: String, doc: Option[String] = None): ResponseCodecNoStatus[NonEmptyList[A]] =
        ResponseCodecNoStatus.ApplyPartialHeader(PartialParamCodec.Json.manyNonEmpty[A], name, doc)

      def many[S[_]: SeqOps, A: JsonSchema](name: String, doc: Option[String] = None): ResponseCodecNoStatus[S[A]] =
        ResponseCodecNoStatus.ApplyPartialHeader(PartialParamCodec.Json.many[S, A], name, doc)

    }

  }

  object body {
    def plain[A: PlainTextSchema]: ResponseCodecNoStatus[A] = ResponseCodecNoStatus.ApplyPartialBodySingle(PartialBodyCodec.Plain.fromSchema[A])
    def json[A: JsonSchema]: ResponseCodecNoStatus[A] = ResponseCodecNoStatus.ApplyPartialBodySingle(PartialBodyCodec.Json.fromSchema[A])
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Impls
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  case object DNE extends ResponseCodecNoStatus[Nothing] {
    override val sources: List[ResponseDecodingFailure.Source] = Nil
    override val schemaAggregator: ResponseSchemaAggregator = ResponseSchemaAggregator.empty
    override def decode(headers: Headers, body: ReadOnlyCachedHttpBody): ZIO[Scope, ResponseDecodingFailure, Nothing] = throw new RuntimeException("This is supposed to be DNE!")
    override private[ResponseCodecNoStatus] def encodeInternal(value: Nothing, acc: Builder): Builder = throw new RuntimeException("This is supposed to be DNE!")
  }

  case object Empty extends ResponseCodecNoStatus[Unit] {
    override val sources: List[ResponseDecodingFailure.Source] = Nil
    override val schemaAggregator: ResponseSchemaAggregator = ResponseSchemaAggregator.empty
    override def decode(headers: Headers, body: ReadOnlyCachedHttpBody): ZIO[Scope, ResponseDecodingFailure, Unit] = ZIO.unit
    override private[ResponseCodecNoStatus] def encodeInternal(value: Unit, acc: Builder): Builder = acc
  }

  final case class ApplyPartialHeader[A](partial: PartialParamCodec[A], name: String, doc: Option[String]) extends ResponseCodecNoStatus[A] {

    override val sources: List[ResponseDecodingFailure.Source] = ResponseDecodingFailure.Source.Header(name) :: Nil
    override val schemaAggregator: ResponseSchemaAggregator = ResponseSchemaAggregator.header(ResponseHeaderSchema(name, partial.partialParamSchema.tpe, partial.partialParamSchema.schema, doc))

    override def decode(headers: Headers, body: ReadOnlyCachedHttpBody): ZIO[Scope, ResponseDecodingFailure, A] =
      ZIO.fromEither { partial.decode(headers.rawHeaders(name).toList).leftMap(ResponseDecodingFailure(sources, _)) }

    override private[ResponseCodecNoStatus] def encodeInternal(value: A, acc: Builder): Builder = {
      val encoded = partial.encode(value)
      if encoded.nonEmpty then acc.copy(headers = acc.headers ++ Growable.many(encoded).map(Header.Custom(name, _)))
      else acc
    }

  }

  final case class SetHeader(name: String, value: String) extends ResponseCodecNoStatus[Unit] {
    override val sources: List[ResponseDecodingFailure.Source] = Nil
    override val schemaAggregator: ResponseSchemaAggregator = ResponseSchemaAggregator.empty
    override def decode(headers: Headers, body: ReadOnlyCachedHttpBody): ZIO[Scope, ResponseDecodingFailure, Unit] = ZIO.unit
    override private[ResponseCodecNoStatus] def encodeInternal(value: Unit, acc: Builder): Builder = acc.copy(headers = acc.headers :+ Header.Custom(this.name, this.value))
  }

  final case class SetContentType(contentType: MediaType) extends ResponseCodecNoStatus[Unit] {
    override val sources: List[ResponseDecodingFailure.Source] = Nil
    override val schemaAggregator: ResponseSchemaAggregator = ResponseSchemaAggregator.empty
    override def decode(headers: Headers, body: ReadOnlyCachedHttpBody): ZIO[Scope, ResponseDecodingFailure, Unit] = ZIO.unit
    override private[ResponseCodecNoStatus] def encodeInternal(value: Unit, acc: Builder): Builder = acc.copy(body = acc.body.contentType(contentType))
  }

  sealed trait ApplyPartialBody[A] extends ResponseCodecNoStatus[A]
  object ApplyPartialBody {

    def apply[A](partial: PartialBodyCodec[A]): ApplyPartialBody[A] =
      partial match
        case PartialBodyCodec.Empty                  => ApplyPartialBodyEmpty(PartialBodyCodec.Empty)
        case partial: PartialBodyCodec.Single[A]     => ApplyPartialBodySingle(partial)
        case _: PartialBodyCodec.ServerSentEvents[?] => throw new RuntimeException("Not allowed: ApplyPartialBody.apply(PartialBodyCodec.ServerSentEvents)")

  }

  final case class ApplyPartialBodyEmpty(partial: PartialBodyCodec.Empty.type) extends ApplyPartialBody[Unit] {

    override val sources: List[ResponseDecodingFailure.Source] = ResponseDecodingFailure.Source.Body :: Nil
    override val schemaAggregator: ResponseSchemaAggregator = ResponseSchemaAggregator.body(ResponseBodySchema.Empty)

    override def decode(headers: Headers, body: ReadOnlyCachedHttpBody): ZIO[Scope, ResponseDecodingFailure, Unit] =
      ZIO.unit

    override private[ResponseCodecNoStatus] def encodeInternal(value: Unit, acc: Builder): Builder =
      acc

  }

  final case class ApplyPartialBodySingle[A](partial: PartialBodyCodec.Single[A]) extends ApplyPartialBody[A] {

    override val sources: List[ResponseDecodingFailure.Source] = ResponseDecodingFailure.Source.Body :: Nil
    override val schemaAggregator: ResponseSchemaAggregator = ResponseSchemaAggregator.body(ResponseBodySchema.Single(partial.partialBodySchema.schema))

    override def decode(headers: Headers, body: ReadOnlyCachedHttpBody): ZIO[Scope, ResponseDecodingFailure, A] =
      partial.decode(body).mapError(ResponseDecodingFailure(sources, _))

    override private[ResponseCodecNoStatus] def encodeInternal(value: A, acc: Builder): Builder =
      acc.copy(body = partial.encode(value))

  }

  final case class ApplyPartialBodySSE[A](partial: PartialBodyCodec.ServerSentEvents[A]) extends ApplyPartialBody[Stream[ResponseDecodingFailure, ServerSentEvent[A]]] {

    override val sources: List[ResponseDecodingFailure.Source] = ResponseDecodingFailure.Source.Body :: Nil
    override val schemaAggregator: ResponseSchemaAggregator = ResponseSchemaAggregator.body(ResponseBodySchema.ServerSentEvents(partial.partialBodySchema.schema))

    override def decode(headers: Headers, body: ReadOnlyCachedHttpBody): ZIO[Scope, ResponseDecodingFailure, Stream[ResponseDecodingFailure, ServerSentEvent[A]]] =
      partial.decode(body).mapBoth(ResponseDecodingFailure(sources, _), _.mapError(ResponseDecodingFailure(sources, _)))

    override private[ResponseCodecNoStatus] def encodeInternal(value: Stream[ResponseDecodingFailure, ServerSentEvent[A]], acc: Builder): Builder =
      acc.copy(body = partial.encode(value.orDie))

  }

  final case class And[A, B, C](a: ResponseCodecNoStatus[A], b: ResponseCodecNoStatus[B], zip: Zip.Out[A, B, C]) extends ResponseCodecNoStatus[C] {

    override val sources: List[ResponseDecodingFailure.Source] = a.sources ++ b.sources
    override val schemaAggregator: ResponseSchemaAggregator = a.schemaAggregator >>> b.schemaAggregator

    override def decode(headers: Headers, body: ReadOnlyCachedHttpBody): ZIO[Scope, ResponseDecodingFailure, C] =
      for {
        aValue <- a.decode(headers, body)
        bValue <- b.decode(headers, body)
      } yield zip.zip(aValue, bValue)

    override private[ResponseCodecNoStatus] def encodeInternal(value: C, acc: Builder): Builder = {
      val (aValue, bValue) = zip.unzip(value)
      b.encodeInternal(bValue, a.encodeInternal(aValue, acc))
    }

  }

  final case class Transform[A, B](a: ResponseCodecNoStatus[A], ab: A => B, ba: B => A) extends ResponseCodecNoStatus[B] {

    override val sources: List[ResponseDecodingFailure.Source] = a.sources
    override val schemaAggregator: ResponseSchemaAggregator = a.schemaAggregator

    override def decode(headers: Headers, body: ReadOnlyCachedHttpBody): ZIO[Scope, ResponseDecodingFailure, B] =
      a.decode(headers, body).map(ab)

    override private[ResponseCodecNoStatus] def encodeInternal(value: B, acc: Builder): Builder =
      a.encodeInternal(ba(value), acc)

  }

  final case class TransformOrFail[A, B](a: ResponseCodecNoStatus[A], ab: A => Either[String, B], ba: B => A) extends ResponseCodecNoStatus[B] {

    override val sources: List[ResponseDecodingFailure.Source] = a.sources
    override val schemaAggregator: ResponseSchemaAggregator = a.schemaAggregator

    override def decode(headers: Headers, body: ReadOnlyCachedHttpBody): ZIO[Scope, ResponseDecodingFailure, B] =
      a.decode(headers, body).flatMap { aValue =>
        ZIO.fromEither { ab(aValue).leftMap { error => ResponseDecodingFailure(sources, DecodingFailureCause.DecodeError(error, DecodingFailureCause.DecodeInput.PreviousValue(aValue.toString))) } }
      }

    override private[ResponseCodecNoStatus] def encodeInternal(value: B, acc: Builder): Builder =
      a.encodeInternal(ba(value), acc)

  }

}
